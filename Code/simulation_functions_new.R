library(mlr3verse)
library(bigsimr)
library(batchtools)

mySpecificity <- R6::R6Class("mySpecificity",
                             inherit = mlr3::MeasureClassif,
                             public = list(
                               initialize = function() { # initialize class
                                 super$initialize(
                                   id = "classif.my_specificity", # unique ID
                                   packages = character(), # no package dependencies
                                   properties = character(), # no special properties
                                   predict_type = "response", # measures response prediction
                                   range = c(0, 1), # results in values between (0, 1)
                                   minimize = FALSE, # larger values are better
                                   average = "macro", 
                                   aggregator = function(x) {
                                     mean(x, na.rm = TRUE)
                                   }
                                 )
                               }
                             ),
                             
                             private = list(
                               # define score as private method
                               .score = function(prediction, ...) {
                                 mlr3measures::specificity(prediction$truth, prediction$response, 
                                                           positive = "1")
                               }
                             )
)
mlr3::mlr_measures$add("classif.my_specificity", mySpecificity)

# TODO: Falls in Fold keine 0en in Testdatensatz --> Mittle für Sensitivity nur über restliche Folds
mySensitivity <- R6::R6Class("mySensitivity",
                             inherit = mlr3::MeasureClassif,
                             public = list(
                               initialize = function() { # initialize class
                                 super$initialize(
                                   id = "classif.my_sensitivity", # unique ID
                                   packages = character(), # no package dependencies
                                   properties = character(), # no special properties
                                   predict_type = "response", # measures response prediction
                                   range = c(0, 1), # results in values between (0, 1)
                                   minimize = FALSE, # larger values are better
                                   average = "macro", 
                                   aggregator = function(x) {
                                     # mean only over folds where there was at least one 
                                     # true zero
                                     mean(x, na.rm = TRUE) 
                                   }
                                 )
                               }
                             ),
                             
                             private = list(
                               # define score as private method
                               .score = function(prediction, ...) {
                                 mlr3measures::sensitivity(prediction$truth, prediction$response, positive = "1")
                               }
                             )
)
mlr3::mlr_measures$add("classif.my_sensitivity", mySensitivity)

myF1 <- R6::R6Class("myF1",
                    inherit = mlr3::MeasureClassif,
                    public = list(
                      initialize = function() { # initialize class
                        super$initialize(
                          id = "classif.my_F1", # unique ID
                          packages = character(), # no package dependencies
                          properties = character(), # no special properties
                          predict_type = "response", # measures response prediction
                          range = c(0, 1), # results in values between (0, 1)
                          minimize = FALSE, # larger values are better
                          average = "macro", 
                          aggregator = function(x) {
                            # mean only over folds where there was at least one 
                            # true one
                            mean(x, na.rm = TRUE)
                          }
                        )
                      }
                    ),
                    
                    private = list(
                      # define score as private method
                      .score = function(prediction, ...) {
                        # define loss
                        my_f1 = function(truth, response) {
                          truth <- relevel(truth, "1")
                          response <- relevel(response, "1")
                          cm <- table(response, truth)
                          nom <- 2 * cm[1L, 1L]
                          f1 <- nom / (nom + cm[2L, 1L] + cm[1L, 2L])
                          # set F1 score to zero if there are no predicted ones
                          if(sum(cm[1, ]) == 0) 0 else f1
                        }
                        # call loss function
                        my_f1(prediction$truth, prediction$response)
                      }
                    )
)
mlr3::mlr_measures$add("classif.my_F1", myF1)

myAUC <- R6::R6Class("myAUC",
                     inherit = mlr3::MeasureClassif,
                     public = list(
                       initialize = function() { # initialize class
                         super$initialize(
                           id = "classif.my_auc", # unique ID
                           packages = character(), # no package dependencies
                           properties = character(), # no special properties
                           predict_type = "response", # measures response prediction
                           range = c(0, 1), # results in values between (0, 1)
                           minimize = FALSE, # larger values are better
                           average = "macro", 
                           aggregator = function(x) {
                             # mean only over folds where there was at least one 
                             # true one
                             mean(x, na.rm = TRUE)
                           }
                         )
                       }
                     ),
                     
                     private = list(
                       # define score as private method
                       .score = function(prediction, ...) {
                         # call loss function
                         mlr3measures::auc(prediction$truth, prediction$prob[, "1"], 
                                           positive = "1", na_value = NaN)
                       }
                     )
)
mlr3::mlr_measures$add("classif.my_auc", myAUC)

# disable julia installation in bigsimr setup
bigsimr_setup <- function (pkg_check = TRUE, ...){
  julia <- JuliaCall::julia_setup(installJulia = FALSE, ...)
  if (pkg_check) {
    JuliaCall::julia_install_package_if_needed("Bigsimr")
  }
  JuliaCall::julia_library("Bigsimr")
  functions <- JuliaCall::julia_eval("filter(isascii, string.(propertynames(Bigsimr)))")
  rm_funcs <- c("Bigsimr", "NearestCorrelationMatrix", "PearsonCorrelationMatch", 
                "CorType", "Pearson", "Spearman", "Kendall")
  functions <- functions[!(functions %in% rm_funcs)]
  bs <- JuliaCall::julia_pkg_import("Bigsimr", functions)
  bs$Pearson <- JuliaCall::julia_eval("Pearson")
  bs$Spearman <- JuliaCall::julia_eval("Spearman")
  bs$Kendall <- JuliaCall::julia_eval("Kendall")
  bs
}
bs <- bigsimr_setup()
dist <- bigsimr::distributions_setup()

predict.LogRegModel <- function(mod, newdata, ...) {
  eta <- mod$intercept + as.matrix(newdata) %*% mod$slopes
  pred.prob <- 1 / (1 + exp(-eta))
  pred.prob <- cbind(pred.prob, 1-pred.prob)
  colnames(pred.prob) <- c("1", "0")
  return(pred.prob)
}

modifyProb <- function(pred.prob, factor.logit) {
  new.logit <- log(pred.prob / (1 - pred.prob)) * factor.logit
  new.prob <- exp(new.logit) / (1 + exp(new.logit))
  new.prob[pred.prob > 1 - 1e-8 & !is.finite(new.prob)] <- 1
  new.prob[pred.prob < 1e-8 & !is.finite(new.prob)] <- 0
  return(pmax(pmin(new.prob, 1), 0))
}

generateResp <- function(Xnew, ogm, factor.logit) {
  pred.prob <- predict(ogm, Xnew, predict_type = "prob")[, "1"]
  pred.prob <- modifyProb(pred.prob, factor.logit)
  y.new <- rbinom(length(pred.prob), 1, pred.prob)
  return(y.new)
}

normToZeroOne <- function(x) {
  mn <- min(x)
  mx <- max(x)
  if(abs(mn - mx) < 1e-8) return(rep(0, length(x)))
  return((x - mn)/(mx - mn))
}

generateCovariatesParam <- function(n, adjusted.corr, margins, norm, shift, scale, 
                                    shift.group = 0) {
  X <- bs$rvec(n, adjusted.corr, margins)
  X[1:floor(n/2), ] <- X[1:floor(n/2), ] + shift.group
  X[(floor(n/2) + 1):n, ] <- X[(floor(n/2) + 1):n, ] - shift.group
  if(norm) X <- apply(X, 2, normToZeroOne)
  X <- scale * X + shift
  colnames(X) <- paste0("X", 1:ncol(X))
  return(as.data.frame(X))
}

generateCovariatesPlasm <- function(X, prop, replace) {
  XP <- X[sample(nrow(X), round(nrow(X) * prop), replace = replace), ]
  return(XP)
}

evaluateModels <- function(X, y, mdls, msrs, resamp = rsmp("cv", folds = 5)) {
  y <- factor(y, levels = 0:1)
  task <- as_task_classif(data.frame(X, y), target = "y", positive = "1")
  task$set_col_roles("y", roles = c("target", "stratum"))
  grid <- benchmark_grid(
    task = task,
    learner = mdls,
    resampling = resamp
  )
  bm <- benchmark(grid)
  res <- bm$aggregate(measures = msrs(msrs), conditions = TRUE, params = TRUE)
  res$alpha.classif <- sapply(res$params, function(p) if(!is.null(p$alpha)) p$alpha else NA)
  res <- as.data.frame(res)[, c("learner_id", "alpha.classif", msrs, "warnings", "errors")]
  return(res)
}

oneIterSimParam <- function(n, adjusted.corr, margins, norm, shift, scale, shift.group,
                            factor.logit, ogm, mdls, msrs, resamp = rsmp("cv", folds = 5)) {
  if(missing(shift.group)) shift.group <- 0
  i <- 0
  while(i <= 50){
    X <- generateCovariatesParam(n, adjusted.corr, margins, norm, shift, scale, shift.group)
    y <- generateResp(X, ogm, factor.logit)
    i <- i + 1
    if(!sum(y) %in% c(0, length(y))) break
  }
  if(i == 50) warning("Maximum number of retry iterations for data generation was reached.")
  res <- evaluateModels(X, y, mdls, msrs, resamp)
  res[, "no.retry.sample"] <- i
  res[, "prop.ones"] <- mean(y == 1)
  return(res)
}

oneIterSimPlasm <- function(X, prop, replace, factor.logit, ogm, mdls, msrs, 
                            resamp = rsmp("cv", folds = 5)) {
  flag <- TRUE
  i <- 0
  while(flag & i <= 50){
    Xnew <- generateCovariatesPlasm(X, prop, replace)
    y <- generateResp(Xnew, ogm, factor.logit)
    i <- i + 1
    if(!sum(y) %in% c(0, length(y))) flag <- FALSE
  }
  if(i == 50) warning("Maximum number of retry iterations for data generation was reached.")
  res <- evaluateModels(Xnew, y, mdls, msrs, resamp)
  res[, "no.retry.sample"] <- i
  res[, "prop.ones"] <- mean(y == 1)
  return(res)
}

oneSimStudy <- function(n.iter, dgp, factor.logit, ogm, mdls, msrs, 
                        resamp = rsmp("cv", folds = 5)) {
  if(is.null(dgp$shift.group)) dgp$shift.group <- 0
  dgp$margins <- eval(parse(text = dgp$margins))
  if(dgp$sim.type == "parametric") {
    res <- lapply(1:n.iter, function(i) {
      r <- oneIterSimParam(dgp$n, dgp$adjusted.corr, dgp$margins, 
                           dgp$norm, dgp$shift, dgp$scale, dgp$shift.group,
                           factor.logit, ogm, mdls, msrs, resamp)
      r$iter <- i
      return(r)
    })
    res <- do.call(rbind, res)
    res$type <- "parametric"
    res$prop <- NA
    res$replace <- NA
    res$shift <- dgp$shift
    res$scale <- dgp$scale
    res$cor <- dgp$cor.par
  } else if(dgp$sim.type == "Plasmode") {
    X <- generateCovariatesParam(dgp$n, dgp$adjusted.corr, dgp$margins, dgp$norm, 
                                 0, 1, dgp$shift.group)
    res <- lapply(1:n.iter, function(i) {
      r <- oneIterSimPlasm(X, dgp$prop, dgp$replace, factor.logit, ogm, mdls, 
                           msrs, resamp)
      r$iter <- i
      return(r)
    })
    res <- do.call(rbind, res)
    res$type <- "Plasmode"
    res$prop <- dgp$prop
    res$replace <- dgp$replace
    res$shift <- 0
    res$scale <- 1
    res$cor <- "true"
  } else {
    error("dgp$sim.type must be either \"parametric\" or \"Plasmode\"")
  }
  res$n <- dgp$n
  res$p <- nrow(dgp$adjusted.corr)
  res$factor.logit <- factor.logit
  res$ogm <- if("LogRegModel" %in% class(ogm)) class(ogm) else class(ogm$base_learner())[1]
  res$alpha.ogm <- if(!is.null(ogm$param_set$values$alpha)) ogm$param_set$values$alpha else NA
  return(res)
}

allSimStudies <- function(n.iter, dgp, factor.logit, ogm, mdls, msrs, resamp = rsmp("cv", folds = 5), 
                          registry, run.times, max.time, mem) {
  ids <- batchMap(oneSimStudy, n.iter = n.iter, dgp = dgp, factor.logit = factor.logit, 
                  ogm = ogm, more.args = list(mdls = mdls, msrs = msrs, resamp = resamp))
  ids[, chunk := binpack(run.times, max.time)]
  submitJobs(ids = ids, reg = registry, resources = list(walltime = max.time, memory = mem))
  if(waitForJobs()) {
    res <- reduceResultsList(reg = registry)
    res.df <- try(do.call(rbind, res))
    if(inherits(res.df, "try-error")) return(res)
    else return(res.df)
  } else {
    print("Errors:\n")
    print(getErrorMessages())
    res <- reduceResultsList(reg = registry)
    res.df <- try(do.call(rbind, res))
    if(inherits(res.df, "try-error")) return(res)
    else return(res.df)
  }
}
