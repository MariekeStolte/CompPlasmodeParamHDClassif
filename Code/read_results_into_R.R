setwd("../Results")

library(Rankcluster)

################################################################################
##                          DATA CLEANING FUNCTIONS                           ##
##                                                                            ##
################################################################################

changeLabels <- function(results) {
  cleanClassifierNames <- function(cl, pen) {
    cl <- gsub("LearnerClassif", "", cl)
    cl <- gsub("\\.tuned", "", cl)
    cl <- gsub("classif\\.", "", cl)
    cl <- gsub("_", "", cl, fixed = TRUE)
    cl <- toupper(cl)
    cl[cl == "CVGLMNET" & pen == "1"] <- "LASSO"
    cl[cl == "CVGLMNET" & pen == "0"] <- "Ridge"
    cl[cl == "KKNN"] <- "KNN"
    cl[cl == "LOGREGMODEL"] <- "Logistic Model"
    cl[cl == "RANGER"] <- "Random Forest"
    return(cl)
  }
  results$learner_id <- cleanClassifierNames(results$learner_id, results$alpha.classif)
  results$ogm <- cleanClassifierNames(results$ogm, results$alpha.ogm)
  results <- results[!colnames(results) %in% c("alpha.classif", "alpha.ogm")]
  colnames(results)[colnames(results) == "learner_id"] <- "classifier"
  results$classifier <- factor(results$classifier, 
                               levels = c("Ridge", "LASSO", "SVM", 
                                          "KNN", "Random Forest"))
  results$ogm <- factor(results$ogm, 
                        levels = c("Ridge", "LASSO", "SVM", 
                                   "KNN", "Random Forest", "Logistic Model"))
  colnames(results) <- gsub("classif\\.", "", colnames(results))
  colnames(results) <- gsub("my\\_", "", colnames(results))
  colnames(results)[colnames(results) == "acc"] <- "Accuracy"
  colnames(results)[colnames(results) == "auc"] <- "AUC"
  colnames(results)[colnames(results) == "bbrier"] <- "Brier.Score"
  colnames(results)[colnames(results) %in% c("fbeta", "F1")] <- "F1.Score"
  colnames(results)[colnames(results) == "sensitivity"] <- "Sensitivity"
  colnames(results)[colnames(results) == "specificity"] <- "Specificity"
  return(results)
}

levenshteinDist <- function(rank1, rank2) {
  a <- intToUtf8(rank1 + 100)
  b <- intToUtf8(rank2 + 100)
  return(as.numeric(adist(a, b)))
}

cleanData <- function(res) {
  res <- changeLabels(res)
  # Label scenarios
  res$scenario <- "True Scenario"
  res$scenario[res$shift != 0] <- paste0("Shift: ", res$shift[res$shift != 0])
  res$scenario[res$scale != 1] <- paste0("Scale: ", round(res$scale[res$scale != 1], 2))
  res$scenario[!res$cor %in% c("true", "sn")] <- paste0("Correlation: ", res$cor[!res$cor %in% c("true", "sn")])
  res$scenario[res$cor == "sn"] <- "Distribution: Standard Normal"
  res$scenario[res$factor.logit != 1] <- paste0("OGM scaled by: ", res$factor.logit[res$factor.logit != 1])
  table(res$scenario, useNA = "al")
  
  truth <- res[res$scenario == "True Scenario" & res$type == "parametric", ]
  
  # split simulations for true scenario: first ones are part of "deviations", 
  # rest is used to calculate true values
  split.truth <- split(truth, list(truth$ogm, truth$type, 
                                   ifelse(is.na(truth$replace), "p", truth$replace), 
                                   ifelse(is.na(truth$prop), "p", truth$prop)), drop = TRUE)
  sim <- do.call(rbind, lapply(split.truth, function(x) x[1:500, ]))
  truth <- lapply(split.truth, function(x) x[-(1:500), ])
  
  res <- rbind(res[!(res$scenario == "True Scenario" & res$type == "parametric"), ], 
               sim)
  
  # Calculate truth
  true.measures <- lapply(truth, function(x) {
    mean.acc <- tapply(x$Accuracy, x$classifier, mean, na.rm = TRUE)
    mean.auc <- tapply(x$AUC, x$classifier, mean, na.rm = TRUE)
    mean.brier <- tapply(x$Brier.Score, x$classifier, mean, na.rm = TRUE)
    mean.f1 <- tapply(x$F1.Score, x$classifier, mean, na.rm = TRUE)
    mean.sens <- tapply(x$Sensitivity, x$classifier, mean, na.rm = TRUE)
    mean.spec <- tapply(x$Specificity, x$classifier, mean, na.rm = TRUE)
    mean.warn <- tapply(x$warnings, x$classifier, mean, na.rm = TRUE)
    mean.err <- tapply(x$errors, x$classifier, mean, na.rm = TRUE)
    data.frame(classifier = factor(names(mean.acc), 
                                   levels = c("Ridge", "LASSO", "SVM", 
                                              "KNN", "Random Forest", "Logistic Model")), 
               True.Accuracy = mean.acc, True.AUC = mean.auc, True.Brier.Score = mean.brier,
               True.F1.Score = mean.f1, True.Sensitivity = mean.sens, True.Specificity = mean.spec, 
               True.Rank.Accuracy = rank(-mean.acc), True.Rank.AUC = rank(-mean.auc), 
               True.Rank.Brier.Score = rank(mean.brier), True.Rank.F1.Score = rank(-mean.f1),
               True.Rank.Sensitivity = rank(-mean.sens), True.Rank.Specificity = rank(-mean.spec),
               n = x$n[1], p = x$p[1], ogm = x$ogm[1])
  })
  true.measures <- do.call(rbind, true.measures)
  
  # Calculate method ranking per simulation iteration
  splitted.res <- split(res, list(res$scenario, res$ogm, res$type, 
                                     ifelse(is.na(res$replace), "p", res$replace), 
                                     ifelse(is.na(res$prop), "p", res$prop), res$iter), 
                        drop = TRUE)
  table(sapply(splitted.res, nrow))
  splitted.res <- lapply(splitted.res, function(x){
    x$Rank.Accuracy <- rank(-x$Accuracy, ties.method = "random")
    x$Rank.AUC <- rank(-x$AUC, ties.method = "random")
    x$Rank.Brier.Score <- rank(x$Brier.Score, ties.method = "random")
    x$Rank.F1.Score <- rank(-x$F1.Score, ties.method = "random")
    x$Rank.Sensitivity <- rank(-x$Sensitivity, ties.method = "random")
    x$Rank.Specificity <- rank(-x$Specificity, ties.method = "random")
    x
  })
  tmp <- do.call(rbind, splitted.res)
  if(nrow(tmp) != nrow(res)) stop("Some data got lost during calculation of ranks")
  res <- tmp
  tmp <- merge(res, true.measures, by = c("classifier", "ogm", "n", "p"))
  if(nrow(tmp) != nrow(res)) stop("Some data got lost during merging of results with true measures")
  res <- tmp
  
  # Calculate absolute and relative errors per measure
  res$Error.Accuracy <- res$Accuracy - res$True.Accuracy
  res$Error.AUC <- res$AUC - res$True.AUC
  res$Error.Brier.Score <- res$Brier.Score - res$True.Brier.Score
  res$Error.F1.Score <- res$F1.Score - res$True.F1.Score
  res$Error.Sensitivity <- res$Sensitivity - res$True.Sensitivity
  res$Error.Specificity <- res$Specificity - res$True.Specificity
  
  res$Relative.Error.Accuracy <- (-res$Error.Accuracy) / (1 - res$True.Accuracy)
  res$Relative.Error.AUC <- (-res$Error.AUC) / (1 - res$True.AUC)
  res$Relative.Error.Brier.Score <- res$Error.Brier.Score / res$True.Brier.Score
  res$Relative.Error.F1.Score <- (-res$Error.F1.Score) / (1 - res$True.F1.Score)
  res$Relative.Error.Sensitivity <- (-res$Error.Sensitivity) / (1 - res$True.Sensitivity)
  res$Relative.Error.Specificity <- (-res$Error.Specificity) / (1 - res$True.Specificity)
  
  # Calculate Kendall distance of true and simulation rankings per scenario
  splitted.res <- split(res, list(res$scenario, res$ogm, res$type, 
                                     ifelse(is.na(res$replace), "p", res$replace), 
                                     ifelse(is.na(res$prop), "p", res$prop), res$iter), 
                        drop = TRUE)
  table(sapply(splitted.res, nrow))
  splitted.res <- lapply(splitted.res, function(x){
    x$Kendall.Distance.Accuracy <- Rankcluster:::distKendall_ranking(x$Rank.Accuracy, x$True.Rank.Accuracy) / 10
    x$Kendall.Distance.AUC <- Rankcluster:::distKendall_ranking(x$Rank.AUC, x$True.Rank.AUC) / 10
    x$Kendall.Distance.Brier.Score <- Rankcluster:::distKendall_ranking(x$Rank.Brier.Score, x$True.Rank.Brier.Score) / 10
    x$Kendall.Distance.F1.Score <- Rankcluster:::distKendall_ranking(x$Rank.F1.Score, x$True.Rank.F1.Score) / 10
    x$Kendall.Distance.Sensitivity <- Rankcluster:::distKendall_ranking(x$Rank.Sensitivity, x$True.Rank.Sensitivity) / 10
    x$Kendall.Distance.Specificity <- Rankcluster:::distKendall_ranking(x$Rank.Specificity, x$True.Rank.Specificity) / 10
    x
  })
  
  tmp <- do.call(rbind, splitted.res)
  if(nrow(tmp) != nrow(res)) stop("Some data got lost during calculation of Kendall distances")
  res <- tmp
  
  # add plasmode type
  res$type.plasmode <- ifelse(res$replace, "Bootstrap", "Subsampling")
  
  return(res)
}

################################################################################
##                            CLEAN RESULTS p=2                               ##
##                                                                            ##
################################################################################
data.2 <- c("results_p=2_n=100_2024_07_24.RData", "results_p=2_n=100_add_2024_07_24.RData", 
            "results_p=2_n=100_add2_2024_07_30.RData")
for(i in seq(along = data.2))
  load(data.2[i], verbose = TRUE)
res.2 <- rbind(res.2, res.2.add, res.2.add2)

summary(res.2)

set.seed(0705)
res.2.clean <- cleanData(res = res.2)

summary(res.2.clean)

table(res.2.clean$scenario)

save(res.2.clean, data.2, file = "results_p=2_n=100_preprocessed_2024_07_30.RData")


################################################################################
##                            CLEAN RESULTS p=10                              ##
##                                                                            ##
################################################################################
data.10 <- c("results_p=10_n=100_2024_07_09.RData", "results_p=10_n=100_add_2024_07_24.RData", 
             "results_p=10_n=100_add2_2024_07_30.RData")
for(i in seq(along = data.10))
  load(data.10[i], verbose = TRUE)

res.10 <- rbind(res.10, res.10.add, res.10.add2)

summary(res.10)

set.seed(0705)
res.10.clean <- cleanData(res = res.10)

summary(res.10.clean)

table(res.10.clean$scenario)

save(res.10.clean, data.10, file = "results_p=10_n=100_preprocessed_2024_07_30.RData")

################################################################################
##                            CLEAN RESULTS p=50                              ##
##                                                                            ##
################################################################################
data.50 <- c("results_p=50_n=100_2024_07_09.RData", "results_p=50_n=100_add_2024_07_24.RData", 
             "results_p=50_n=100_add2_2024_07_30.RData")
for(i in seq(along = data.50))
  load(data.50[i], verbose = TRUE)

res.50 <- rbind(res.50, res.50.add, res.50.add2)
summary(res.50)

set.seed(0705)
res.50.clean <- cleanData(res = res.50)

summary(res.50.clean)

table(res.50.clean$scenario)

save(res.50.clean, data.50, file = "results_p=50_n=100_preprocessed_2024_07_30.RData")


################################################################################
##                            CLEAN RESULTS p=150                             ##
##                                                                            ##
################################################################################
data.150 <- c("results_p=150_n=100_2024_07_09.RData", "results_p=150_n=100_add_2024_07_24.RData", 
              "results_p=150_n=100_add2_2024_07_30.RData")
for(i in seq(along = data.150))
  load(data.150[i], verbose = TRUE)

res.150 <- rbind(res.150, res.150.add, res.150.add2)
summary(res.150)

set.seed(0705)
res.150.clean <- cleanData(res = res.150)

summary(res.150.clean)

table(res.150.clean$scenario)

save(res.150.clean, data.150, file = "results_p=150_n=100_preprocessed_2024_07_30.RData")
