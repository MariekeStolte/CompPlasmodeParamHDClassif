setwd("../Results")
library(xtable)

tablesTrueMeasures <- function(res) {
  if("factor.logit" %in% colnames(res)) {
    res$ogm <- ifelse(res$factor.logit == 1, as.character(res$ogm), 
                      paste0(res$ogm, " * ", res$factor.logit))
  }
  true.values <- res
  true.values <- true.values[, c("ogm", "classifier", "n", "p",
                                 grep("True", colnames(true.values), value = TRUE))]
  true.values <- unique(true.values)
  true.values <- split(true.values, true.values$ogm, drop = TRUE)
  tables <- lapply(true.values, function(x) {
    tmp <- t(x[, -(1:4)])
    colnames(tmp) <- x$classifier
    tmp <- tmp[, c("Ridge", "LASSO", "SVM", "KNN", "Random Forest")]
    rownames(tmp) <- gsub("\\.", " ", rownames(tmp))
    xtable(tmp, caption = paste0("True values for performance measures for the five ",
                                 "classifiers for true OGM ", x$ogm[1], ", $p = ", 
                                 x$p[1], "$, and $n = ", x$n[1], "$."), 
           digits = 4)
  })
  print(tables)
}
################################################################################
load("../Results/results_p=2_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
load("../Results/results_p=10_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
load("../Results/results_p=50_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
load("../Results/results_p=150_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
tablesTrueMeasures(res.2.clean)
tablesTrueMeasures(res.10.clean)
tablesTrueMeasures(res.50.clean)
tablesTrueMeasures(res.150.clean)


