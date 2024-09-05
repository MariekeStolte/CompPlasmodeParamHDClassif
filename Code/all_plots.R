source("plotting_functions.R")
source("acceptable_simulations.R")

library(xtable)

setwd("../Results")
load("results_p=2_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
load("results_p=10_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
load("results_p=50_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
load("results_p=150_n=100_preprocessed_2024_07_30.RData", verbose = TRUE)
################################################################################
##                          PLOT RESULTS FOR p = 2                            ##
##                                                                            ##
################################################################################
# Define all combinations of deviation, measure and true ogm
deviation.type <- c("Correlation", "Distribution", "OGM", "Scale", "Shift")
measure <- c("Accuracy", "AUC", "Brier.Score", "F1.Score", "Sensitivity", "Specificity")
combs <- expand.grid(measure, deviation.type)
colnames(combs) <- c("measure", "deviation.type")
lvls <- lapply(1:nrow(combs), function(i) character(0))
lvls[combs$deviation.type == "Correlation"] <- rep(list(c("True Scenario", unique(grep("Correlation", res.2.clean$scenario, value = TRUE)))[c(1, 3:2, 4:6)]), 
                                                   sum(combs$deviation.type == "Correlation"))
lvls[combs$deviation.type == "Distribution"] <- rep(list(c("True Scenario", unique(grep("Distribution", res.2.clean$scenario, value = TRUE)))), 
                                                    sum(combs$deviation.type == "Distribution"))    
lvls[combs$deviation.type == "OGM"] <- rep(list(c("True Scenario", unique(grep("OGM", res.2.clean$scenario, value = TRUE)))[c(3, 1, 4, 2)]),
                                           sum(combs$deviation.type == "OGM")) 

lvls[combs$deviation.type == "Shift"] <- rep(list(c("True Scenario", unique(grep("Shift", res.2.clean$scenario, value = TRUE)))[c(4:1, 5:7)]), 
                                             sum(combs$deviation.type == "Shift")) 
lvls[combs$deviation.type == "Scale"] <- rep(list(c("True Scenario", unique(grep("Scale", res.2.clean$scenario, value = TRUE)))[c(2:4, 1, 5:7)]), 
                                             sum(combs$deviation.type == "Scale")) 
any(sapply(lvls, is.null))

tapply(res.2.clean$Accuracy, list(res.2.clean$scenario, res.2.clean$classifier), function(x) mean(is.na(x)))
tapply(res.2.clean$AUC, list(res.2.clean$scenario, res.2.clean$classifier), function(x) mean(is.na(x)))
tapply(res.2.clean$Brier.Score, list(res.2.clean$scenario, res.2.clean$classifier), function(x) mean(is.na(x)))
tapply(res.2.clean$F1.Score, list(res.2.clean$scenario, res.2.clean$classifier), function(x) mean(is.na(x)))
tapply(res.2.clean$Sensitivity, list(res.2.clean$scenario, res.2.clean$classifier), function(x) mean(is.na(x)))
tapply(res.2.clean$Specificity, list(res.2.clean$scenario, res.2.clean$classifier), function(x) mean(is.na(x)))

tapply(res.2.clean$errors, list(res.2.clean$scenario, res.2.clean$classifier), function(x) mean(x > 0))
tab <- tapply(res.2.clean$errors, list(res.2.clean$scenario, res.2.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:error.2", 
             caption = "Number of iterations with error messages per scenario and classifier for $p = 2$. Scenarios with no error messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")
tab <- tapply(res.2.clean$warnings, list(res.2.clean$scenario, res.2.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:warning.2", 
             caption = "Number of iterations with warning messages per scenario and classifier for $p = 2$. Scenarios with no warning messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")

# plot everything
pdf("plots_error_results_p=2_n=100_2024_07_30.pdf", height = 10, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotError(res = res.2.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                  lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()

pdf("plots_order_Kendall_results_p=2_n=100_2024_07_30.pdf", height = 8, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotKendall(res = res.2.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                    lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()

pdf("plots_comparison_simulation_types_p=2_n=100_2024_07_30.pdf", height = 10, width = 12)
# combs.tmp <- unique(combs[, c("measure", "ogm")])
for(m in measure) {
  # print(i)
  print(plotCompPlasmode(res = res.2.clean, measure = m, 
                         ogm = "Logistic Model"))
}
dev.off()


################################################################################
##                          PLOT RESULTS FOR p = 10                           ##
##                                                                            ##
################################################################################
# Define all combinations of deviation, measure and true ogm
deviation.type <- c("Correlation", "Distribution", "OGM", "Scale", "Shift")
measure <- c("Accuracy", "AUC", "Brier.Score", "F1.Score", "Sensitivity", "Specificity")
# ogm <- levels(droplevels(res.50.clean$ogm))
combs <- expand.grid(measure, deviation.type)
colnames(combs) <- c("measure", "deviation.type")
lvls <- lapply(1:nrow(combs), function(i) character(0))
lvls[combs$deviation.type == "Correlation"] <- rep(list(c("True Scenario", unique(grep("Correlation", res.10.clean$scenario, value = TRUE)))[c(1, 3:2, 4:6)]), 
                                                   sum(combs$deviation.type == "Correlation"))
lvls[combs$deviation.type == "Distribution"] <- rep(list(c("True Scenario", unique(grep("Distribution", res.10.clean$scenario, value = TRUE)))), 
                                                    sum(combs$deviation.type == "Distribution"))    
lvls[combs$deviation.type == "OGM"] <- rep(list(c("True Scenario", unique(grep("OGM", res.10.clean$scenario, value = TRUE)))[c(3, 1, 4, 2)]),
                                           sum(combs$deviation.type == "OGM")) 

lvls[combs$deviation.type == "Shift"] <- rep(list(c("True Scenario", unique(grep("Shift", res.10.clean$scenario, value = TRUE)))[c(4:1, 5:7)]), 
                                             sum(combs$deviation.type == "Shift")) 
lvls[combs$deviation.type == "Scale"] <- rep(list(c("True Scenario", unique(grep("Scale", res.10.clean$scenario, value = TRUE)))[c(2:4, 1, 5:7)]), 
                                             sum(combs$deviation.type == "Scale")) 
any(sapply(lvls, is.null))

tapply(res.10.clean$Accuracy, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(is.na(x)))
tapply(res.10.clean$AUC, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(is.na(x)))
tapply(res.10.clean$Brier.Score, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(is.na(x)))
tapply(res.10.clean$F1.Score, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(is.na(x)))
tapply(res.10.clean$Sensitivity, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(is.na(x)))
tapply(res.10.clean$Specificity, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(is.na(x)))

tapply(res.10.clean$errors, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(x > 0))
tapply(res.10.clean$warnings, list(res.10.clean$scenario, res.10.clean$classifier), function(x) mean(x > 0))

tab <- tapply(res.10.clean$errors, list(res.10.clean$scenario, res.10.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:error.10", 
             caption = "Number of iterations with error messages per scenario and classifier for $p = 10$. Scenarios with no error messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")
tab <- tapply(res.10.clean$warnings, list(res.10.clean$scenario, res.10.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:warning.10", 
             caption = "Number of iterations with warning messages per scenario and classifier for $p = 10$. Scenarios with no warning messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")

# plot everything
pdf("plots_error_results_p=10_n=100_2024_07_30.pdf", height = 10, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotError(res = res.10.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                  lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()

pdf("plots_order_Kendall_results_p=10_n=100_2024_07_30.pdf", height = 8, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotKendall(res = res.10.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                    lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()


pdf("plots_comparison_simulation_types_p=10_n=100_2024_07_30.pdf", height = 10, width = 12)
for(m in measure) {
  print(plotCompPlasmode(res = res.10.clean, measure = m, 
                         ogm = "Logistic Model"))
}
dev.off()

################################################################################
##                          PLOT RESULTS FOR p = 50                           ##
##                                                                            ##
################################################################################
# Define all combinations of deviation, measure and true ogm
deviation.type <- c("Correlation", "Distribution", "OGM", "Scale", "Shift")
measure <- c("Accuracy", "AUC", "Brier.Score", "F1.Score", "Sensitivity", "Specificity")
# ogm <- levels(droplevels(res.50.clean$ogm))
combs <- expand.grid(measure, deviation.type)
colnames(combs) <- c("measure", "deviation.type")
lvls <- lapply(1:nrow(combs), function(i) character(0))
lvls[combs$deviation.type == "Correlation"] <- rep(list(c("True Scenario", unique(grep("Correlation", res.50.clean$scenario, value = TRUE)))[c(1, 3:2, 4:6)]), 
                                                   sum(combs$deviation.type == "Correlation"))
lvls[combs$deviation.type == "Distribution"] <- rep(list(c("True Scenario", unique(grep("Distribution", res.50.clean$scenario, value = TRUE)))), 
                                                    sum(combs$deviation.type == "Distribution"))    
lvls[combs$deviation.type == "OGM"] <- rep(list(c("True Scenario", unique(grep("OGM", res.50.clean$scenario, value = TRUE)))[c(3, 1, 4, 2)]),
                                           sum(combs$deviation.type == "OGM")) 

lvls[combs$deviation.type == "Shift"] <- rep(list(c("True Scenario", unique(grep("Shift", res.50.clean$scenario, value = TRUE)))[c(4:1, 5:7)]), 
                                             sum(combs$deviation.type == "Shift")) 
lvls[combs$deviation.type == "Scale"] <- rep(list(c("True Scenario", unique(grep("Scale", res.50.clean$scenario, value = TRUE)))[c(2:4, 1, 5:7)]), 
                                             sum(combs$deviation.type == "Scale")) 
any(sapply(lvls, is.null))

tapply(res.50.clean$Accuracy, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(is.na(x)))
tapply(res.50.clean$AUC, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(is.na(x)))
tapply(res.50.clean$Brier.Score, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(is.na(x)))
tapply(res.50.clean$F1.Score, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(is.na(x)))
tapply(res.50.clean$Sensitivity, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(is.na(x)))
tapply(res.50.clean$Specificity, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(is.na(x)))

tapply(res.50.clean$errors, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(x > 0))
tapply(res.50.clean$warnings, list(res.50.clean$scenario, res.50.clean$classifier), function(x) mean(x > 0))

tab <- tapply(res.50.clean$errors, list(res.50.clean$scenario, res.50.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:error.50", 
             caption = "Number of iterations with error messages per scenario and classifier for $p = 50$. Scenarios with no error messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")
tab <- tapply(res.50.clean$warnings, list(res.50.clean$scenario, res.50.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:warning.50", 
             caption = "Number of iterations with warning messages per scenario and classifier for $p = 50$. Scenarios with no warning messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")

# plot everything
pdf("plots_error_results_p=50_n=100_2024_07_30.pdf", height = 10, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotError(res = res.50.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                  lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()

pdf("plots_order_Kendall_results_p=50_n=100_2024_07_30.pdf", height = 8, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotKendall(res = res.50.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                    lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()

pdf("plots_comparison_simulation_types_p=50_n=100_2024_07_30.pdf", height = 10, width = 12)
for(m in measure) {
  # print(i)
  print(plotCompPlasmode(res = res.50.clean, measure = m, 
                         ogm = "Logistic Model"))
}
dev.off()

################################################################################
##                          PLOT RESULTS FOR p = 150                          ##
##                                                                            ##
################################################################################
# Define all combinations of deviation, measure and true ogm
deviation.type <- c("Correlation", "Distribution", "OGM", "Scale", "Shift")
measure <- c("Accuracy", "AUC", "Brier.Score", "F1.Score", "Sensitivity", "Specificity")
combs <- expand.grid(measure, deviation.type)
colnames(combs) <- c("measure", "deviation.type")
lvls <- lapply(1:nrow(combs), function(i) character(0))
lvls[combs$deviation.type == "Correlation"] <- rep(list(c("True Scenario", unique(grep("Correlation", res.150.clean$scenario, value = TRUE)))[c(1, 3:2, 4:6)]), 
                                                   sum(combs$deviation.type == "Correlation"))
lvls[combs$deviation.type == "Distribution"] <- rep(list(c("True Scenario", unique(grep("Distribution", res.150.clean$scenario, value = TRUE)))), 
                                                    sum(combs$deviation.type == "Distribution"))    
lvls[combs$deviation.type == "OGM"] <- rep(list(c("True Scenario", unique(grep("OGM", res.150.clean$scenario, value = TRUE)))[c(3, 1, 4, 2)]),
                                           sum(combs$deviation.type == "OGM")) 

lvls[combs$deviation.type == "Shift"] <- rep(list(c("True Scenario", unique(grep("Shift", res.150.clean$scenario, value = TRUE)))[c(4:1, 5:7)]), 
                                             sum(combs$deviation.type == "Shift")) 
lvls[combs$deviation.type == "Scale"] <- rep(list(c("True Scenario", unique(grep("Scale", res.150.clean$scenario, value = TRUE)))[c(2:4, 1, 5:7)]), 
                                             sum(combs$deviation.type == "Scale")) 
any(sapply(lvls, is.null))

tapply(res.150.clean$Accuracy, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(is.na(x)))
tapply(res.150.clean$AUC, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(is.na(x)))
tapply(res.150.clean$Brier.Score, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(is.na(x)))
tapply(res.150.clean$F1.Score, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(is.na(x)))
tapply(res.150.clean$Sensitivity, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(is.na(x)))
tapply(res.150.clean$Specificity, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(is.na(x)))


tapply(res.150.clean$errors, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(x > 0))
tapply(res.150.clean$warnings, list(res.150.clean$scenario, res.150.clean$classifier), function(x) mean(x > 0))

tab <- tapply(res.150.clean$errors, list(res.150.clean$scenario, res.150.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:error.150", 
             caption = "Number of iterations with error messages per scenario and classifier for $p = 150$. Scenarios with no error messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")
tab <- tapply(res.150.clean$warnings, list(res.150.clean$scenario, res.150.clean$classifier), 
              function(x) sum(x > 0))
tab <- tab[rowSums(tab) > 0, ]
print(xtable(tab, label = "tab:warning.150", 
             caption = "Number of iterations with warning messages per scenario and classifier for $p = 150$. Scenarios with no warning messages are not displayed."), 
      booktabs = TRUE, table.placement = "!tb")

# plot everything
pdf("plots_error_results_p=150_n=100_2024_07_30.pdf", height = 10, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotError(res = res.150.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                  lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()

pdf("plots_ranks_results_p=150_n=100_2024_07_30.pdf", height = 10, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotRankMosaic(res = res.150.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                       lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()

pdf("plots_order_Kendall_results_p=150_n=100_2024_07_30.pdf", height = 8, width = 12)
for(i in 1:nrow(combs)) {
  # print(i)
  print(plotKendall(res = res.150.clean, measure = combs$measure[i], deviation.type = combs$deviation.type[i], 
                    lvls = lvls[[i]], ogm = "Logistic Model"))
}
dev.off()


pdf("plots_comparison_simulation_types_p=150_n=100_2024_07_30.pdf", height = 10, width = 12)
for(m in measure) {
  # print(i)
  print(plotCompPlasmode(res = res.150.clean, measure = m, 
                         ogm = "Logistic Model"))
}
dev.off()

################################################################################
##                            ACCEPTABLE SIMULATION RUNS                      ##
##                                                                            ##
################################################################################
err.2 <- calcAccSim(res.2.clean)
err.10 <- calcAccSim(res.10.clean)
err.50 <- calcAccSim(res.50.clean)
err.150 <- calcAccSim(res.150.clean)

pdf("plots_proportion_acceptable_runs_2024_07_30.pdf", height = 10 * 13/12, 
    width = 13)
plotAccSim(err.2)
plotAccSim(err.10)
plotAccSim(err.50)
plotAccSim(err.150)
dev.off()

################################################################################
##                            TRUE CORRELATIONS                               ##
##                                                                            ##
################################################################################
target.cor <- read.csv("true_dgp_target_cor.csv")
pdf("hist_true_cors.pdf", height = 4, width = 6)
par(mar = c(5.1, 4.1, 2.1, 2.1))
hist(target.cor[upper.tri(target.cor, diag = FALSE)], freq = FALSE, main = "",
     xlab = "Pairwise correlations")
box()
dev.off()

################################################################################
##                    DISTRIBUTION OF VARIABLES FOR TRUE DGP                  ##
##                                                                            ##
################################################################################
source("../Code/simulation_functions_new.R")
load("true_dgp_new.RData", verbose = TRUE)
set.seed(2024)
X <- generateCovariatesParam(1000, adjusted.corr = adj.corr.10, 
                             margins = eval(parse(text = m.10)), 
                             shift = 0, scale = 1, norm = TRUE)
X <- data.frame(Value = as.numeric(unlist(X)), 
                Variable = factor(rep(c(paste0("Normal ", 1:3), 
                                        paste0("Log-normal ", 1:3), 
                                        paste0("Outlier ", 1:2), 
                                        paste0("Bimodal ", 1:2)), each = 1000), 
                                  levels = c(paste0("Normal ", 1:3), 
                                             paste0("Log-normal ", 1:3), 
                                             paste0("Outlier ", 1:2), 
                                             paste0("Bimodal ", 1:2))))
pdf("violin_true_dgp.pdf", height = 4, width = 6)
ggplot(data = X, aes(x = Variable, y = Value)) + geom_violin(bounds = 0:1, 
                                                             scale = "width") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15), 
        text = element_text(size = 17), 
        title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        strip.background = element_rect(fill="white"), 
        strip.text = element_text(size = 15))
dev.off()

################################################################################
##                      DISTRIBUTION OF TRUE COEFFICIENTS                     ##
##                                                                            ##
################################################################################
set.seed(1406)
coefs <- numeric(150)
coefs[1:125] <- ifelse(runif(125) < 0.5, runif(125, -8, -3), runif(125, 3, 8))
coefs[126:150] <- ifelse(runif(25) < 0.5, runif(25, -15, -10), runif(25, 10, 15))
pdf("hist_true_ogm.pdf", height = 4, width = 6)
hist(coefs, freq = FALSE, breaks = -15:15, xlab = "Coefficients", las = 1, main = "")
box()
dev.off()

