source("simulation_functions_new.R")
load("../Results/true_dgp.RData", verbose = TRUE)
################################################################################
set.seed(1406)
JuliaCall::julia_eval('using Random; Random.seed!(42);')
X <- generateCovariatesParam(3000, adjusted.corr = adj.corr, margins = eval(parse(text = m)), 
                             shift = 0, scale = 1, norm = TRUE)
set.seed(1406)
coefs <- numeric(150)
coefs[1:125] <- ifelse(runif(125) < 0.5, runif(125, -8, -3), runif(125, 3, 8))
coefs[126:150] <- ifelse(runif(25) < 0.5, runif(25, -15, -10), runif(25, 10, 15))
eta <- drop(as.matrix(X) %*% coefs + 38.5)
prob <- 1 / (1 + exp(-eta))
# pdf("../Results/hist_prob_LR_new.pdf", height = 4, width = 6)
hist(prob, freq = FALSE, main = "")
eta.scale <- drop(3/4 * as.matrix(X) %*% coefs + 38.5)
prob.scale <- 1 / (1 + exp(-eta.scale))
# pdf("../Results/hist_prob_LR_new.pdf", height = 4, width = 6)
hist(prob.scale, freq = FALSE, main = "")
# dev.off()
mean(prob > 0.5)
y <- rbinom(3000, 1, prob = prob)
table(y)

ridge <- glmnet::cv.glmnet(as.matrix(X[1:2000,]), y[1:2000], alpha = 0, family = "binomial")
conf <- table(predict(ridge, new = as.matrix(X[2001:3000, ]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.936
conf[1, 1] / sum(conf[, 1])
# 0.9496982
conf[2, 2] / sum(conf[, 2])
# 0.9224652

ridge <- glmnet::cv.glmnet(as.matrix(X[1:100,]), y[1:100], alpha = 0, family = "binomial")
conf <- table(pred = predict(ridge, new = as.matrix(X[201:300, ]), type = "class"), true = y[201:300])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.7
conf[1, 1] / sum(conf[, 1])
# 0.8333333
conf[2, 2] / sum(conf[, 2])
# 0.5

lasso <- glmnet::cv.glmnet(as.matrix(X[1:2000,]), y[1:2000], alpha = 1, family = "binomial")
conf <- table(predict(lasso, new = as.matrix(X[2001:3000, ]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.946
conf[1, 1] / sum(conf[, 1])
# 0.9577465
conf[2, 2] / sum(conf[, 2])
# 0.9343936

lasso <- glmnet::cv.glmnet(as.matrix(X[1:100,]), y[1:100], alpha = 1, family = "binomial")
conf <- table(pred = factor(predict(lasso, new = as.matrix(X[201:300, ]), type = "class"), levels= 0:1), true = y[201:300])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.69
conf[1, 1] / sum(conf[, 1])
# 0.65
conf[2, 2] / sum(conf[, 2])
# 0.75

rf <- ranger::ranger(x = X[1:2000,], y = y[1:2000], classification = TRUE)
conf <- table(predict(rf, data = X[2001:3000, ], type = "response")$predictions, y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.795
conf[1, 1] / sum(conf[, 1])
# 0.7686117
conf[2, 2] / sum(conf[, 2])
# 0.8210736

rf <- ranger::ranger(x = X[1:100,], y = y[1:100], classification = TRUE)
conf <- table(predict(rf, data = X[201:300, ], type = "response")$predictions, y[201:300])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.67
conf[1, 1] / sum(conf[, 1])
# 0.8
conf[2, 2] / sum(conf[, 2])
# 0.475

# p = 50
set.seed(1406)
inds <- c(1:15, 51:65, 101:110, 126:135)
coefs.50 <- coefs[inds] + ifelse(coefs[inds] > 0, 4, -4)
coefs.50[41:50] <- coefs.50[41:50] + ifelse(coefs.50[41:50] > 0, 2, -2)
eta <- drop(as.matrix(X[, inds]) %*% coefs.50 - 67.2)
prob <- 1 / (1 + exp(-eta))
# pdf("../Results/hist_prob_LR_p=50.pdf", height = 4, width = 6)
hist(prob, freq = FALSE, main = "")
# dev.off()
mean(prob > 0.5)

# Effect of shifting on probabilities
exp(-0.5 * sum(coefs.50))
eta.shift <- drop((as.matrix(X[, inds]) * 0.5) %*% coefs.50 - 67.2)
prob.shift <- 1 / (1 + exp(-eta.shift))
# pdf("../Results/hist_prob_LR_p=50.pdf", height = 4, width = 6)
hist(prob.shift, freq = FALSE, main = "")
#dev.off()
summary(prob.shift)

# Have a first look at the performance
set.seed(0407)
y <- rbinom(3000, 1, prob = prob)
table(y)
ridge <- glmnet::cv.glmnet(as.matrix(X[1:2000, inds]), y[1:2000], alpha = 0, family = "binomial")
conf <- table(predict(ridge, new = as.matrix(X[2001:3000, inds]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.948
conf[1, 1] / sum(conf[, 1])
# 0.9404762
conf[2, 2] / sum(conf[, 2])
# 0.9556452

ridge <- glmnet::cv.glmnet(as.matrix(X[1:50, inds]), y[1:50], alpha = 0, family = "binomial")
conf <- table(pred = factor(predict(ridge, new = as.matrix(X[51:100, inds]), type = "class"), levels = 0:1), 
                            true = y[51:100])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.42
conf[1, 1] / sum(conf[, 1])
# 1
conf[2, 2] / sum(conf[, 2])
# 0.06451613

ridge <- glmnet::cv.glmnet(as.matrix(X[1:100, inds]), y[1:100], alpha = 0, family = "binomial")
conf <- table(pred = factor(predict(ridge, new = as.matrix(X[101:200, inds]), type = "class"), levels = 0:1), 
              true = y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.82
conf[1, 1] / sum(conf[, 1])
# 0.8888889
conf[2, 2] / sum(conf[, 2])
# 0.7636364

lasso <- glmnet::cv.glmnet(as.matrix(X[1:2000, inds]), y[1:2000], alpha = 1, family = "binomial")
conf <- table(predict(lasso, new = as.matrix(X[2001:3000, inds]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.957
conf[1, 1] / sum(conf[, 1])
# 0.952381
conf[2, 2] / sum(conf[, 2])
# 0.9616935

lasso <- glmnet::cv.glmnet(as.matrix(X[1:50, inds]), y[1:50], alpha = 1, family = "binomial")
conf <- table(pred = factor(predict(lasso, new = as.matrix(X[51:100, inds]), type = "class"), levels= 0:1), 
              true = y[51:100])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.58
conf[1, 1] / sum(conf[, 1])
# 0.8947368
conf[2, 2] / sum(conf[, 2])
# 0.3870968

lasso <- glmnet::cv.glmnet(as.matrix(X[1:100, inds]), y[1:100], alpha = 1, family = "binomial")
conf <- table(pred = factor(predict(lasso, new = as.matrix(X[101:200, inds]), type = "class"), levels= 0:1), 
              true = y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.75
conf[1, 1] / sum(conf[, 1])
# 0.7777778
conf[2, 2] / sum(conf[, 2])
# 0.7272727

rf <- ranger::ranger(x = X[1:2000,], y = y[1:2000], classification = TRUE)
conf <- table(predict(rf, data = X[2001:3000, ], type = "response")$predictions, y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.832
conf[1, 1] / sum(conf[, 1])
# 0.8075397
conf[2, 2] / sum(conf[, 2])
# 0.8568548 

rf <- ranger::ranger(x = X[1:50,], y = y[1:50], classification = TRUE)
conf <- table(predict(rf, data = X[51:100, ], type = "response")$predictions, y[51:100])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.62
conf[1, 1] / sum(conf[, 1])
# 0.8421053
conf[2, 2] / sum(conf[, 2])
# 0.483871

rf <- ranger::ranger(x = X[1:100,], y = y[1:100], classification = TRUE)
conf <- table(predict(rf, data = X[101:200, ], type = "response")$predictions, y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.73
conf[1, 1] / sum(conf[, 1])
# 0.6
conf[2, 2] / sum(conf[, 2])
# 0.8363636


# p = 10
set.seed(1406)
inds <- c(1:3, 51:53, 101:102, 126:127)
coefs.10 <- coefs[inds]
eta <- drop(as.matrix(X[, inds]) %*% coefs.10 - 9.6)
prob <- 1 / (1 + exp(-eta))
# pdf("../Results/hist_prob_LR_p=10.pdf", height = 4, width = 6)
hist(prob, freq = FALSE, main = "")
# dev.off()
mean(prob > 0.5)

set.seed(0407)
y <- rbinom(3000, 1, prob = prob)
table(y)
ridge <- glmnet::cv.glmnet(as.matrix(X[1:2000, inds]), y[1:2000], alpha = 0, family = "binomial")
conf <- table(predict(ridge, new = as.matrix(X[2001:3000, inds]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.912
conf[1, 1] / sum(conf[, 1])
# 0.9309021
conf[2, 2] / sum(conf[, 2])
# 0.8914405

ridge <- glmnet::cv.glmnet(as.matrix(X[1:100, inds]), y[1:100], alpha = 0, family = "binomial")
conf <- table(pred = factor(predict(ridge, new = as.matrix(X[101:200, inds]), type = "class"), levels = 0:1), 
              true = y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.91
conf[1, 1] / sum(conf[, 1])
# 0.9166667
conf[2, 2] / sum(conf[, 2])
# 0.9038462

lasso <- glmnet::cv.glmnet(as.matrix(X[1:2000, inds]), y[1:2000], alpha = 1, family = "binomial")
conf <- table(predict(lasso, new = as.matrix(X[2001:3000, inds]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.911
conf[1, 1] / sum(conf[, 1])
# 0.9270633
conf[2, 2] / sum(conf[, 2])
# 0.8935282

lasso <- glmnet::cv.glmnet(as.matrix(X[1:100, inds]), y[1:100], alpha = 1, family = "binomial")
conf <- table(pred = factor(predict(lasso, new = as.matrix(X[101:200, inds]), type = "class"), levels= 0:1), 
              true = y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.9
conf[1, 1] / sum(conf[, 1])
# 0.9166667
conf[2, 2] / sum(conf[, 2])
# 0.8846154

rf <- ranger::ranger(x = X[1:2000,], y = y[1:2000], classification = TRUE)
conf <- table(predict(rf, data = X[2001:3000, ], type = "response")$predictions, y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.898
conf[1, 1] / sum(conf[, 1])
# 0.9328215
conf[2, 2] / sum(conf[, 2])
# 0.8601253 

rf <- ranger::ranger(x = X[1:100,], y = y[1:100], classification = TRUE)
conf <- table(predict(rf, data = X[101:200, ], type = "response")$predictions, y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.84
conf[1, 1] / sum(conf[, 1])
# 0.8125
conf[2, 2] / sum(conf[, 2])
# 0.8653846

# p = 2
set.seed(1406)
inds <- c(1, 127)
coefs.2 <- coefs[inds] * c(6, -2)
eta <- drop(as.matrix(X[, inds]) %*% coefs.2 - 1.5)
prob <- 1 / (1 + exp(-eta ))
# pdf("../Results/hist_prob_LR_p=2.pdf", height = 4, width = 6)
hist(prob, freq = FALSE, main = "")
exp(-0.5 * sum(coefs.2))
eta.shift <- drop((as.matrix(X[, inds]) - 0.5) %*% coefs.2 - 2)
prob.shift <- 1 / (1 + exp(-eta.shift))
hist(prob.shift, freq = FALSE, main = "")
summary(prob.shift)
mean(prob > 0.5)

set.seed(0407)
y <- rbinom(3000, 1, prob = prob)
table(y)
ridge <- glmnet::cv.glmnet(as.matrix(X[1:2000, inds]), y[1:2000], alpha = 0, family = "binomial")
conf <- table(predict(ridge, new = as.matrix(X[2001:3000, inds]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.925
conf[1, 1] / sum(conf[, 1])
# 0.9714868
conf[2, 2] / sum(conf[, 2])
# 0.8801572

ridge <- glmnet::cv.glmnet(as.matrix(X[1:100, inds]), y[1:100], alpha = 0, family = "binomial")
conf <- table(pred = factor(predict(ridge, new = as.matrix(X[101:200, inds]), type = "class"), levels = 0:1), 
              true = y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.94
conf[1, 1] / sum(conf[, 1])
# 1
conf[2, 2] / sum(conf[, 2])
# 0.8666667

lasso <- glmnet::cv.glmnet(as.matrix(X[1:2000, inds]), y[1:2000], alpha = 1, family = "binomial")
conf <- table(predict(lasso, new = as.matrix(X[2001:3000, inds]), type = "class"), y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.945
conf[1, 1] / sum(conf[, 1])
# 0.9613035
conf[2, 2] / sum(conf[, 2])
# 0.9292731

lasso <- glmnet::cv.glmnet(as.matrix(X[1:100, inds]), y[1:100], alpha = 1, family = "binomial")
conf <- table(pred = factor(predict(lasso, new = as.matrix(X[101:200, inds]), type = "class"), levels= 0:1), 
              true = y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.94
conf[1, 1] / sum(conf[, 1])
# 1
conf[2, 2] / sum(conf[, 2])
# 0.8666667

rf <- ranger::ranger(x = X[1:2000,], y = y[1:2000], classification = TRUE)
conf <- table(predict(rf, data = X[2001:3000, ], type = "response")$predictions, y[2001:3000])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.932
conf[1, 1] / sum(conf[, 1])
# 0.9490835
conf[2, 2] / sum(conf[, 2])
# 0.9155206

rf <- ranger::ranger(x = X[1:100,], y = y[1:100], classification = TRUE)
conf <- table(predict(rf, data = X[101:200, ], type = "response")$predictions, y[101:200])
(conf[1, 1] + conf[2, 2]) / sum(conf)
# 0.82
conf[1, 1] / sum(conf[, 1])
# 0.9454545
conf[2, 2] / sum(conf[, 2])
# 0.6666667