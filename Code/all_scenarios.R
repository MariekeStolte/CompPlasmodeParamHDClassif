source("simulation_functions_new.R")
load("../Results/true_dgp_new.RData", verbose = TRUE)

################################################################################
## p = 150
set.seed(1406)
coefs <- numeric(150)
coefs[1:125] <- ifelse(runif(125) < 0.5, runif(125, -8, -3), runif(125, 3, 8))
coefs[126:150] <- ifelse(runif(25) < 0.5, runif(25, -15, -10), runif(25, 10, 15))
LR.150 <- list(intercept = 38.5, slopes = coefs)
class(LR.150) <- "LogRegModel"
# Parametric
dgp.true.param.150 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                           margins = m, norm = TRUE, shift = 0, scale = 1, cor.par = "true")

dgp.shift.neg.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                          margins = m, norm = TRUE, shift = -0.5, scale = 1, cor.par = "true")
dgp.shift.neg.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                           margins = m, norm = TRUE, shift = -0.25, scale = 1, cor.par = "true")
dgp.shift.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                      margins = m, norm = TRUE, shift = 0.5, scale = 1, cor.par = "true")
dgp.shift.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                       margins = m, norm = TRUE, shift = 0.25, scale = 1, cor.par = "true")
dgp.shift.neg.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                           margins = m, norm = TRUE, shift = -0.125, scale = 1, cor.par = "true")
dgp.shift.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                       margins = m, norm = TRUE, shift = 0.125, scale = 1, cor.par = "true")


dgp.scale.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                       margins = m, norm = TRUE, shift = 0, scale = 0.25, cor.par = "true")
dgp.scale.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                      margins = m, norm = TRUE, shift = 0, scale = 0.5, cor.par = "true")
dgp.scale.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                    margins = m, norm = TRUE, shift = 0, scale = 2, cor.par = "true")
dgp.scale.4 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                    margins = m, norm = TRUE, shift = 0, scale = 4, cor.par = "true")
dgp.scale.0.75 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                       margins = m, norm = TRUE, shift = 0, scale = 0.75, cor.par = "true")
dgp.scale.4.3 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr, 
                      margins = m, norm = TRUE, shift = 0, scale = 4/3, cor.par = "true")

adj.no.cor <- as.matrix(read.csv("../Results/dev_dgp_adj_no_cor.csv"))
adj.corr.0.1 <- as.matrix(read.csv("../Results/dev_dgp_adj_cor_1.csv"))
adj.corr.0.2 <- as.matrix(read.csv("../Results/dev_dgp_adj_cor_2.csv"))
adj.corr.neg.0.1 <- as.matrix(read.csv("../Results/dev_dgp_adj_cor_neg_1.csv"))
adj.corr.neg.0.2 <- as.matrix(read.csv("../Results/dev_dgp_adj_cor_neg_2.csv"))

dgp.corr.neg.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2, 
                         margins = m, norm = TRUE, shift = 0, scale = 1, cor.par = -0.2)
dgp.corr.neg.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.1, 
                         margins = m, norm = TRUE, shift = 0, scale = 1, cor.par = -0.1)
dgp.corr.no.corr <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.no.cor, 
                         margins = m, norm = TRUE, shift = 0, scale = 1, cor.par = 0)
dgp.corr.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2, 
                     margins = m, norm = TRUE, shift = 0, scale = 1, cor.par = 0.1)
dgp.corr.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.0.2, 
                     margins = m, norm = TRUE, shift = 0, scale = 1, cor.par = 0.2)

m.sn <- paste0("c(", paste0(rep("dist$Normal(0, 1)", 150), collapse = ", "), ")")
dgp.sn <- list(sim.type = "parametric", n = 100, adjusted.corr = diag(150), 
               margins = m.sn, norm = TRUE, shift = 0, scale = 1, cor.par = "sn")

param.dgp.150 <- c(c(rep(list(dgp.true.param.150), 6), list(dgp.shift.neg.0.5, dgp.shift.neg.0.25, 
                                                            dgp.shift.0.5, dgp.shift.0.25, dgp.scale.0.25, dgp.scale.0.5, 
                                                            dgp.scale.2, dgp.scale.4, dgp.corr.neg.0.2, dgp.corr.neg.0.1, 
                                                            dgp.corr.no.corr, dgp.corr.0.1, dgp.corr.0.2, dgp.sn)), # deviations dgp 
                   rep(list(dgp.true.param.150), 3)) # deviations ogm
param.factor.logit <- c(rep(1, 20), 0.5, 2, 0)
ogm.lr <- list(intercept = 0, slopes = rep(0, 150))
class(ogm.lr) <- "LogRegModel"
param.ogm.150 <- rep(list(LR.150), 23)
param.n.iter <- rep(100, 23)

# Plasmode
dgp.m.out.of.n <- list(sim.type = "Plasmode", prop = 0.632, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr, margins = m, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.n.out.of.n <- list(sim.type = "Plasmode", prop = 1, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr, margins = m, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.subsampling <- list(sim.type = "Plasmode", prop = 0.632, replace = FALSE, 
                        n = 100, adjusted.corr = adj.corr, margins = m, 
                        norm = TRUE, shift = 0, scale = 1)
dgp.no.resampling <- list(sim.type = "Plasmode", prop = 1, replace = FALSE, 
                          n = 100, adjusted.corr = adj.corr, margins = m, 
                          norm = TRUE, shift = 0, scale = 1)
plasmode.dgp.150 <- rep(list(dgp.m.out.of.n, dgp.n.out.of.n, dgp.subsampling, dgp.no.resampling), each = 4)
plasmode.factor.logit <- rep(c(0.5, 1, 2, 0), 4)
plasmode.ogm.150 <- rep(list(LR.150), 16)
plasmode.n.iter <- rep(100, 16)

dgp.150 <- c(param.dgp.150, plasmode.dgp.150)
factor.logit <- c(param.factor.logit, plasmode.factor.logit)
ogm.150 <- c(param.ogm.150, plasmode.ogm.150)
n.iter <- c(param.n.iter, plasmode.n.iter)

dgp.150.add <- list(dgp.scale.0.75, dgp.scale.4.3)
factor.logit.add <- c(1, 1)
ogm.150.add <- rep(list(LR.150), 2)
n.iter.add <- rep(100, 2)

dgp.150.add2 <- list(dgp.shift.neg.0.125, dgp.shift.0.125)
factor.logit.add2 <- c(1, 1)
ogm.150.add2 <- rep(list(LR.150), 2)
n.iter.add2 <- rep(100, 2)

################################################################################
## p = 50
set.seed(1406)
coefs <- numeric(150)
coefs[1:125] <- ifelse(runif(125) < 0.5, runif(125, -8, -3), runif(125, 3, 8))
coefs[126:150] <- ifelse(runif(25) < 0.5, runif(25, -15, -10), runif(25, 10, 15))
inds <- c(1:15, 51:65, 101:110, 126:135)
coefs.50 <- coefs[inds] + ifelse(coefs[inds] > 0, 4, -4)
coefs.50[41:50] <- coefs.50[41:50] + ifelse(coefs.50[41:50] > 0, 2, -2)
LR.50 <- list(intercept = -67.2, slopes = coefs.50)
class(LR.50) <- "LogRegModel"
# Parametric
dgp.true.param.50 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                          margins = m.50, norm = TRUE, shift = 0, scale = 1, cor.par = "true")

dgp.shift.neg.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                          margins = m.50, norm = TRUE, shift = -0.5, scale = 1, cor.par = "true")
dgp.shift.neg.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                           margins = m.50, norm = TRUE, shift = -0.25, scale = 1, cor.par = "true")
dgp.shift.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                      margins = m.50, norm = TRUE, shift = 0.5, scale = 1, cor.par = "true")
dgp.shift.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                       margins = m.50, norm = TRUE, shift = 0.25, scale = 1, cor.par = "true")
dgp.shift.neg.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                           margins = m.50, norm = TRUE, shift = -0.125, scale = 1, cor.par = "true")
dgp.shift.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                       margins = m.50, norm = TRUE, shift = 0.125, scale = 1, cor.par = "true")

dgp.scale.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                       margins = m.50, norm = TRUE, shift = 0, scale = 0.25, cor.par = "true")
dgp.scale.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                      margins = m.50, norm = TRUE, shift = 0, scale = 0.5, cor.par = "true")
dgp.scale.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                    margins = m.50, norm = TRUE, shift = 0, scale = 2, cor.par = "true")
dgp.scale.4 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                    margins = m.50, norm = TRUE, shift = 0, scale = 4, cor.par = "true")
dgp.scale.0.75 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                       margins = m.50, norm = TRUE, shift = 0, scale = 0.75, cor.par = "true")
dgp.scale.4.3 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.50, 
                      margins = m.50, norm = TRUE, shift = 0, scale = 4/3, cor.par = "true")


dgp.corr.neg.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2[inds, inds], 
                         margins = m.50, norm = TRUE, shift = 0, scale = 1, cor.par = -0.2)
dgp.corr.neg.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.1[inds, inds], 
                         margins = m.50, norm = TRUE, shift = 0, scale = 1, cor.par = -0.1)
dgp.corr.no.corr <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.no.cor[inds, inds], 
                         margins = m.50, norm = TRUE, shift = 0, scale = 1, cor.par = 0)
dgp.corr.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2[inds, inds], 
                     margins = m.50, norm = TRUE, shift = 0, scale = 1, cor.par = 0.1)
dgp.corr.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.0.2[inds, inds], 
                     margins = m.50, norm = TRUE, shift = 0, scale = 1, cor.par = 0.2)

m.sn <- paste0("c(", paste0(rep("dist$Normal(0, 1)", 50), collapse = ", "), ")")
dgp.sn <- list(sim.type = "parametric", n = 100, adjusted.corr = diag(50), 
               margins = m.sn, norm = TRUE, shift = 0, scale = 1, cor.par = "sn")

param.dgp.50 <- c(c(rep(list(dgp.true.param.50), 6), list(dgp.shift.neg.0.5, dgp.shift.neg.0.25, 
                                                          dgp.shift.0.5, dgp.shift.0.25, dgp.scale.0.25, dgp.scale.0.5, 
                                                          dgp.scale.2, dgp.scale.4, dgp.corr.neg.0.2, dgp.corr.neg.0.1, 
                                                          dgp.corr.no.corr, dgp.corr.0.1, dgp.corr.0.2, dgp.sn)), # deviations dgp 
                  rep(list(dgp.true.param.50), 3)) # deviations ogm
ogm.lr <- list(intercept = 0, slopes = rep(0, 50))
class(ogm.lr) <- "LogRegModel"
param.ogm.50 <- rep(list(LR.50), 23)

# Plasmode
dgp.m.out.of.n <- list(sim.type = "Plasmode", prop = 0.632, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr.50, margins = m.50, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.n.out.of.n <- list(sim.type = "Plasmode", prop = 1, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr.50, margins = m.50, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.subsampling <- list(sim.type = "Plasmode", prop = 0.632, replace = FALSE, 
                        n = 100, adjusted.corr = adj.corr.50, margins = m.50, 
                        norm = TRUE, shift = 0, scale = 1)
dgp.no.resampling <- list(sim.type = "Plasmode", prop = 1, replace = FALSE, 
                          n = 100, adjusted.corr = adj.corr.50, margins = m.50, 
                          norm = TRUE, shift = 0, scale = 1)
plasmode.dgp.50 <- rep(list(dgp.m.out.of.n, dgp.n.out.of.n, dgp.subsampling, dgp.no.resampling), each = 4)
plasmode.ogm.50 <- rep(list(LR.50), 16)

dgp.50 <- c(param.dgp.50, plasmode.dgp.50)
ogm.50 <- c(param.ogm.50, plasmode.ogm.50)

dgp.50.add <- list(dgp.scale.0.75, dgp.scale.4.3)
ogm.50.add <- rep(list(LR.50), 2)

dgp.50.add2 <- list(dgp.shift.neg.0.125, dgp.shift.0.125)
ogm.50.add2 <- rep(list(LR.50), 2)


################################################################################
## p = 10
set.seed(1406)
coefs <- numeric(150)
coefs[1:125] <- ifelse(runif(125) < 0.5, runif(125, -8, -3), runif(125, 3, 8))
coefs[126:150] <- ifelse(runif(25) < 0.5, runif(25, -15, -10), runif(25, 10, 15))
inds <- c(1:3, 51:53, 101:102, 126:127)
coefs.10 <- coefs[inds]
LR.10 <- list(intercept = -9.6, slopes = coefs.10)
class(LR.10) <- "LogRegModel"
# Parametric
dgp.true.param.10 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                          margins = m.10, norm = TRUE, shift = 0, scale = 1, cor.par = "true")

dgp.shift.neg.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                          margins = m.10, norm = TRUE, shift = -0.5, scale = 1, cor.par = "true")
dgp.shift.neg.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                           margins = m.10, norm = TRUE, shift = -0.25, scale = 1, cor.par = "true")
dgp.shift.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                      margins = m.10, norm = TRUE, shift = 0.5, scale = 1, cor.par = "true")
dgp.shift.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                       margins = m.10, norm = TRUE, shift = 0.25, scale = 1, cor.par = "true")
dgp.shift.neg.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                           margins = m.10, norm = TRUE, shift = -0.125, scale = 1, cor.par = "true")
dgp.shift.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                       margins = m.10, norm = TRUE, shift = 0.125, scale = 1, cor.par = "true")

dgp.scale.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                       margins = m.10, norm = TRUE, shift = 0, scale = 0.25, cor.par = "true")
dgp.scale.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                      margins = m.10, norm = TRUE, shift = 0, scale = 0.5, cor.par = "true")
dgp.scale.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                    margins = m.10, norm = TRUE, shift = 0, scale = 2, cor.par = "true")
dgp.scale.4 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                    margins = m.10, norm = TRUE, shift = 0, scale = 4, cor.par = "true")
dgp.scale.0.75 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                       margins = m.10, norm = TRUE, shift = 0, scale = 0.75, cor.par = "true")
dgp.scale.4.3 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.10, 
                      margins = m.10, norm = TRUE, shift = 0, scale = 4/3, cor.par = "true")


dgp.corr.neg.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2[inds, inds], 
                         margins = m.10, norm = TRUE, shift = 0, scale = 1, cor.par = -0.2)
dgp.corr.neg.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.1[inds, inds], 
                         margins = m.10, norm = TRUE, shift = 0, scale = 1, cor.par = -0.1)
dgp.corr.no.corr <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.no.cor[inds, inds], 
                         margins = m.10, norm = TRUE, shift = 0, scale = 1, cor.par = 0)
dgp.corr.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2[inds, inds], 
                     margins = m.10, norm = TRUE, shift = 0, scale = 1, cor.par = 0.1)
dgp.corr.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.0.2[inds, inds], 
                     margins = m.10, norm = TRUE, shift = 0, scale = 1, cor.par = 0.2)

m.sn <- paste0("c(", paste0(rep("dist$Normal(0, 1)", 10), collapse = ", "), ")")
dgp.sn <- list(sim.type = "parametric", n = 100, adjusted.corr = diag(10), 
               margins = m.sn, norm = TRUE, shift = 0, scale = 1, cor.par = "sn")

param.dgp.10 <- c(c(rep(list(dgp.true.param.10), 6), list(dgp.shift.neg.0.5, dgp.shift.neg.0.25, 
                                                          dgp.shift.0.5, dgp.shift.0.25, dgp.scale.0.25, dgp.scale.0.5, 
                                                          dgp.scale.2, dgp.scale.4, dgp.corr.neg.0.2, dgp.corr.neg.0.1, 
                                                          dgp.corr.no.corr, dgp.corr.0.1, dgp.corr.0.2, dgp.sn)), # deviations dgp 
                  rep(list(dgp.true.param.10), 3)) # deviations ogm
ogm.lr <- list(intercept = 0, slopes = rep(0, 10))
class(ogm.lr) <- "LogRegModel"
param.ogm.10 <- rep(list(LR.10), 23)

# Plasmode
dgp.m.out.of.n <- list(sim.type = "Plasmode", prop = 0.632, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr.10, margins = m.10, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.n.out.of.n <- list(sim.type = "Plasmode", prop = 1, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr.10, margins = m.10, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.subsampling <- list(sim.type = "Plasmode", prop = 0.632, replace = FALSE, 
                        n = 100, adjusted.corr = adj.corr.10, margins = m.10, 
                        norm = TRUE, shift = 0, scale = 1)
dgp.no.resampling <- list(sim.type = "Plasmode", prop = 1, replace = FALSE, 
                          n = 100, adjusted.corr = adj.corr.10, margins = m.10, 
                          norm = TRUE, shift = 0, scale = 1)
plasmode.dgp.10 <- rep(list(dgp.m.out.of.n, dgp.n.out.of.n, dgp.subsampling, dgp.no.resampling), each = 4)
plasmode.ogm.10 <- rep(list(LR.10), 16)

dgp.10 <- c(param.dgp.10, plasmode.dgp.10)
ogm.10 <- c(param.ogm.10, plasmode.ogm.10)

dgp.10.add <- list(dgp.scale.0.75, dgp.scale.4.3)
ogm.10.add <- rep(list(LR.10), 2)

dgp.10.add2 <- list(dgp.shift.neg.0.125, dgp.shift.0.125)
ogm.10.add2 <- rep(list(LR.10), 2)

################################################################################
## p = 2
set.seed(1406)
coefs <- numeric(150)
coefs[1:125] <- ifelse(runif(125) < 0.5, runif(125, -8, -3), runif(125, 3, 8))
coefs[126:150] <- ifelse(runif(25) < 0.5, runif(25, -15, -10), runif(25, 10, 15))
inds <- c(1, 127)
coefs.2 <- coefs[inds] * c(6, -2)
LR.2 <- list(intercept = -1.5, slopes = coefs.2)
class(LR.2) <- "LogRegModel"
# Parametric
dgp.true.param.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                         margins = m.2, norm = TRUE, shift = 0, scale = 1, cor.par = "true")

dgp.shift.neg.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                          margins = m.2, norm = TRUE, shift = -0.5, scale = 1, cor.par = "true")
dgp.shift.neg.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                           margins = m.2, norm = TRUE, shift = -0.25, scale = 1, cor.par = "true")
dgp.shift.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                      margins = m.2, norm = TRUE, shift = 0.5, scale = 1, cor.par = "true")
dgp.shift.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                       margins = m.2, norm = TRUE, shift = 0.25, scale = 1, cor.par = "true")
dgp.shift.neg.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                           margins = m.2, norm = TRUE, shift = -0.125, scale = 1, cor.par = "true")
dgp.shift.0.125 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                       margins = m.2, norm = TRUE, shift = 0.125, scale = 1, cor.par = "true")

dgp.scale.0.25 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                       margins = m.2, norm = TRUE, shift = 0, scale = 0.25, cor.par = "true")
dgp.scale.0.5 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                      margins = m.2, norm = TRUE, shift = 0, scale = 0.5, cor.par = "true")
dgp.scale.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                    margins = m.2, norm = TRUE, shift = 0, scale = 2, cor.par = "true")
dgp.scale.4 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                    margins = m.2, norm = TRUE, shift = 0, scale = 4, cor.par = "true")
dgp.scale.0.75 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                       margins = m.2, norm = TRUE, shift = 0, scale = 0.75, cor.par = "true")
dgp.scale.4.3 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.2, 
                      margins = m.2, norm = TRUE, shift = 0, scale = 4/3, cor.par = "true")


dgp.corr.neg.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2[inds, inds], 
                         margins = m.2, norm = TRUE, shift = 0, scale = 1, cor.par = -0.2)
dgp.corr.neg.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.1[inds, inds], 
                         margins = m.2, norm = TRUE, shift = 0, scale = 1, cor.par = -0.1)
dgp.corr.no.corr <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.no.cor[inds, inds], 
                         margins = m.2, norm = TRUE, shift = 0, scale = 1, cor.par = 0)
dgp.corr.0.1 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.neg.0.2[inds, inds], 
                     margins = m.2, norm = TRUE, shift = 0, scale = 1, cor.par = 0.1)
dgp.corr.0.2 <- list(sim.type = "parametric", n = 100, adjusted.corr = adj.corr.0.2[inds, inds], 
                     margins = m.2, norm = TRUE, shift = 0, scale = 1, cor.par = 0.2)

m.sn <- paste0("c(", paste0(rep("dist$Normal(0, 1)", 2), collapse = ", "), ")")
dgp.sn <- list(sim.type = "parametric", n = 100, adjusted.corr = diag(2), 
               margins = m.sn, norm = TRUE, shift = 0, scale = 1, cor.par = "sn")

param.dgp.2 <- c(c(rep(list(dgp.true.param.2), 6), list(dgp.shift.neg.0.5, dgp.shift.neg.0.25, 
                                                        dgp.shift.0.5, dgp.shift.0.25, dgp.scale.0.25, dgp.scale.0.5, 
                                                        dgp.scale.2, dgp.scale.4, dgp.corr.neg.0.2, dgp.corr.neg.0.1, 
                                                        dgp.corr.no.corr, dgp.corr.0.1, dgp.corr.0.2, dgp.sn)), # deviations dgp 
                 rep(list(dgp.true.param.2), 3)) # deviations ogm
ogm.lr <- list(intercept = 0, slopes = rep(0, 2))
class(ogm.lr) <- "LogRegModel"
param.ogm.2 <- rep(list(LR.2), 23)

# Plasmode
dgp.m.out.of.n <- list(sim.type = "Plasmode", prop = 0.632, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr.2, margins = m.2, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.n.out.of.n <- list(sim.type = "Plasmode", prop = 1, replace = TRUE, 
                       n = 100, adjusted.corr = adj.corr.2, margins = m.2, 
                       norm = TRUE, shift = 0, scale = 1)
dgp.subsampling <- list(sim.type = "Plasmode", prop = 0.632, replace = FALSE, 
                        n = 100, adjusted.corr = adj.corr.2, margins = m.2, 
                        norm = TRUE, shift = 0, scale = 1)
dgp.no.resampling <- list(sim.type = "Plasmode", prop = 1, replace = FALSE, 
                          n = 100, adjusted.corr = adj.corr.2, margins = m.2, 
                          norm = TRUE, shift = 0, scale = 1)
plasmode.dgp.2 <- rep(list(dgp.m.out.of.n, dgp.n.out.of.n, dgp.subsampling, dgp.no.resampling), each = 4)
plasmode.ogm.2 <- rep(list(LR.2), 16)

dgp.2 <- c(param.dgp.2, plasmode.dgp.2)
ogm.2 <- c(param.ogm.2, plasmode.ogm.2)

dgp.2.add <- list(dgp.scale.0.75, dgp.scale.4.3)
ogm.2.add <- rep(list(LR.2), 2)

dgp.2.add2 <- list(dgp.shift.neg.0.125, dgp.shift.0.125)
ogm.2.add2 <- rep(list(LR.2), 2)

################################################################################
# Ridge
at.ridge <- lrn("classif.cv_glmnet", relax = FALSE, alpha = 0, predict_type = "prob", 
                nfolds = 5, nlambda = 100)

# LASSO
at.lasso <- lrn("classif.cv_glmnet", relax = FALSE, alpha = 1, predict_type = "prob", 
                nfolds = 5, nlambda = 100)

# Random Forest 
search_space.rf <- ps(
  mtry.ratio = p_dbl(lower = 0, upper = 1),
  replace = p_lgl(default = TRUE),
  sample.fraction = p_dbl(lower = 0.1, upper = 1), 
  num.trees = p_int(lower = 1, upper = 2000, default = 500)
)

at.rf <- AutoTuner$new(
  learner = lrn("classif.ranger", predict_type = "prob"),
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.acc"),
  search_space = search_space.rf,
  terminator = trm("evals", n_evals = 100),
  tuner = tnr("random_search")
)

search_space.svm = ps(
  cost = p_dbl(-12, 12, trafo = function(x) 2^x),
  gamma = p_dbl(-12, 12, trafo = function(x) 2^x), 
  kernel = p_fct(c("radial", "sigmoid"))
)
at.svm <- AutoTuner$new(
  learner = lrn("classif.svm", predict_type = "prob", type = "C-classification",
                kernel = "radial"),
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.acc"),
  search_space = search_space.svm,
  terminator = trm("evals", n_evals = 100),
  tuner = tnr("random_search")
)

# k-NN
search_space.knn <- ps(
  k = p_dbl(lower = log(1), upper = log(30), trafo = function(x) floor(exp(x))), 
  distance = p_dbl(lower = 1, upper = 5), 
  kernel = p_fct(c("rectangular", "optimal", "epanechnikov", "biweight", "triweight",
                   "cos", "inv", "gaussian", "rank"))
)
at.knn <- AutoTuner$new(
  learner = lrn("classif.kknn", predict_type = "prob"),
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.acc"),
  search_space = search_space.knn,
  terminator = trm("evals", n_evals = 100),
  tuner = tnr("random_search")
)



at.ridge$fallback = lrn("classif.featureless", predict_type = "prob")
at.lasso$fallback = lrn("classif.featureless", predict_type = "prob")
at.svm$fallback = lrn("classif.featureless", predict_type = "prob")
at.rf$fallback = lrn("classif.featureless", predict_type = "prob")
at.knn$fallback = lrn("classif.featureless", predict_type = "prob")

mdls <- c(at.ridge, at.lasso, at.svm, at.rf, at.knn)
msrs <- c("classif.acc", "classif.my_auc", "classif.bbrier", 
          "classif.my_F1", "classif.my_sensitivity", "classif.my_specificity")

################################################################################
runtimes.150 <- numeric(length(factor.logit))
runtimes.150[sapply(dgp.150, "[[", "sim.type") == "parametric"] <- 24 * 3600
runtimes.150[sapply(dgp.150, "[[", "sim.type") == "Plasmode" & 
               sapply(dgp.150, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 1)] <- 23 * 3600 
runtimes.150[sapply(dgp.150, "[[", "sim.type") == "Plasmode" & 
               sapply(dgp.150, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 0.632)] <- 20 * 3600 

runtimes.50 <- numeric(length(factor.logit))
runtimes.50[sapply(dgp.50, "[[", "sim.type") == "parametric"] <- 16 * 3600
runtimes.50[sapply(dgp.50, "[[", "sim.type") == "Plasmode" & 
              sapply(dgp.50, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 1)] <- 16 * 3600 
runtimes.50[sapply(dgp.50, "[[", "sim.type") == "Plasmode" & 
              sapply(dgp.50, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 0.632)] <- 15 * 3600 

runtimes.10 <- numeric(length(factor.logit))
runtimes.10[sapply(dgp.10, "[[", "sim.type") == "parametric"] <- 16 * 3600
runtimes.10[sapply(dgp.10, "[[", "sim.type") == "Plasmode" & 
              sapply(dgp.10, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 1)] <- 16 * 3600 
runtimes.10[sapply(dgp.10, "[[", "sim.type") == "Plasmode" & 
              sapply(dgp.10, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 0.632)] <- 15 * 3600 

runtimes.2 <- numeric(length(factor.logit))
runtimes.2[sapply(dgp.2, "[[", "sim.type") == "parametric"] <- 16 * 3600
runtimes.2[sapply(dgp.2, "[[", "sim.type") == "Plasmode" & 
             sapply(dgp.2, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 1)] <- 16 * 3600 
runtimes.2[sapply(dgp.2, "[[", "sim.type") == "Plasmode" & 
             sapply(dgp.2, function(x) !is.null(x[["prop"]]) && x[["prop"]]  == 0.632)] <- 15 * 3600 

################################################################################
save(dgp.50, ogm.50, factor.logit, n.iter, mdls, msrs, runtimes.50, dgp.50.add, 
     ogm.50.add, factor.logit.add, n.iter.add, dgp.50.add2, ogm.50.add2, 
     factor.logit.add2, n.iter.add2,
     file = "../Results/scenarios_p=50_n=100.RData")
save(dgp.150, ogm.150, factor.logit, n.iter, mdls, msrs, runtimes.150, dgp.150.add, 
     ogm.150.add, factor.logit.add, n.iter.add, dgp.150.add2, ogm.150.add2, 
     factor.logit.add2, n.iter.add2,
     file = "../Results/scenarios_p=150_n=100.RData")
save(dgp.10, ogm.10, factor.logit, n.iter, mdls, msrs, runtimes.10, dgp.10.add, 
     ogm.10.add, factor.logit.add, n.iter.add, dgp.10.add2, 
     ogm.10.add2, factor.logit.add2, n.iter.add2,
     file = "../Results/scenarios_p=10_n=100.RData")
save(dgp.2, ogm.2, factor.logit, n.iter, mdls, msrs, runtimes.2, dgp.2.add, 
     ogm.2.add, factor.logit.add, n.iter.add, dgp.2.add2, 
     ogm.2.add2, factor.logit.add2, n.iter.add2,
     file = "../Results/scenarios_p=2_n=100.RData")

################################################################################
