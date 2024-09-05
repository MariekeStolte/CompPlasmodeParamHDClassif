setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=150_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_150_100", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.150 <- allSimStudies(n.iter = n.iter, dgp = dgp.150, 
                         factor.logit = factor.logit, ogm = ogm.150, 
                         mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                         registry = reg, run.times = runtimes.150, 
                         max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.150 <- do.call(rbind, res)
save(res.150, file = "HD-simulation-paper/Results/results_p=150_n=100_2024_07_09.RData")


################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=50_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_50_100", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.50 <- allSimStudies(n.iter = n.iter, dgp = dgp.50, 
                        factor.logit = factor.logit, ogm = ogm.50, 
                        mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                        registry = reg, run.times = runtimes.50, 
                        max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.50 <- do.call(rbind, res)
save(res.50, file = "HD-simulation-paper/Results/results_p=50_n=100_2024_07_09.RData")

################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=10_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_10_100", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.10 <- allSimStudies(n.iter = n.iter, dgp = dgp.10, 
                        factor.logit = factor.logit, ogm = ogm.10, 
                        mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                        registry = reg, run.times = runtimes.10, 
                        max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.10 <- do.call(rbind, res)
save(res.10, file = "HD-simulation-paper/Results/results_p=10_n=100_2024_07_09.RData")


################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=2_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_2_100_07_24", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.2 <- allSimStudies(n.iter = n.iter, dgp = dgp.2, 
                       factor.logit = factor.logit, ogm = ogm.2, 
                       mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                       registry = reg, run.times = runtimes.2, 
                       max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.2 <- do.call(rbind, res)
save(res.2, file = "HD-simulation-paper/Results/results_p=2_n=100_2024_07_24.RData")

################################################################################
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=150_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_150_100_add", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.150.add <- allSimStudies(n.iter = n.iter.add, dgp = dgp.150.add, 
                             factor.logit = factor.logit.add, ogm = ogm.150.add, 
                             mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                             registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                             max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.150.add <- do.call(rbind, res)
save(res.150.add, file = "HD-simulation-paper/Results/results_p=150_n=100_add_2024_07_24.RData")


################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=50_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_50_100_add", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.50.add <- allSimStudies(n.iter = n.iter.add, dgp = dgp.50.add, 
                            factor.logit = factor.logit.add, ogm = ogm.50.add, 
                            mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                            registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                            max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.50.add <- do.call(rbind, res)
save(res.50.add, file = "HD-simulation-paper/Results/results_p=50_n=100_add_2024_07_24.RData")


################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=10_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_10_100_add", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.10.add <- allSimStudies(n.iter = n.iter.add, dgp = dgp.10.add, 
                            factor.logit = factor.logit.add, ogm = ogm.10.add, 
                            mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                            registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                            max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.10.add <- do.call(rbind, res)
save(res.10.add, file = "HD-simulation-paper/Results/results_p=10_n=100_add_2024_07_24.RData")


################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=2_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_2_100_add", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.2.add <- allSimStudies(n.iter = n.iter.add, dgp = dgp.2.add, 
                           factor.logit = factor.logit.add, ogm = ogm.2.add, 
                           mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                           registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                           max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.2.add <- do.call(rbind, res)
save(res.2.add, file = "HD-simulation-paper/Results/results_p=2_n=100_add_2024_07_24.RData")

################################################################################
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=150_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_150_100_add2", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.150.add2 <- allSimStudies(n.iter = n.iter.add2, dgp = dgp.150.add2, 
                              factor.logit = factor.logit.add2, ogm = ogm.150.add2, 
                              mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                              registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                              max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.150.add2 <- do.call(rbind, res)
save(res.150.add2, file = "HD-simulation-paper/Results/results_p=150_n=100_add2_2024_07_30.RData")


################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=50_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_50_100_add2", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.50.add2 <- allSimStudies(n.iter = n.iter.add2, dgp = dgp.50.add2, 
                             factor.logit = factor.logit.add2, ogm = ogm.50.add2, 
                             mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                             registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                             max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.50.add2 <- do.call(rbind, res)
save(res.50.add2, file = "HD-simulation-paper/Results/results_p=50_n=100_add2_2024_07_30.RData")

################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=10_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_10_100_add2", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.10.add2 <- allSimStudies(n.iter = n.iter.add2, dgp = dgp.10.add2, 
                             factor.logit = factor.logit.add2, ogm = ogm.10.add2, 
                             mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                             registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                             max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.10.add2 <- do.call(rbind, res)
save(res.10.add2, file = "HD-simulation-paper/Results/results_p=10_n=100_add2_2024_07_30.RData")


################################################################################
rm(list = ls());gc()
setwd("HD-simulation-paper/Code") # on cluster
source("simulation_functions_new.R")
load("../Results/scenarios_p=2_n=100.RData", verbose = TRUE)

reg <- makeRegistry("../Results/registry_2_100_add2", seed = 2024,
                    source = "simulation_functions_new.R", 
                    conf.file = "../../batchtools.conf.R") # on cluster
res.2.add2 <- allSimStudies(n.iter = n.iter.add2, dgp = dgp.2.add2, 
                            factor.logit = factor.logit.add2, ogm = ogm.2.add2, 
                            mdls = mdls, msrs = msrs, resamp = rsmp("cv", folds = 5), 
                            registry = reg, run.times = rep(2 * 24 * 3600, 2), 
                            max.time = 2 * 24 * 3600, mem = 1024)
res <- reduceResultsList(reg = reg)
res.2.add2 <- do.call(rbind, res)
save(res.2.add2, file = "HD-simulation-paper/Results/results_p=2_n=100_add2_2024_07_30.RData")

