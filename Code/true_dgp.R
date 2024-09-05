library(bigsimr)
bs <- bigsimr::bigsimr_setup()
dist <- bigsimr::distributions_setup()

m <- read.csv2("../Results/true_dgp_margins.csv", header = FALSE)
m <- strsplit(m[, 1], "___")
m <- sapply(m, function(x) gsub("\\(\\n", "\\(", x))
m <- sapply(m, function(x) gsub("\\n\\)\\n", "\\)", x))
m <- sapply(m, function(x) gsub("\\n", ", ", x))
m <- sapply(m, function(x) gsub(": ", " = ", x, fixed = TRUE))
m <- sapply(m, function(x) gsub("Categorical\\{Float64, Vector\\{Float64\\}\\}", "dist$Categorical", x))
m <- sapply(m, function(x) gsub("\\{.*\\}", "", x))
m <- sapply(m, function(x) gsub("\\[", "c\\(", x))
m <- sapply(m, function(x) gsub("\\]", "\\)", x))
m <- sapply(m, function(x) gsub("support=Base.OneTo(2), ", "", x, fixed = TRUE))
m <- sapply(m, function(x) gsub("K = 2, ", "", x, fixed = TRUE))
m <- sapply(m, function(x) gsub("[[:alpha:]]+[[:blank:]]?\\=[[:blank:]]?", "", x))
m <- as.vector(unlist(m))
m <- m[-1]
m.50 <- m[c(1:15, 51:65, 101:110, 126:135)]
m.10 <- m[c(1:3, 51:53, 101:102, 126:127)]
m.2 <- m[c(1, 127)]
m <- paste0("dist$", m)
m <- paste0("c(", paste0(m, collapse = ", "), ")")
m.50 <- paste0("dist$", m.50)
m.50 <- paste0("c(", paste0(m.50, collapse = ", "), ")")
m.10 <- paste0("dist$", m.10)
m.10 <- paste0("c(", paste0(m.10, collapse = ", "), ")")
m.2 <- paste0("dist$", m.2)
m.2 <- paste0("c(", paste0(m.2, collapse = ", "), ")")
margins <- eval(parse(text = m))
margins.50 <- eval(parse(text = m.50))
margins.10 <- eval(parse(text = m.10))
margins.2 <- eval(parse(text = m.2))

adj.corr <- read.csv("../Results/true_dgp_adj_cor.csv")
adj.corr <- as.matrix(adj.corr)
dimnames(adj.corr) <- NULL

adj.corr.50 <- adj.corr[c(1:15, 51:65, 101:110, 126:135), c(1:15, 51:65, 101:110, 126:135)]
adj.corr.10 <- adj.corr[c(1:3, 51:53, 101:102, 126:127), c(1:3, 51:53, 101:102, 126:127)]
adj.corr.2 <- adj.corr[c(1, 127), c(1, 127)]

save(adj.corr, adj.corr.50, adj.corr.10, adj.corr.2, m, m.50, m.10, m.2, file = "../Results/true_dgp_new.RData")


