library(ggplot2)
library(ggh4x)
library(rankdist)

plotError <- function(res, measure, deviation.type, lvls, ogm) {
  res <- res[grepl(deviation.type, res$scenario) | grepl("True", res$scenario), ]
  res <- res[res$ogm == ogm, ]
  res$scenario <- factor(res$scenario, levels = lvls)
  res$prop[is.na(res$prop)] <- ""
  res$type.plasmode[is.na(res$type.plasmode)] <- ""
  res$type <- gsub("p", "P", res$type)
  res$classifier <- factor(gsub("Random Forest", "RF", res$classifier), 
                           levels = c("Ridge", "LASSO", "SVM", 
                                      "KNN", "RF"))
  ggplot(data = res, mapping = aes(x = scenario, y = get(paste0("Error.", measure)))) +
    facet_nested(classifier ~ type + type.plasmode + prop, scales = "free_x", 
                 space = "free_x") + 
    geom_boxplot() + theme_bw() + geom_hline(yintercept = 0, colour = "darkgrey") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16), 
          text = element_text(size = 17), 
          title = element_text(size = 15),
          axis.title = element_text(size = 15), 
          axis.text = element_text(size = 15), 
          strip.background = element_rect(fill="white"), 
          strip.text = element_text(size = 15), 
          plot.margin = if(grepl("OGM", deviation.type)) unit(c(0.3, 0.3, 0.3, 1.3), "cm") 
          else unit(c(0.3, 0.3, 0.3, 0.3), "cm")) + 
    xlab("Scenario") + 
    ylab(if(measure == "F1.Score") bquote(Error~"in"~Estimating~F[1]~Score) else paste0("Error in Estimating ", gsub("\\.", " ", measure))) # + 
}


plotKendall <- function(res, measure, deviation.type, lvls, ogm) {
  res <- res[grepl(deviation.type, res$scenario) | grepl("True", res$scenario), ]
  res <- res[res$ogm == ogm, ]
  res$scenario <- factor(res$scenario, levels = lvls)
  res$prop[is.na(res$prop)] <- ""
  res$type.plasmode[is.na(res$type.plasmode)] <- ""
  res$type <- gsub("p", "P", res$type)
  res <- res[, c("ogm", "n", "p", "iter", "type", "prop", "replace", "shift", "scale", 
                 "cor", "factor.logit", "scenario", 
                 grep("Kendall", colnames(res), value = TRUE), "type.plasmode")]
  res <- unique(res)
  ggplot(data = res, mapping = aes(x = scenario, y = get(paste0("Kendall.Distance.", measure)))) +
    facet_nested(. ~ type + type.plasmode + prop, scales = "free_x", 
                 space = "free_x") + 
    geom_boxplot() + theme_bw() + geom_hline(yintercept = 0, colour = "darkgrey") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16), 
          text = element_text(size = 17), 
          title = element_text(size = 15),
          axis.title = element_text(size = 15), 
          axis.text = element_text(size = 15), 
          strip.background = element_rect(fill="white"), 
          strip.text = element_text(size = 15), 
          plot.margin = if(grepl("OGM", deviation.type)) unit(c(0.3, 0.3, 0.3, 1.3), "cm") 
          else unit(c(0.3, 0.3, 0.3, 0.3), "cm")) + 
    xlab("Scenario") + 
    ylab(if(measure == "F1.Score") bquote(Kendall~Distance~of~Ranking~w.r.t.~F[1]~Score) 
         else paste0("Kendall Distance of Ranking w.r.t. ", gsub("\\.", " ", measure)))
}

plotCompPlasmode <- function(res, measure, ogm) {
  res <- res[grepl("True", res$scenario), ]
  res <- res[res$ogm == ogm, ]
  res$prop[is.na(res$prop)] <- ""
  res$type.plasmode[is.na(res$type.plasmode)] <- ""
  res$type <- gsub("p", "P", res$type)
  res$classifier <- factor(gsub("Random Forest", "RF", res$classifier), 
                           levels = c("Ridge", "LASSO", "SVM", 
                                      "KNN", "RF"))
  ggplot(data = res, mapping = aes(x = scenario, y = get(paste0("Error.", measure)))) +
    facet_nested(classifier ~ type + type.plasmode + prop, scales = "free_x", 
                 space = "free_x") + 
    geom_boxplot() + theme_bw() + geom_hline(yintercept = 0, colour = "darkgrey") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16), 
          text = element_text(size = 17), 
          title = element_text(size = 15),
          axis.title = element_text(size = 15), 
          axis.text = element_text(size = 15), 
          strip.background = element_rect(fill="white"), 
          strip.text = element_text(size = 15)) + 
    xlab("Scenario") +
    ylab(if(measure == "F1.Score") bquote(Error~"in"~Estimating~F[1]~Score) else 
      paste0("Error in Estimating ", gsub("\\.", " ", measure)))
}
