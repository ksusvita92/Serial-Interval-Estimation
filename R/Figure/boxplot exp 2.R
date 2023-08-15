# Simulation result for the second experiment
# Create boxplot of the difference between estimates and parameters


library(dplyr)
library(siestim)
library(ggplot2)
library(patchwork)


exp2dt <- readxl::read_xlsx("./Result/Distribution-based Simulation.xlsx", sheet = "experiment_2") %>% select(-c("msg"))
exp2dt$scene. <- paste(exp2dt$tmu, exp2dt$tpi, sep = ";")
exp2dt$scene. <- factor(exp2dt$scene., as.vector(t(outer(unique(exp2dt$tmu),
                                                         unique(exp2dt$tpi),
                                                         paste, sep=";"))))
#



# **** Plot the estimation accuracy
# *********************************
data <- exp2dt %>% mutate(mu = mu-tmu, sigma = sigma-tsigma, pi = pi-tpi, w = w-tw)

# create boxplots
bpplt <- lapply(1:4, function(i) {
  par <- c("mu","sigma","pi","w")
  ylab <- expression(hat(mu)-mu, hat(sigma)-sigma, hat(pi)-pi, hat(w)-w)
  
  means <- rep(c(2,6,10), each = 3) #for tick label
  pis <- rep(c(".3",".6",".9"), 3)
  
  dt <- data %>% mutate(est = data[[par[i]]]) %>%
    group_by(scene., tw) %>%
    ungroup()
  
  plt <- ggplot(dt) +
    geom_boxplot(aes(x = scene., y = est, fill = as.factor(tw), col = as.factor(tw)), alpha = .4) +
    geom_hline(aes(yintercept = 0), lty = "dashed") +
    theme_light() + coord_flip() + ylab(ylab[i]) + xlab("") +
    scale_x_discrete(labels = lapply(1:9, function(i) {
      bquote(atop(mu == .(means[i])~ "; ", pi == .(pis[i])))
    }))
  
  if(i != 1) {
    plt <- plt + theme(legend.position = "top",
                       legend.title = element_blank(),
                       axis.text.y = element_blank(),
                       axis.text.x = element_text(size = 12),
                       axis.title.x = element_text(size = 12))
  } else {
    plt <- plt + theme(legend.position = "top",
                       legend.title = element_blank(),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 12))
  }
  
  plt
})

(bpplt[[1]]+bpplt[[2]]+bpplt[[3]]+bpplt[[4]]) +
  plot_layout(guides = "collect", nrow = 1) &
  theme(legend.position = "bottom") &
  guides(guide_legend(nrow = 1))
#