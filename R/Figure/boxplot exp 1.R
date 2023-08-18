# Simulation result from the first experiment
# Create boxplot of the difference between estimate and parameter values


library(dplyr)
library(siestim)
library(ggplot2)
library(patchwork)


# ***** Load simulation result ****
# *********************************
exp1dt <- readxl::read_xlsx("./Result/Distribution-based Simulation.xlsx", sheet = "experiment_1") %>%
  select(-c(contains("se"), "logll", "msg")) %>%
  mutate(scene. = paste(tmu, ";", tpi, sep = ""))
exp1dt$scene. <- factor(exp1dt$scene., as.vector(t(outer(unique(sort(exp1dt$tmu)),
                                                         unique(sort(exp1dt$tpi)),
                                                         paste, sep=";"))))
####



# **** Create boxplot ****
# ************************
data <- exp1dt %>%
  mutate(mu = mu-tmu, sigma = sigma-tsigma, pi = pi-tpi, w = w-tw) %>%
  select(-c(contains("t")))


bplt <- lapply(1:4, function(i) {
  par <- c("mu","sigma","pi","w")
  ylab <- expression(hat(mu)-mu, hat(sigma)-sigma, hat(pi)-pi, hat(w)-w)
  
  means <- rep(c(2,6,10), each = 3) #for tick label
  pis <- rep(c(".3",".6",".9"), 3)
  
  dt <- data %>% mutate(est = data[[par[i]]])
  plt <- ggplot(dt) +
    geom_boxplot(aes(x = scene., y = est, fill = as.factor(N), col = as.factor(N)), alpha = .4) +
    geom_hline(aes(yintercept = 0), lty = "dashed") +
    theme_light() + ylab(ylab[i]) + xlab("") +
    scale_x_discrete(labels = lapply(1:9, function(i) {
      bquote(atop(mu == .(means[i])~ "; ", pi == .(pis[i])))
    }))
  
  if(i!=4) {
    plt + theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.text.x = element_blank(),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 12)
    )
  } else {
    plt + theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 12)
    )
  }
})

(bplt[[1]] + bplt[[2]] + bplt[[3]] + bplt[[4]]) +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")
# -------- #