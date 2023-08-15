# Simulation result from the first experiment
# Create obs. serial interval and true serial interval distributions
# from the estimates, and then compared them to the  true densities



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




mu <- 6; sigma <- 1.5; pi <- .6; w <- .7 # pick mu=2,6,10, pi=.3,.6,.9
sc <- paste(mu, pi, sep = ";")
# Note: To create Figures S1 & S2, pick different mu and pi, and then combine
# using patchwork

# create the estimated obs. serial interval distribution
osi <- lapply(c(1e2,5e2,1e3,5e3), function(n) {
  dt <- exp1dt %>% filter(N == n, scene. == sc)
  
  plt <- ggplot()
  for(j in 1:100) {
    m <- dt$mu[j]; s <- dt$sigma[j]; p <- dt$pi[j]; w <- dt$w[j]
    x <- seq(0,20,length.out = 500)
    xx <- data.frame(x = x, fx = w*dcgg(x,m,s,p)+(1-w)*dfgd(x,m,s))
    
    plt <- plt + geom_line(aes(x,fx), xx, col = "gray")
  }
  
  m <- dt$tmu[1]; s <- 1.5; p <- dt$tpi[1]; w <- .7
  xx <- data.frame(x = x, fx = w*dcgg(x,m,s,p)+(1-w)*dfgd(x,m,s))
  plt <- plt + geom_line(aes(x,fx), xx, col = "black") + theme_light() +
    labs(title = paste("N", n, sep = "="), subtitle = "Observed serial interval")
  plt
})

# create the estimated serial interval distribution
tsi <- lapply(c(1e2,5e2,1e3,5e3), function(n) {
  dt <- exp1dt %>% filter(N == n, scene. == sc)
  
  plt <- ggplot()
  for(j in 1:100) {
    m <- dt$mu[j]; s <- dt$sigma[j]; p <- dt$pi[j]; w <- dt$w[j]
    a <- (m/s)^2; b <- m/s^2
    x <- seq(0,20,length.out = 500)
    xx <- data.frame(x = x, fx = dgamma(x,a,b))
    
    plt <- plt + geom_line(aes(x,fx), xx, col = "gray")
  }
  
  m <- dt$tmu[1]; s <- 1.5; p <- dt$tpi[1]; w <- .7
  a <- (m/s)^2; b <- m/s^2
  xx <- data.frame(x = x, fx = dgamma(x,a,b))
  plt <- plt + geom_line(aes(x,fx), xx, col = "black") + theme_light() +
    labs(subtitle = "True serial interval")
  plt
})

# combine all
(osi[[1]] + tsi[[1]] + osi[[2]] + tsi[[2]] +
    osi[[3]] + tsi[[3]] + osi[[4]] + tsi[[4]]) +
  plot_layout(nrow = 2, ncol = 4) &
  theme(axis.title = element_blank())
