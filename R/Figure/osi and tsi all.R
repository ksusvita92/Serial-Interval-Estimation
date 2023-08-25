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

# create the estimated obs. serial interval distribution
osi <- function(data) {
  lapply(c(1e2,5e2,1e3,5e3), function(n) {
    dt <- data %>% filter(N == n)
    
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
      annotate("text",
               x = Inf, y = Inf,
               hjust = 1.25, vjust = 1.25,
               label = paste("N", n, sep = "="))
    if(n == 100) plt <- plt + labs(x = "", y = bquote(atop(mu==.(m),pi==.(p))))
    else plt <- plt + labs(x = "", y = "")
    plt
  })
  
}

# create the estimated serial interval distribution
tsi <- function(data) {
  lapply(c(1e2,5e2,1e3,5e3), function(n) {
    dt <- data %>% filter(N == n)
    
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
      annotate("text",
               x = Inf, y = Inf,
               hjust = 1.25, vjust = 1.25,
               label = paste("N", n, sep = "="))
    if(n == 100) plt <- plt + labs(x = "", y = bquote(atop(mu==.(m),pi==.(p))))
    else plt <- plt + labs(x = "", y = "")
    plt
  })
}




####
getosi <- lapply(levels(exp1dt$scene.), function(i) {
  data <- exp1dt %>% filter(scene. == i)
  osi(data)
})

getsi <- lapply(levels(exp1dt$scene.), function(i) {
  data <- exp1dt %>% filter(scene. == i)
  tsi(data)
})
####



(getosi[[1]][[1]] + getosi[[1]][[2]] + getosi[[1]][[3]] + getosi[[1]][[4]] +
    getosi[[2]][[1]] + getosi[[2]][[2]] + getosi[[2]][[3]] + getosi[[2]][[4]] +
    getosi[[3]][[1]] + getosi[[3]][[2]] + getosi[[3]][[3]] + getosi[[3]][[4]] +
    getosi[[4]][[1]] + getosi[[4]][[2]] + getosi[[4]][[3]] + getosi[[4]][[4]] +
    getosi[[5]][[1]] + getosi[[5]][[2]] + getosi[[5]][[3]] + getosi[[5]][[4]] +
    getosi[[6]][[1]] + getosi[[6]][[2]] + getosi[[6]][[3]] + getosi[[6]][[4]] +
    getosi[[7]][[1]] + getosi[[7]][[2]] + getosi[[7]][[3]] + getosi[[7]][[4]] +
    getosi[[8]][[1]] + getosi[[8]][[2]] + getosi[[8]][[3]] + getosi[[8]][[4]] +
    getosi[[9]][[1]] + getosi[[9]][[2]] + getosi[[9]][[3]] + getosi[[9]][[4]]) +
  plot_layout(nrow = 9)


(getsi[[1]][[1]] + getsi[[1]][[2]] + getsi[[1]][[3]] + getsi[[1]][[4]] +
    getsi[[2]][[1]] + getsi[[2]][[2]] + getsi[[2]][[3]] + getsi[[2]][[4]] +
    getsi[[3]][[1]] + getsi[[3]][[2]] + getsi[[3]][[3]] + getsi[[3]][[4]] +
    getsi[[4]][[1]] + getsi[[4]][[2]] + getsi[[4]][[3]] + getsi[[4]][[4]] +
    getsi[[5]][[1]] + getsi[[5]][[2]] + getsi[[5]][[3]] + getsi[[5]][[4]] +
    getsi[[6]][[1]] + getsi[[6]][[2]] + getsi[[6]][[3]] + getsi[[6]][[4]] +
    getsi[[7]][[1]] + getsi[[7]][[2]] + getsi[[7]][[3]] + getsi[[7]][[4]] +
    getsi[[8]][[1]] + getsi[[8]][[2]] + getsi[[8]][[3]] + getsi[[8]][[4]] +
    getsi[[9]][[1]] + getsi[[9]][[2]] + getsi[[9]][[3]] + getsi[[9]][[4]]) +
  plot_layout(nrow = 9)
