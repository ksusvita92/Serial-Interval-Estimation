# Create Supplementary plot of the mixture model and its components' densities
# given extreme values of parameters.



library(dplyr)
library(ggplot2)
library(siestim)
library(patchwork)



exp2 <- readxl::read_xlsx("./Result/Distribution-based Simulation.xlsx", sheet = "experiment_2")


# Define function to create the plots
createplt <- function(data) {
  lapply(sort(unique(data$tpi)), function(prop) {
    par <- c(data$tmu[1], data$tsigma[1], prop, data$tw[1])
    fnc <- fc <- fx <- matrix(0, 100, 100)
    x <- seq(0.5, 40, length.out = 100)
    dt <- data %>% filter(tpi == prop)
    
    for(i in 1:nrow(dt)) {
      m <- dt$mu[i]; s <- dt$sigma[i]; p <- dt$pi[i]; w <- dt$w[i]
      fnc[,i] <- w*dcgg(x,m,s,p)
      fc[,i] <- (1-w)*dfgd(x,m,s)
      fx[,i] <- w*dcgg(x,m,s,p)+(1-w)*dfgd(x,m,s)
    }
    
    
    xx <- data.frame(x = x,
                     fnc_min = apply(fnc, 1, function(x) quantile(x, .025)),
                     fnc_mean = apply(fnc, 1, function(x) mean(x)),
                     fnc_max = apply(fnc, 1, function(x) quantile(x, .975)),
                     fc_min = apply(fc, 1, function(x) quantile(x, .025)),
                     fc_mean = apply(fc, 1, function(x) mean(x)),
                     fc_max = apply(fc, 1, function(x) quantile(x, .975)),
                     fx_min = apply(fx, 1, function(x) quantile(x, .025)),
                     fx_mean = apply(fx, 1, function(x) mean(x)),
                     fx_max = apply(fx, 1, function(x) quantile(x, .975))
    )
    txx <- data.frame(x = x,
                      fnc = par[4]*dcgg(x,par[1],par[2],par[3]),
                      fc = (1-par[4])*dfgd(x,par[1],par[2]),
                      fx = par[4]*dcgg(x,par[1],par[2],par[3])+(1-par[4])*dfgd(x,par[1],par[2]))
    
    
    plt <- ggplot() +
      geom_ribbon(aes(x = x, ymin = fc_min, ymax = fc_max), xx, alpha = .4, fill = "dark green") +
      geom_ribbon(aes(x = x, ymin = fnc_min, ymax = fnc_max), xx, alpha = .4, fill = "blue") +
      geom_ribbon(aes(x = x, ymin = fx_min, ymax = fx_max), xx, alpha = .4, fill = "red") +
      geom_line(aes(x = x, y = fc_mean), xx, lty = "dotted", lwd = 1) +
      geom_line(aes(x = x, y = fnc_mean), xx, lty = "dotted", col = "blue", lwd = 1) +
      geom_line(aes(x = x, y = fx_mean), xx, lty = "dotted", col = "dark green", lwd = 1) +
      theme_light()
    
    plt <- plt +
      geom_line(aes(x = x, y = fc), txx, col = "dark green") +
      geom_line(aes(x = x, y = fnc), txx, col = "blue") +
      geom_line(aes(x = x, y = fx), txx, col = "red") +
      annotate("text", x = Inf, y = Inf, vjust = 1.25, hjust = 1.25,
               label = bquote(pi == .(prop)))
    
    if(prop == .3) plt <- plt + labs(y = "density", x = "")
    else plt <- plt + labs(y = "", x = "")
    plt
    
  })
}


# Create S3b plot
## for mu = 6 days
plt1 <- createplt(filter(exp2, tmu == 6, tw == .5))

## for mu = 10 days
plt2 <- createplt(filter(exp2, tmu == 10, tw == .5))


## combine plots
(plt1[[1]] + plt1[[2]] + plt1[[3]] +
    plt2[[1]] + plt2[[2]] + plt2[[3]]) +
  plot_layout(nrow = 2)

