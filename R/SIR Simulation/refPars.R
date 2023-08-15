# get reference estimation of pi and w given tree

library(doSNOW)
library(parallel)
library(foreach)



refPars <- function(trees){
  
  # estimate pi and w for each transmission tree
  cl <- makeCluster(detectCores()-1, "SOCK")
  registerDoSNOW(cl)
  myest <- foreach(i = 1:N,
                   .errorhandling = "pass",
                   .combine = rbind,
                   .packages = c("dplyr")) %dopar% {
                     print(i)
                     x <- trees[[i]] %>% filter(!is.na(M)) %>% pull(M)
                     pi <- optim(.5, function(x, p){
                       if(p > 1) p <- 1
                       if(p <= 0) p <- 1e-3
                       fx <- dgeom(x, p)
                       fx[fx == 0 | !is.finite(fx)] <- 1e-300
                       nll <- -sum(log(fx))
                     }, method = "L-BFGS-B", lower = 1e-3, upper = 1, x = x)$par
                     
                     x <- trees[[i]] %>% pull(type)
                     x <- table(x)
                     print(x)
                     w <- x[2]/sum(x)
                     
                     data.frame(pi = pi, w = w)
                   }
  stopCluster(cl)
  
  
  # output
  c(pi = mean(myest$pi, na.rm = T), w = mean(myest$w, na.rm = T))
}