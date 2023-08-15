# Title: Experiment 1
#
# Description:
#   We aim to investigate if the estimates converge to the true parameter as
#   the sample size increases
#
# NOTE: The number of trials is reduced from 100 to 10 for quicker run.


library(siestim)
library(doSNOW)
library(parallel)
library(dplyr)
library(tcltk)



## ***** Configuration setup for no.1 *****
## ****************************************
M <- 5 #number of trials; changed from 100 to 5 for quick run.
N <- c(50, 100) #sample sizes are changed from c(1e2, 5e2, 1e3, 5e3)
mu <- c(2, 6, 10)
sigma <- 1.5
pi <- c(.3, .6, .9)
w <- .7
#



## ***** Create a function to perform the experiment *****
## *******************************************************
experiment1 <- function(par, sample_size = N, ntrial = M){
  # generate seed number
  set.seed(123)
  seed <- sample(1e3:1e4, length(sample_size))
  
  
  myest <- foreach(i=1:length(sample_size),
                   .combine = bind_rows,
                   .packages = c("dplyr", "siestim", "parallel")) %do% {
                     #generate data
                     set.seed(seed[i])
                     m <- rbinom(ntrial, sample_size[i], par[4])
                     x <- lapply(1:ntrial, function(j) c(rcgg(m[j],par[1],par[2],par[3]), rfgd(sample_size[i]-m[j],par[1],par[2])))
                     
                     
                     #config the optim bounds and init values
                     lb <- c(max(.5,par[1]-4), max(.5,par[2]-4), max(.01,par[3]-.4), max(.01,par[4]-.4))
                     ub <- c(par[1:2]+4, min(1,par[3]+.4), min(1,par[4]+.4))
                     
                     
                     #perform simulations
                     res <- siestim(x, par, lb, ub, list(ncore = detectCores()))
                     res$record$logN <- log(sample_size[i],10)
                     res$record$tmu <- par[1]
                     res$record$tsigma <- par[2]
                     res$record$tpi <- par[3]
                     res$record$tw <- par[4]
                     res$record
                   }
  
  return(myest)
}
#



## ***** Perform simulations ******
## ********************************
pb <- tkProgressBar("progress", "progress", max=length(mu)*length(pi))
n <- 0

df <- NULL
for(mean in mu){
  for(p in pi){
    tmp <- experiment1(c(mean,sigma,p,w))
    df <- bind_rows(df, tmp)
    
    n <- n+1
    info <- paste(round(n/(length(mu)*length(pi)) *100, 0), "% completed")
    setTkProgressBar(pb, n, label = info)
  }
}

View(df)
