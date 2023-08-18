# This script has a fixed dcgg function. The purpose is to run experiment 2
#
# NOTE: The number of trials is reduced from 100 to 5 for quicker run.
# The sample size is reduced from 5000 to 100 for quicker run.

library(dplyr)
library(siestim)
library(doSNOW)
library(dfoptim)
library(tcltk)




## run experiments
N <- 5 #number of trials; changed from 100 to 5 for quick run.
m <- 100 #sample size; changed from 5000 to 100 for quick run.
w <- c(.01,.05,.1,.3,.5,.7,.9,.95,.99)

set.seed(1)
seeds <- sample(1e3:1e4, N)


# estimate the parameters
pb <- txtProgressBar(max = N*length(w), title = "Simulation", style = 3)
opt <- function(n) setTxtProgressBar(pb, n)
cl <- makeCluster(parallel::detectCores(), type = "SOCK")
registerDoSNOW(cl)

res <- foreach(j=1:length(w), .combine = bind_rows) %:%
  foreach(i=1:N,
          .packages = c("siestim","dfoptim","dplyr"),
          .options.snow = list(progress = opt),
          .combine = bind_rows) %dopar% {
            
            wl <- max(1e-3,w[j]-.4)
            wu <- min(1,w[j]+.4)
            wi <- .5*(wu-wl) + wl
            
            
            # sample data
            set.seed(seeds[i])
            n <- rbinom(1, m, w[j])
            x11 <- c(rcgg(n, 2, 1.5, .3), rfgd(m-n, 2, 1.5))
            x12 <- c(rcgg(n, 2, 1.5, .6), rfgd(m-n, 2, 1.5))
            x13 <- c(rcgg(n, 2, 1.5, .9), rfgd(m-n, 2, 1.5))
            x21 <- c(rcgg(n, 6, 1.5, .3), rfgd(m-n, 6, 1.5))
            x22 <- c(rcgg(n, 6, 1.5, .6), rfgd(m-n, 6, 1.5))
            x23 <- c(rcgg(n, 6, 1.5, .9), rfgd(m-n, 6, 1.5))
            x31 <- c(rcgg(n, 10, 1.5, .3), rfgd(m-n, 10, 1.5))
            x32 <- c(rcgg(n, 10, 1.5, .6), rfgd(m-n, 10, 1.5))
            x33 <- c(rcgg(n, 10, 1.5, .9), rfgd(m-n, 10, 1.5))
            
            
            # estimate
            myest11 <- tryCatch(siestim(x11, c(2,2,.2,wi), c(.5,.5,.01,wl), c(6,5.5,.7,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest11 <- myest11 %>% bind_cols(as.data.frame(as.list(c(2,1.5,.3,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            myest12 <- tryCatch(siestim(x12, c(2,2,.6,wi), c(.5,.5,.2,wl), c(6,5.5,1,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest12 <- myest12 %>% bind_cols(as.data.frame(as.list(c(2,1.5,.6,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            myest13 <- tryCatch(siestim(x13, c(2,2,.75,wi), c(.5,.5,.5,wl), c(6,5.5,1,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest13 <- myest13 %>% bind_cols(as.data.frame(as.list(c(2,1.5,.9,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            
            myest21 <- tryCatch(siestim(x21, c(6,3,0.41,wi), c(2.5,.5,.01,wl), c(10,5.5,0.8,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest21 <- myest21 %>% bind_cols(as.data.frame(as.list(c(6,1.5,.3,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            myest22 <- tryCatch(siestim(x22, c(6,3,0.6,wi), c(2,.5,.2,wl), c(10,5.5,1,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest22 <- myest22 %>% bind_cols(as.data.frame(as.list(c(6,1.5,.6,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            myest23 <- tryCatch(siestim(x23, c(6,3,0.75,wi), c(2,.5,.5,wl), c(10,5.5,1,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest23 <- myest23 %>% bind_cols(as.data.frame(as.list(c(6,1.5,.9,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            
            myest31 <- tryCatch(siestim(x31, c(10,3,0.41,wi), c(6,.5,.01,wl), c(14,5.5,.8,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest31 <- myest31 %>% bind_cols(as.data.frame(as.list(c(10,1.5,.3,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            myest32 <- tryCatch(siestim(x32, c(10,3,0.6,wi), c(6,.5,.2,wl), c(14,5.5,1,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest32 <- myest32 %>% bind_cols(as.data.frame(as.list(c(10,1.5,.6,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            myest33 <- tryCatch(siestim(x33, c(10,3,0.75,wi), c(6,.5,.5,wl), c(14,5.5,1,wu), control = list(maximize = T))$record,
                                error = function(e) data.frame(mu=NA,sigma=NA,pi=NA,w=NA,msg=conditionMessage(e)))
            myest33 <- myest33 %>% bind_cols(as.data.frame(as.list(c(10,1.5,.9,w[j])), col.names = c("tmu","tsigma","tpi","tw")))
            
            
            bind_rows(myest11,myest12,myest13,myest21,myest22,myest23,myest31,myest32,myest33)
          }

stopCluster(cl)


View(res)
#write.csv(res, "./BUG fixed/res/result_exp.csv", row.names = F)