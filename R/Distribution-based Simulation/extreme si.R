# Create Supplementary plot of the mixture model and its components' densities
# given extreme values of parameters.



library(siestim)
library(doSNOW)
library(tcltk)
library(dplyr)



par <- c(1, 1.5, .3, .5) # param in order: mu, sigma, pi, w
n <- 100 # how many independent data sets generated?
N <- 1000 # sample size



# **** Estimate the parameters *********
set.seed(12)
seeds <- sample(1e3:1e4, n) # generate seed numbers

pb <- txtProgressBar(max = n, style = 3)
opt <- function(x) setTxtProgressBar(pb, x)
cl <- makeCluster(parallel::detectCores(), type = "SOCK")
registerDoSNOW(cl)

dt <- foreach(i = 1:n,
              .packages = c("siestim", "dplyr"),
              .options.snow = list(progress = opt),
              .combine = bind_rows) %dopar% {
                
                # generate data
                set.seed(seeds[i])
                ncop <- rbinom(1, N, par[4])
                x <- c(rcgg(ncop, par[1], par[2], par[3]), rfgd(N-ncop, par[1], par[2]))
                
                # estimate
                myest <- siestim(x, c(2,2,.75,.5), c(.5,.5,.1,.1), c(6,5,1,1))
                data.frame(mu = myest$par[1],
                           sigma = myest$par[2],
                           pi = myest$par[3],
                           w = myest$par[4])
              }
stopCluster(cl)
#

View(dt)

#write.csv(dt, file = "./results/supp_extreme_example.csv", row.names = F)