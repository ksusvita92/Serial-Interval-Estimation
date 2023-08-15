# Title: MERS Analysis
#
# Description:
#   We aim to estimate the serial interval distributions of 1 data set.
#


library(siestim)
library(dplyr)




## ***** Load all covid data *****
## *******************************
load("./data/si_mers.rda")
#



## ***** Setup *******
## *******************
par0 <- c(10, 1, .7, .5) # initial parameter value for optimization
low0 <- rep(0, 4)
upp0 <- c(30, 10, 1, 1)
ncore <- parallel::detectCores()
#



## ***** Estimate parameters ******
## ********************************
# mers, South Korea, 2015
mkor <- siestim(mers$skorea, par0, low0, upp0, list(ncore = ncore))
mkor$record$data <- "South Korea"


mers_res <- list(skorea = mkor)
#save(mers_res, file = "./results/outbreak analysis/mers_res.rda")



