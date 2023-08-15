# Title: Measles Analysis
#
# Description:
#   We aim to estimate the serial interval distributions of 3 data sets.
#


library(siestim)
library(dplyr)




## ***** Load all measles data *****
## *******************************
load("./data/si_measles.rda")
#



## ***** Setup *******
## *******************
par0 <- c(12, 3, .7, .5) # initial parameter value for optimization
low0 <- c(5, 0, 0, 0)
upp0 <- c(20, 10, 1, 1)
#



## ***** Estimate parameters ******
## ********************************
# measles Germany, 1861
mger <- siestim(measles$hagelloch, par0, low0, upp0)
mger$record$data <- "Hagelloch, DE"


# measles Japan, 2017
mjap <- siestim(measles$japan, par0, low0, upp0)
mjap$record$data <- "Japan"
#


measles_res <- list(hagelloch = mger, japan = mjap)
#save(measles_res, file = "./results/outbreak analysis/measles_res.rda")


