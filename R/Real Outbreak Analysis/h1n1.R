# Title: Swine Flu Analysis
#
# Description:
#   We aim to estimate the serial interval distributions of 3 data sets.
#


library(siestim)
library(dplyr)




## ***** Load all swine data *****
## *******************************
load("./data/si_h1n1.rda")
#



## ***** Setup *******
## *******************
par0 <- c(5, 2, .7, .5) # initial parameter value for optimization
low0 <- c(1,1,0,0)
upp0 <- c(15, 15, 1, 1)
#



## ***** Estimate parameters ******
## ********************************
# h1n1 quebec, 2009
hque <- siestim(h1n1$quebec, par0, low0, upp0)
hque$record$data <- "Quebec, CA"


# h1n1 san antonio, 2009
htex <- siestim(h1n1$texas, par0, low0, upp0)
htex$record$data <- "Texas, US"


# h1n1 SA, 2009
hsan <- siestim(h1n1$south_africa, par0, low0, upp0)
hsan$record$data <- "South Africa"
#


h1n1_res <- list(quebec = hque, texas = htex, safrica = hsan)
#save(h1n1_res, file = "./results/outbreak analysis/h1n1_res.rda")

