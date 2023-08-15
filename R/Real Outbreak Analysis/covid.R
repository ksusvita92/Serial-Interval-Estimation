# Title: COVID-19 Analysis
#
# Description:
#   We aim to estimate the serial interval distributions of 3 data sets.
#
# Requirement:


library(siestim)
library(dplyr)




## ***** Load all covid data *****
## *******************************
load("./Data/si_covid.rda")
#



## ***** Setup *******
## *******************
par0 <- c(4, 2, .7, .5) # initial parameter value for optimization
low0 <- c(.5,.5,0,0)
upp0 <- c(14, 10, 1, 1)
ncore <- parallel::detectCores()
#



## ***** Estimate parameters ******
## ********************************
# covid tianjin, CN, 2020
ctia <- siestim(covid$tianjin, par0, low0, upp0, list(ncore = ncore))
ctia$record$data <- "Tianjin, CN"


# covid singapore, 2020
csin <- siestim(covid$singapore, par0, low0, upp0, list(ncore = ncore))
csin$record$data <- "Singapore"


# covid skorea, 2021
ckor <- siestim(covid$south_korea_omicron, par0, low0, upp0)
ckor$record$data <- "South Korea"
#


covid_res <- list(tianjin = ctia, singapore = csin, skorea = ckor)
#save(covid_res, file = "./results/outbreak analysis/covid_res.rda")


