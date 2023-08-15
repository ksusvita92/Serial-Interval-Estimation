# Re-analysis of H1N1 data in Quebec
#
# Description:
#   Some outliers are detected in the data which makes the estimation biased.
#   Thus, this script is to re-analyze the data without the outlier.


library(dplyr)
library(siestim)



## **** Analyze the data *****
## ***************************
load("./data/si_h1n1.rda")
cf <- 2 #coef to detemine outlier

outlier <- boxplot.stats(h1n1$quebec, cf)
dt <- h1n1$quebec[h1n1$quebec != outlier$out] #data without outlier

newest <- siestim(dt, c(5,2,.7,.5), c(1,1,0,0), c(15,15,1,1))

h1n1_res2 <- list(quebec2 = newest)
#save(h1n1_res2, file = "./results/outbreak analysis/h1n1_res2.rda")