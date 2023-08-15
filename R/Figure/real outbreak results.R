# Fit estimated densities of the mixture and components' for
# each outbreak data


library(ggplot2)
library(patchwork)
library(dplyr)
library(siestim)



## ***** Load data and analsysis results *****
## *******************************************
load("./Result/covid_res.rda")
load("./Result/measles_res.rda")
load("./Result/mers_res.rda")
load("./Result/h1n1_res.rda")

load("./Data/si_covid.rda")
load("./Data/si_measles.rda")
load("./Data/si_mers.rda")
load("./Data/si_h1n1.rda")
#



## ***** Define function to create a plot ****
## *******************************************
plotOSI <- function(data, par, diseasenm) {
  dt <- data.frame(x = seq(max(min(data)-2,0), max(data)+2, length.out = 100)) %>%
    mutate(f1 = par[4]*dcgg(x,par[1],par[2],par[3]),
           f2 = (1-par[4])*dfgd(x,par[1],par[2]),
           fx = f1+f2)
  
  ggplot() + geom_histogram(aes(data, ..density..), binwidth = 1, col = "black", fill = "white") +
    xlim(max(min(data)-2,0), max(data)+2) + labs(title = diseasenm, x = "", y = "") +
    geom_line(aes(x, fx), dt, col = "red", lwd = 1) +
    geom_line(aes(x, f1), dt, col = "dark green", lty = "dashed", lwd = 1) +
    geom_line(aes(x, f2), dt, col = "blue", lty = "dashed", lwd = 1) +
    theme_light()
}
#


## ***** Plot covid-19 data ****
## *****************************
nm <- as.list(paste("COVID-19", c("Singapore", "Tianjin, CN", "South Korea"), sep = "; "))
params <- list(covid_res$singapore$par, covid_res$tianjin$par, covid_res$skorea$par)
names(nm) <- names(params) <- names(covid)
covid$singapore <- unlist(covid$singapore)
covid$tianjin <- unlist(covid$tianjin)

plt_cov <- lapply(1:length(covid), function(i) plotOSI(covid[[i]], params[[i]], nm[[i]]))
#



## ***** Plot measles data ****
## ****************************
nm <- as.list(paste("Measles", c("Japan", "Hagelloch, DE"), sep = "; "))
params <- list(measles_res$japan$par, measles_res$hagelloch$par)

plt_mea <- lapply(1:length(measles), function(i) plotOSI(measles[[i]], params[[i]], nm[[i]]))
#


## ***** Plot mers data ****
## *************************
nm <- as.list(paste("MERS", "South Korea", sep = "; "))
params <- list(mers_res$skorea$par)
mers$skorea <- unlist(mers$skorea)

plt_mers <- lapply(1:length(mers), function(i) plotOSI(mers[[i]], params[[i]], nm[[i]]))
#



## ***** Plot h1n1 data ****
## *************************
nm <- as.list(paste("Swine flu", c("South Africa", "Texas, US", "Quebec, CA"), sep = "; "))
params <- list(h1n1_res$safrica$par, h1n1_res$texas$par, h1n1_res$quebec$par)

plt_h1n <- lapply(1:length(h1n1), function(i) plotOSI(h1n1[[i]], params[[i]], nm[[i]]))
#



# combine all plots
cov <- (plt_cov[[1]] + plt_cov[[2]] + plt_cov[[3]]) + plot_layout(nrow = 1)
mea <- (plt_mea[[1]] + plt_mea[[2]] + plot_spacer()) + plot_layout(nrow = 1)
mer <- (plt_mers[[1]] + plot_spacer() + plot_spacer()) + plot_layout(nrow = 1)
h1n <- (plt_h1n[[1]] + plt_h1n[[2]] + plt_h1n[[3]]) + plot_layout(nrow = 1)

cov / mea / mer / h1n


