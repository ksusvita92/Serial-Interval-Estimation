# Create estimated densities of the observed serial intervals from
# ICC interval and our method


library(ggplot2)
library(siestim)


# load data
load("./Data/si_measles.rda")
load("./Result/measles_res.rda")
load("./Result/measles_icc.rda")
source("./R/Real Outbreak Analysis/Hagelloch ICC.R")

icc_par <- c(icc_hag$mu, icc_hag$sigma, 1-icc_hag$w1)
our_par <- measles_res$hagelloch$par
x <- seq(5, 16, length.out = 100)


icc_dt <- getdens(seq(5, 16, 1), icc_par[1:2],
                  c(icc_hag$w1, icc_hag$w2, icc_hag$w3, icc_hag$w4,
                    icc_hag$w5, icc_hag$w6, icc_hag$w6))

plt <- ggplot() +
  geom_histogram(aes(x = measles$hagelloch, y = ..density..),
                 col = "black", fill = "white", binwidth = 1) +
  geom_line(aes(x = x,
                y = our_par[4]*dcgg(x, our_par[1], our_par[2], our_par[3]) +
                  (1-our_par[4])*dfgd(x, our_par[1], our_par[2])),
            col = "red") +
  geom_line(aes(x = x, y = f), icc_dt, col = "blue") +
  labs(x = "", y = "density") + theme_light()
plt
