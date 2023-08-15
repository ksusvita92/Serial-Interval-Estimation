# Compare pdf of FGD, GDD, and Gamma distributions


library(siestim)
library(ggplot2)
library(patchwork)



# ++++++++++++++++++++++++=
# define GDD density:
dgdd <- function(x, m, s) {
  ifelse(x < 0, dfgd(-x, m, s)/2, dfgd(x, m, s)/2)
}
#



# +++++++++++++++++++++++
mu <- 4
sigma <- c(1,4,7)

plt <- lapply(sigma, function(s) {
  x <- seq(-12, 12, length.out = 300)
  a <- (mu/s)^2; b <- mu/s^2
  dt <- data.frame(x = x, gamma = dgamma(x, a, b),
                   fgd = dfgd(x, mu,s),
                   gdd = dgdd(x, mu, s))
  dt$fgd[which(dt$x < .1)] <- NA
  dt$gamma[which(dt$x < .1)] <- NA
  
  dt %>% ggplot() +
    geom_line(aes(x, fgd, linetype = "FGD", col = "FGD")) +
    geom_line(aes(x, gdd, linetype = "GDD", col = "GDD")) +
    geom_line(aes(x, gamma, linetype = "Gamma", col = "Gamma")) +
    annotate("text", x = Inf, y = Inf, vjust = 1.5, hjust = 1.5, label = bquote(sigma == .(s))) +
    scale_linetype_manual(name = "xx", values= c("FGD"="solid", "GDD"="dashed", "Gamma"="dotted")) +
    scale_color_manual(name = "xx", values= c("FGD"="red", "GDD"="dark green", "Gamma"="blue")) +
    labs(x = "", y = "") + theme_light() +
    theme(legend.position = "bottom")
})


(plt[[1]] + plt[[2]] + plt[[3]]) +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10))