# Create illustrative figure for the CGG distribution with various values
# of pi's.


library(siestim)
library(ggplot2)
library(dplyr)
library(patchwork)


# +++++++++++++++++++++++++++
par <- c(6, 1) # mu, sigma;
pi <- seq(.1, 1, .1) # sequence of pi


x <- seq(0, 25, length.out = 100)
fx <- sapply(pi, function(p) dcgg(x, par[1], par[2], p)) # pdf of cgg for each pi
px <- sapply(pi, function(p) pcgg(x, par[1], par[2], p)) # cdf of cgg for each pi

dt <- data.frame(x = rep(x, length(pi)),
                 fx = as.vector(fx), px = as.vector(px),
                 pi = rep(pi, each = length(x)))


# ++++++++++++++++++++++++++=
plt <- ggplot() +
  geom_line(aes(x = x, y = fx, group = pi, col = as.factor(pi)), dt) +
  geom_line(aes(x = x, y = dgamma(x, (par[1]/par[2])^2, par[1]/par[2]^2)), lty = "dashed") +
  labs(y = "density", x = "") + theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(lwd = 1.5), title = expression(pi)))

plt2 <- ggplot() +
  geom_line(aes(x = x, y = px, group = pi, col = as.factor(pi)), dt) +
  geom_line(aes(x = x, y = pgamma(x, (par[1]/par[2])^2, par[1]/par[2]^2)), lty = "dashed") +
  labs(y = "cumulative distribution", x = "") + theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(lwd = 1.5), title = expression(pi)))

(plt+plt2) + plot_layout(nrow = 1, guides = "collect") &
  theme(legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

