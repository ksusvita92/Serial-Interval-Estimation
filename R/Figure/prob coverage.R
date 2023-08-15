# Get confidence interval coverage
library(dplyr)
library(ggplot2)
library(patchwork)



# Load data
exp1_cov <- readxl::read_xlsx("./Result/Distribution-based Simulation.xlsx", sheet = "ci_coverage_exp1")


levels <- as.vector(t(outer(c(2,6,10), c(.3,.6,.9), paste, sep = ";")))
means <- rep(c(2,6,10), each = 3)
pis <- rep(c(.3,.6,.9), 3)


# mu coverage
mu_cov <- exp1_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(as.factor(round(N)), cov_mu, col = scenario)) +
  geom_line(aes(as.factor(round(N)), cov_mu, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(as.factor(round(N)), mu, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("N") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(mu)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))


# sigma coverage
sigma_cov <- exp1_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(as.factor(round(N)), cov_sigma, col = scenario)) +
  geom_line(aes(as.factor(round(N)), cov_sigma, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(as.factor(round(N)), sigma, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("N") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(sigma)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))


# pi coverage
pi_cov <- exp1_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(as.factor(round(N)), cov_pi, col = scenario)) +
  geom_line(aes(as.factor(round(N)), cov_pi, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(as.factor(round(N)), pi, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("N") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(pi)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))


# w coverage
w_cov <- exp1_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(as.factor(round(N)), cov_w, col = scenario)) +
  geom_line(aes(as.factor(round(N)), cov_w, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(as.factor(round(N)), w, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("N") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(w)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))



(mu_cov + sigma_cov + pi_cov + w_cov) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")




# Load data
exp2_cov <- readxl::read_xlsx("./Result/Distribution-based Simulation.xlsx", sheet = "ci_coverage_exp2")


levels <- as.vector(t(outer(c(2,6,10), c(.3,.6,.9), paste, sep = ";")))
means <- rep(c(2,6,10), each = 3)
pis <- rep(c(.3,.6,.9), 3)


# mu coverage
mu_cov <- exp2_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(tw, cov_mu, col = scenario)) +
  geom_line(aes(tw, cov_mu, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(tw, mu, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("w") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(mu)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))


# sigma coverage
sigma_cov <- exp2_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(tw, cov_sigma, col = scenario)) +
  geom_line(aes(tw, cov_sigma, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(tw, sigma, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("w") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(sigma)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))


# pi coverage
pi_cov <- exp2_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(tw, cov_pi, col = scenario)) +
  geom_line(aes(tw, cov_pi, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(tw, pi, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("w") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(pi)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))


# w coverage
w_cov <- exp2_cov %>%
  mutate(scenario = factor(paste(tmu, tpi, sep = ";"), levels = levels)) %>%
  ggplot() +
  geom_point(aes(tw, cov_w, col = scenario)) +
  geom_line(aes(tw, cov_w, group = scenario, col = scenario), alpha = .4, lty = "dashed") +
  #geom_bar(aes(tw, w, fill = scenario), stat = "identity", position = "fill") +
  geom_hline(aes(yintercept = .9), alpha = .4) +
  ylim(c(0,1)) + xlab("w") + ylab("") +
  ggtitle(expression(paste("Coverage probability of ", hat(w)))) +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_discrete(labels = lapply(1:9, function(i) {
    bquote(mu == .(means[i]) ~";"~ pi == .(pis[i]))
  }))



(mu_cov + sigma_cov + pi_cov + w_cov) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")
