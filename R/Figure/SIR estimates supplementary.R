library(ggplot2)
library(patchwork)
library(dplyr)


# load data
SIR_Results <- readxl::read_excel("./Result/SIR Simulation.xlsx", sheet = "summary_for_figure_Figure S4", range = "A22:Q31")


# define function to create plot
plt <- function(data, trueval, lev = .95){
  names(data) <- c("est", "lb", "ub","p")
  
  # plot
  myplot <- data %>%
    ggplot() +
    geom_errorbar(aes(x = factor(p), ymin = lb, ymax = ub), width = 0, col = "coral") +
    geom_point(aes(x = factor(p), y = est), shape = 4) +
    theme_light() + xlab("p") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12))
  
  if(length(trueval) == 1){
    myplot <- myplot +
      geom_hline(aes(yintercept = trueval), linetype = "dotted")
  } else{
    myplot <- myplot +
      geom_point(aes(x = factor(p), y = trueval), shape = 1, alpha = .6)
  }
  
  return(myplot)
}


mu <- plt(select(SIR_Results, mu, contains("b_mu"), p), SIR_Results$t_mu) +
  ylab(expression(hat(mu)))
sg <- plt(select(SIR_Results, sigma, contains("b_sigma"), p), SIR_Results$t_sigma) +
  ylab(expression(hat(sigma)))
pi <- plt(select(SIR_Results, pi, contains("b_pi"), p), SIR_Results$t_pi) +
  ylab(expression(hat(pi)))
w <- plt(select(SIR_Results, w, contains("b_w"), p), SIR_Results$t_w) +
  ylab("w")

(mu+sg+pi+w) + plot_layout(ncol = 2)

