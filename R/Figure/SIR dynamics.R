# Create the dynamics of the SIR simulation

source("./R/SIR Simulation/simOutbreak.R")


library(ggplot2)


# ***************************************
# parameters
gamma <- c(4.5,2) # Generation time
p <- .1 # sampling proportion
epsilon <- 88 # genomic distance cutoff



# ****************************************
# Generate SIR outbreak
set.seed(1)
outbreak <- simOutbreak(1000, gamma, R0=2)

# plot the dynamic
ggplot(outbreak$dynam %>% mutate(t = 0:100)) +
  geom_line(aes(t, nsus, col = "0")) +
  geom_line(aes(t, ninf, col = "1")) +
  geom_line(aes(t, nrec, col = "2")) +
  scale_color_manual(values = c("0" = "darkgreen",
                                "1" = "red",
                                "2" = "darkblue"),
                     labels = c("0" = "Susceptible",
                                "1" = "Infected",
                                "2" = "Recovered"),
                     name = "") +
  labs(x = "days", y = "count") +
  theme_light() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


