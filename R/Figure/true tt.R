# Create the true transmission tree

source("./R/SIR Simulation/simOutbreak.R")
source("./R/SIR Simulation/createTC.R")

library(ggplot2)


# ***************************************
# parameters
gamma <- c(4.5,2) # Generation time
p <- .6 # sampling proportion
epsilon <- 17 # genomic distance cutoff



# ****************************************
# Generate SIR outbreak
set.seed(1)
outbreak <- simOutbreak(1000, gamma, R0=2)



# ************************************
# create transmission cloud
mytc <- createTC(outbreak, p, epsilon)



plot(mytc$tt,
     thin = F,
     x_axis = "inf.times",
     node_color = "group",
     node_value = 1,
     col_pal = c(unsampled = "black", sampled = "gold"),
     arrow_size = .5,
     node_size = 5,
     edge_width = .5,
     node_width = .5,
     height = 800,
     width = 600,
     label = F)

