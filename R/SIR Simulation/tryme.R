source("./R/SIR Simulation/simOutbreak.R")
source("R/SIR Simulation/createTC.R")
source("R/SIR Simulation/refPars.R")



library(siestim)



# ***************************************
# parameters
gamma <- c(4.5,2) # Generation time
p <- .7 # sampling proportion
epsilon <- 15 # genomic distance cutoff
N <- 100 # no. of sampled transmission trees
iv <- c(5,1,.5,.5) # initial values for siestim
lb <- rep(0,4) # lower bound for siestim
ub <- c(10,5,1,1) # upper bound for siestim



# ****************************************
# Generate SIR outbreak
set.seed(1)
outbreak <- simOutbreak(1000, gamma, R0=2)



# ************************************
# create transmission cloud
mytc <- createTC(outbreak, p, epsilon)



# number of transmission types
table(mytc$tc$type)
length(
  mytc$tc %>%
    filter(!is.na(si)) %>%
    pull(inf.ID) %>%
    unique()
)

# number of plausible transmission paths
npaths <- mytc$tc %>%
  group_by(inf.ID) %>%
  filter(!is.na(si)) %>%
  summarise(ntp = n()) %>%
  ungroup()
npaths$class <- ifelse(npaths$ntp == 1, "1",
                       ifelse(npaths$ntp <= 4, "2-4",
                              ifelse(npaths$ntp <= 10, "5-10", "more")))
npaths %>%
  group_by(class) %>%
  summarise(freq = n()) %>%
  mutate(p = round(freq/sum(freq), 4)) %>%
  pull(freq)



# ***********************************
# Implement siestim
## data
dt <- lapply(1:N, function(i){
  mytc$tc %>%
    group_by(inf.ID) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    filter(!is.na(si))
})


# **********************************
# Get reference estimate of pi and w
mypiw <- refPars(dt)


mysi <- lapply(1:N, function(i){
  dt[[i]] %>% pull(si)
})
myest <- siestim(mysi, iv, lb, ub, list(ncore=detectCores()-1))
myest$par
myest$se
mypiw
getci(myest)[,1]
getci(myest)[,2]
write.csv(myest$record, "xx.csv", row.names = F)

