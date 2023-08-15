# Create transmission cloud from downsampling cases


source("R/SIR Simulation/getTType.R")

library(dplyr)
library(foreach)
library(parallel)
library(doSNOW)

createTC <- function(outbreak,
                     downsampling_prob,
                     cutoff,
                     dna_model = "N",
                     seed = 1){
  
  set.seed(seed)
  # downsampling the ttrue transmission tree
  unsampled <- outbreak$epidata %>%
    slice_sample(prop = 1-downsampling_prob) %>%
    pull(inf.ID) #get unsampled cases' name
  sampled <- outbreak$epidata %>%
    filter(!(inf.ID %in% unsampled)) %>%
    pull(inf.ID)
  
  
  # find SNP distance
  pdist <- ape::dist.dna(outbreak$aligndata[sampled], dna_model, as.matrix = T)
  
  
  # infectee whose infector is sampled
  newepi <- outbreak$epidata %>%
    filter(!(inf.ID %in% unsampled), !(inf.source %in% unsampled), !is.na(si)) %>%
    select(inf.source, inf.ID, si) %>%
    mutate(type = "non-coprimary", M = 0)
  if(nrow(newepi) > 1) {
    newepi$d <- diag(pdist[newepi$inf.source, newepi$inf.ID])
  } else newepi$d <- pdist[newepi$inf.source, newepi$inf.ID]
  
  
  # find infectee that has unsampled infector
  findInf <- sampled[which(!(sampled %in% newepi$inf.ID))]
  
  
  # create a transmission cloud
  ## find all potential infectors for each intectee whose infector is unsampled
  newtc <- lapply(findInf, function(x){
    infector <- names(which(pdist[,x] <= cutoff))
    infector <- infector[infector != x]
    d <- pdist[infector, x]
    si <- outbreak$epidata$inf.times[outbreak$epidata$inf.ID == x] - outbreak$epidata$inf.times[outbreak$epidata$inf.ID %in% infector]
    
    data.frame(inf.ID = rep(x, length(infector)), inf.source = infector, si = si, d = d) %>%
      filter(si >= 0)
  })
  newtc <- bind_rows(newtc)
  row.names(newtc) <- NULL
  
  ## get the transmission types and exclude the coprimary with intermediaries
  ### define graph to find the transmission path
  gg <- igraph::graph_from_data_frame(outbreak$epidata %>%
                                        select(inf.source, inf.ID, si) %>%
                                        filter(!is.na(si)),
                                      directed = F,
                                      outbreak$epidata %>%
                                        select(inf.ID))
  
  cl <- makeCluster(detectCores()-1, "SOCK")
  registerDoSNOW(cl)
  
  tmp <- foreach(i=1:nrow(newtc),
                 .packages = c("dplyr"),
                 .export = c("getTType"),
                 .combine = bind_rows,
                 .errorhandling = "pass") %dopar% {
                   .GlobalEnv$getTType <- getTType
                   
                   getTType(newtc$inf.source[i], newtc$inf.ID[i], gg, outbreak)
                 }
  stopCluster(cl)
  
  newtc <- newtc %>%
    left_join(tmp, by = c("inf.source"="from", "inf.ID"="to")) #%>%
  #filter(type != "neither") # exclude coprimary with intermediate
  
  ## the transmission cloud
  newepi <- newepi %>% bind_rows(newtc)
  newepi$type <- factor(newepi$type, c("coprimary", "non-coprimary", "neither"))
  
  
  
  # plot true transmission tree
  outbreak$epidata$group <- ifelse(!(outbreak$epidata$inf.ID %in% unsampled), "sampled", "unsampled")
  epicont <- epicontacts::make_epicontacts(outbreak$epidata,
                                           contacts = outbreak$epidata %>%
                                             select(inf.source, inf.ID, nmut) %>%
                                             filter(!is.na(inf.source)),
                                           id = "inf.ID",
                                           from = "inf.source",
                                           to = "inf.ID",
                                           directed = T)
  
  
  
  # output
  list(tt = epicont, tc = newepi)
}