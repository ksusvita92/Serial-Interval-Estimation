# Get transmission type

getTType <- function(case1, case2, graph, outbreak){
  
  # find the shortest path
  path <- igraph::shortest_paths(graph, case1, case2)
  path <- labels(path$vpath[[1]])
  
  
  
  # find the mrca
  # mrca must be the one having the earliest infectiousness time in the path
  if(length(path) > 0){
    mrca <- outbreak$epidata %>%
      filter(inf.ID %in% path) %>%
      filter(inf.times == inf.times[which.min(inf.times)]) %>%
      pull(inf.ID)
  } else{
    mrca <- outbreak$epidata %>%
      filter(inf.ID %in% c(case1, case2)) %>%
      filter(inf.times == inf.times[which.min(inf.times)]) %>%
      pull(inf.ID)
  }
  
  
  # get the transmission type
  k <- length(path)-2
  if(!(mrca %in% c(case1, case2))) {
    if(k == 1){
      type <- "coprimary"
      M <- NA
    } else {
      type <- "neither"
      M <- NA
    }
  } else {
    type <- "non-coprimary"
    M <- k
  }
  
  
  # output
  return(data.frame(from = case1, to = case2, type = type, M = M))
}