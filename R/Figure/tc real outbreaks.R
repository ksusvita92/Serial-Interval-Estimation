# Make transmission networks for COVID-19 in Singapore & Tianjin, and MERS
# in South Korea


library(visNetwork)
library(dplyr)



## **** Load the data from references ******
file_nm <- list.files("./Data/", "net")
for(nm in file_nm) load(paste("./Data", nm, sep = "/"))

rm(file_nm, nm)
#



## ***** Create visNetwork data ******
theclouds <- setNames(vector("list",3), c("singapore", "tianjin", "south_korea"))

# create transmission network for covid-19 in singapore
covid_net$singapore$edges$arrows <- "to"
covid_net$singapore$edges$length <- covid_net$singapore$edges$length + 15
covid_net$singapore$nodes <- select(covid_net$singapore$nodes, id, group, font.size)
covid_net$singapore$nodes <- covid_net$singapore$nodes %>%
  filter(id %in% c(covid_net$singapore$edges$from,covid_net$singapore$edges$to))

theclouds[[1]] <- visNetwork(covid_net$singapore$nodes, covid_net$singapore$edges, width = "100%", height = "500px") %>%
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical")) %>%
  visNodes(size = 25) %>%
  visEdges(width = 5, color = "black") %>%
  visLegend() %>%
  visLayout(randomSeed = 123456)

# create transmission network for covid-19 in tianjin
covid_net$tianjin$edges$arrows <- "to"
covid_net$tianjin$edges$length <- covid_net$tianjin$edges$length + 20
covid_net$tianjin$nodes <- select(covid_net$tianjin$nodes, id, group, font.size)
covid_net$tianjin$nodes <- covid_net$tianjin$nodes %>%
  filter(id %in% c(covid_net$tianjin$edges$from,covid_net$tianjin$edges$to))

theclouds[[2]] <- visNetwork(covid_net$tianjin$nodes, covid_net$tianjin$edges, width = "100%", height = "500px") %>%
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical")) %>%
  visNodes(size = 25) %>%
  visEdges(width = 5, color = "black") %>%
  visLegend() %>%
  visLayout(randomSeed = 123456)

# create transmission network for mers in south korea
mers_net$south_korea$edges <- mers_net$south_korea$edges %>%
  filter(!is.na(length))
mers_net$south_korea$edges$arrows <- "to"
mers_net$south_korea$edges$length <- mers_net$south_korea$edges$length + 20
mers_net$south_korea$nodes <- select(mers_net$south_korea$nodes, id, group, font.size)
mers_net$south_korea$nodes <- mers_net$south_korea$nodes %>%
  filter(id %in% c(mers_net$south_korea$edges$from, mers_net$south_korea$edges$to))

theclouds[[3]] <- visNetwork(mers_net$south_korea$nodes, mers_net$south_korea$edges %>% filter(!is.na(length)), width = "100%", height = "500px") %>%
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical")) %>%
  visNodes(size = 25) %>%
  visEdges(width = 5, color = "black") %>%
  visLegend() %>%
  visLayout(randomSeed = 1)


theclouds