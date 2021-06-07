install.packages("igraph")
install.packages("ggraph")
library(tidyverse)
library(ggraph)
library(igraph)
library(network)

skills <- rio::import("data/clean/skills_final.csv") %>% 
  select(-V1) %>%  # artifact due to a save as .csv to remove (at least on a Mac?)
  mutate_all(na_if,"") %>%
  drop_na(level) %>%
  filter(level != "Want to work with NEXT year") %>%
  select(-level)


#Creating a node list, id = tool id 
nodes <- skills %>%
  distinct(tool) %>%
  rename(label = tool) %>%
  drop_na(label) %>%
  rowid_to_column("id")

weights <- skills %>%  
  group_by(tool) %>%
  summarise(weight = n())  

skills <- skills %>%
  mutate(
    value = 1
  )
skills_transformed <- pivot_wider(data = skills, 
                                  names_from =  tool, 
                                  values_from =  value)

### do not use
  ungroup() %>%
  left_join(nodes, by = c("tool" = "label")) %>% 
  rename(from = id) #%>%
 # left_join(nodes, by = c("tool_2" = "label")) %>% 
  #rename(to = id)

edges <- select(edges, from, to, weight)


library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, 
                                       vertices = nodes, 
                                       directed = FALSE)
plot(routes_igraph, edge.arrow.size = 0.2)




skill_count <- skills %>% group_by(tool) %>% count()
nodes <- skills %>%
  left_join(skill_count, by = 'tool') %>%
  select(tool, weight = n) %>%
  unique()
# build edge list:
edges <- skills %>%
  mutate(to = paste(nodes$tool, collapse = ',')) %>%
  separate_rows(to, sep = ',') %>%
  select(from = tool, to) %>%
  filter(from != to) # we are only interested in drawing edges between cities
# create graph from edges, add nodes info:
graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes$tool)
graph
