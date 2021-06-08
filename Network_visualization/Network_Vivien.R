# install.packages("igraph")
# install.packages("ggraph")
library(tidyverse)
library(ggraph)
library(tidygraph)
# library(igraph)
# library(network)

skills <- rio::import("../data/clean/skills_final.csv") %>% 
  select(-V1) %>%  # artifact due to a save as .csv to remove (at least on a Mac?)
  mutate_all(na_if,"") %>%
  drop_na(level) %>%
  filter(level != "Want to work with NEXT year") %>%
  select(-level) %>%
  mutate( # group those
    tool = ifelse(tool == "HTML", "HTML/CSS", tool),
    tool = ifelse(tool == "CSS", "HTML/CSS", tool),
  )


#Creating a node list, id = tool id 
# I DO NOT THINK THAT THIS IS VERY USEFUL IN THE END
# the code just after does the job.
# nodes <- skills %>%
#   distinct(tool) %>%
#   rename(label = tool) %>%
#   drop_na(label) %>%
#   rowid_to_column("id")
# the number of times each tool is mentioned is the weight of the node
# it will determine the size of the bubbles
nodes <- skills %>%  
  group_by(tool) %>%
  summarise(weight = n()) 

# EDGES LIST:
edges <- skills %>%
  # we group by respondent
  group_by(id) %>%
  # we add a new column with all the tools used by the respondents
  # we just paste all of them together, separated by a ";"
  # it works because we grouped by id
  mutate(target = paste(tool, collapse = ';')) %>%
  # we do not need to continue working on the grouped dataframe:
  ungroup() %>%
  # then we create a new row for all the combinations observed in the data,
  # by splitting the column added above:
  separate_rows(target, sep = ';') %>%
  # not sure why but sometimes they are duplicates, did respondents mention twice the same tool? probably.
  # we don't want duplicates, we just want a table with all the observed combinations
  unique() %>%
  # just to make it clear:
  select(resp_id = id, source = tool, target) %>%
  # we do not want "self-references", link from a tool to itself,
  # so we remove cases where source == target:
  filter(source != target) %>%
  # for each respondent, we have the link (edges) twice, in both directions
  # we do not want this (e.g. source=HTML/CSS, target=JavaScript + source=JavaScript, target=HTML/CSS)
  # we want to count only one link for these!
  # first we sort the data (not really useful, just easier to see what's going on:)
  arrange(resp_id, source, target) %>%
  # the next operation (mutate) will be performed row by row (and not by column)
  # more info: https://dplyr.tidyverse.org/articles/rowwise.html
  rowwise() %>%
  # what we want to do is "remove unordered combinations".
  # we add a new column which is the concatenation/contraction of the source and target (paste),
  # but here is the trick: we sort the source and target alphabetically (str_sort).
  # (inspired by this: https://stackoverflow.com/questions/32329315/duplicate-combination-of-values-in-columns?rq=1)
  # Voilà, now we can identify the duplicates!
  mutate(
    source_target = paste(str_sort(c(source, target)), collapse = "_")
  ) %>%
  # we stop the rowwise:
  ungroup() %>%
  # we remove all the rows with duplicates "source_target":
  distinct(source_target, .keep_all = TRUE) %>% # keep_all to keep all columns
  # IT WORKS! woot woot!
  # we can now keep only what we need: source and target:
  select(source, target)
  # FIN.
  

# (following a tutorial with ggraph here, you could certainly do it differently!)
# create graph from edges, add nodes info:
graph <- as_tbl_graph(edges, directed = FALSE, vertices = nodes) 
# create the layout for the graph:
lay <- ggraph::create_layout(graph, layout = "auto") %>%
  # we add the weight to the nodes:
  left_join(nodes, by = c("name" = "tool"))

ggnet <- ggraph(lay) + 
  geom_edge_link(alpha = 0.05) + 
  geom_node_point(
    aes(size = weight),
    color = 'black',
    show.legend = FALSE) +
  geom_node_label(aes(label = name), repel = TRUE) +
  theme_minimal()
ggnet # the layout is bad, not great to see "clusters"

# here is a webpage where to check for some of the possible layouts:
#   https://igraph.org/c/doc/igraph-Layout.html#two-d-layout-generators
# and : the animation somewhere after the middle of this page https://www.data-imaginist.com/2017/ggraph-introduction-layouts
# and the list here: https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html
# ==> kk, fr, graphopt, dh, mds...

# test layout kk:
lay <- ggraph::create_layout(graph, layout = "kk") %>%
  left_join(nodes, by = c("name" = "tool"))
ggnet <- ggraph(lay) + 
  geom_edge_link(alpha = 0.05) + 
  geom_node_point(
    aes(size = weight),
    color = 'black',
    show.legend = FALSE) +
  geom_node_label(aes(label = name), repel = TRUE) + # remove the labels if want to plot faster (comment this line)
  theme_minimal()
ggnet # a bit better, but not great neither.

# ALREADY TESTED: 
#   - lgl is not good, too crowded.
#   - fr is very bad, large cluster in the middle
#   - graphopt is not so good, too crowded
#   - mds -> place on a "plane", a bit strange

# test layout dh:
lay <- ggraph::create_layout(graph, layout = "dh") %>%
  left_join(nodes, by = c("name" = "tool"))
ggnet <- ggraph(lay) + 
  geom_edge_link(alpha = 0.05) + 
  geom_node_point(
    aes(size = weight),
    color = 'black',
    show.legend = FALSE) +
  geom_node_label(aes(label = name), repel = TRUE) + # remove the labels if want to plot faster (comment this line)
  theme_minimal()
ggnet # a bit better we see the structure better. 
# we could try to play with the arguments if we want to improve the layout,
# e.g. more distance between all nodes.
# see https://igraph.org/r/doc/layout_with_dh.html


# hard thing to do now: find a nice combination of technology (ggraph or other),
# layout, arguments of the layout function, and how to label nicely...
# possibly, thinking "theoretically" what the best layout would be, is a good idea? or too hard?
# Maybe we should remove all non-programming languages/technologies
# (e.g. Slack, Asana, Windows, GitHub, Trello, AI, Internet-of-Things, ML/AI, ...)
# they clutter the graph and are not really interesting? 





#### --------------- OLD CODE BELOW

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
