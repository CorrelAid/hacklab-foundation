install.packages("igraph")
install.packages("graphlayouts")
# install.packages("ggraph")
library(tidyverse)
library(ggraph)
library(tidygraph)
library(igraph)
library(graphlayouts)
# library(network)

skills <- rio::import("./data/clean/skills_final.csv") %>% 
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

#adding a new column with the category
languages = c("C", "C#", "C++", "HTML/CSS", "Java", "JavaScript", 
              "Kotlin", "PHP", "Python", "Ruby", "SQL", "Swift", "TypeScript")

platforms = c("Android", "Arduino", "AWS", "Docker", "Google Cloud Platform", 
              "Heroku", "IBM Cloud or Watson", "iOS", "Linux", "MacOS", 
              "Microsoft Azure", "Raspberry Pi", "Windows", "WordPress")

web_frameworks = c("Angular", "ASP.NET", "Django", "Express", "jQuery", 
                   "Lavarel", "React.js", "Ruby on Rails", "Vue.js", 
                   "Spring")

web_frameworks_frontend = c(".NET", ".NET Core", "Flutter", "Node.js",
                            "Puppet", "React Native", "Deno.js")


nodes_new <- nodes %>%
  mutate(
    category = ifelse(is.element(tool, languages), "language", 
                      ifelse(is.element(tool, platforms), "platforms", 
                             ifelse(is.element(tool, web_frameworks), 
                                    "web_frameworks_backend",
                                    ifelse(is.element(tool, web_frameworks_frontend), 
                                           "web_frameworks_frontend", "other")))
    )) %>%
  filter(weight > 1)



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
#  distinct(source_target, .keep_all = TRUE) %>% # keep_all to keep all columns
  # IT WORKS! woot woot!
  # we can now keep only what we need: source and target:
  select(source, target)
  # FIN.
  


# hard thing to do now: find a nice combination of technology (ggraph or other),
# layout, arguments of the layout function, and how to label nicely...
# possibly, thinking "theoretically" what the best layout would be, is a good idea? or too hard?
# Maybe we should remove all non-programming languages/technologies
# (e.g. Slack, Asana, Windows, GitHub, Trello, AI, Internet-of-Things, ML/AI, ...)
# they clutter the graph and are not really interesting? 

####Plotting the network ----

#Only the interesting tools
nodes_filter <- nodes_new %>%
  filter(category != "other") %>%
  filter(weight > 1)

edges_filter <- edges %>%
  filter(is.element(source, languages) |
           is.element(source, platforms) | 
           is.element(source, web_frameworks) |
           is.element(source, web_frameworks_frontend)) %>%
  filter(is.element(target, languages) |
           is.element(target, platforms) |
           is.element(target, web_frameworks) |
           is.element(target, web_frameworks_frontend)) %>%
  group_by(source, target) %>%
  count() %>%
  filter(source != target) %>%
  ##Including a threshold for connection strength
  filter(n > 20)

#Excluding the tools that no longer have any connections
nodes_filter <- nodes_filter %>%
  filter(is.element(tool, edges_filter$target) |
           is.element(tool, edges_filter$source)) 

got_palette <- c("#E04B4F", "#B36C80", "#858EB1", "#58AFE2")

graph <- as_tbl_graph(edges_filter, directed = FALSE, vertices = nodes_filter) 
# create the layout for the graph:
set.seed(100)
lay <- ggraph::create_layout(graph, layout = "dh") %>%
  left_join(nodes, by = c("name" = "tool"))

#dn layout seems to be the best standard layout
ggnet <- ggraph(lay) + 
  geom_edge_link(alpha = 0.05) + 
  geom_node_point(
    aes(size = weight, color = nodes_filter$category), 
    shape = 19,
    show.legend = TRUE) +
  geom_node_text(aes(label = name), repel = TRUE) + # remove the labels if want to plot faster (comment this line)
  theme_graph() + 
  scale_color_manual(name = "Category", values = got_palette, 
                     labels = c("Languages", "Platforms", "Web Frameworks Backend", 
                                "Web Frameworks Frontend")) +
  scale_size_continuous(name = "Number of respondents") +
  scale_edge_width(range = c(0.2,3))
ggnet

ggsave("dh_threshold.png", plot = ggnet, width = 12, height = 8)

#### Some ideas from tutorials ----
## Backbone layout
graph_simple <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
is_simple(graph_simple)

net_backbone <- ggraph(graph_simple,layout = "backbone")+
  geom_edge_link(alpha = 0.10,edge_colour = "black")+
  geom_node_point(aes(color=nodes_filter$category,size=nodes_filter$weight),shape = 19)+
  geom_node_text(aes(label = nodes_filter$tool), repel = TRUE)+
  scale_edge_width_continuous(range = c(0.2,0.9)) + 
  scale_size_continuous(name = "Number of respondents") +
  scale_color_manual(name = "Category", values = got_palette, 
                     labels = c("Languages", "Platforms", "Web Frameworks Backend", 
                                "Web Frameworks Frontend"))+
  coord_fixed()+
  theme_graph() +
  theme(legend.position = "right")

net_backbone

ggsave(filename = "backbone_threshold.png", plot = net_backbone, width = 12, height = 6)

### Just for comparison: 
##Plots without threshold ----

nodes_filter <- nodes_new %>%
  filter(category != "other") %>%
  filter(weight > 1)

edges_filter_without_threshold <- edges %>%
  filter(is.element(source, languages) |
           is.element(source, platforms) | 
           is.element(source, web_frameworks) |
           is.element(source, web_frameworks_frontend)) %>%
  filter(is.element(target, languages) |
           is.element(target, platforms) |
           is.element(target, web_frameworks) |
           is.element(target, web_frameworks_frontend)) %>%
  group_by(source, target) %>%
  count() %>%
  filter(source != target)

#Excluding the tools that no longer have any connections
nodes_filter <- nodes_filter %>%
  filter(is.element(tool, edges_filter_without_threshold$target) |
           is.element(tool, edges_filter_without_threshold$source)) 

##to do: Use hacklab foundation colors!
graph <- as_tbl_graph(edges_filter_without_threshold, directed = FALSE, vertices = nodes_filter) 
# create the layout for the graph:
lay <- ggraph::create_layout(graph, layout = "dh") %>%
  left_join(nodes_filter, by = c("name" = "tool"))

#dn layout seems to be the best standard layout
dh <- ggraph(lay) + 
  geom_edge_link(alpha = 0.05) + 
  geom_node_point(
    aes(size = weight, color = nodes_filter$category), 
    shape = 19,
    show.legend = T) +
  geom_node_text(aes(label = name), repel = TRUE) + # remove the labels if want to plot faster (comment this line)
  theme_graph() + 
  scale_size_continuous(name = "Number of respondents") +
  scale_color_manual(name = "Category", values = got_palette, 
                     labels = c("Languages", "Platforms", "Web Frameworks Backend", 
                                "Web Frameworks Frontend"))+
  scale_edge_width(range = c(0.2,3))
dh

ggsave("dh_all.png", plot = dh, width = 12, height = 8)


##Backbone layout
graph_simple <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
is_simple(graph_simple)

backbone_all <- ggraph(graph_simple,layout = "backbone")+
  geom_edge_link(alpha = 0.1,edge_colour = "black")+
  geom_node_point(aes(color=nodes_filter$category,size=nodes_filter$weight),shape = 19)+
  geom_node_text(aes(label = nodes_filter$tool), repel = TRUE)+
  scale_edge_width_continuous(range = c(0.2,0.9)) +
  scale_size_continuous(name = "Number of respondents") +
  scale_color_manual(name = "Category", values = got_palette, 
                     labels = c("Languages", "Platforms", "Web Frameworks Backend", 
                                "Web Frameworks Frontend"))+
  coord_fixed()+
  theme_graph()+
  theme(legend.position = "right")
backbone_all

ggsave("backbone_all.png", plot = backbone_all, width = 14, height = 8)
