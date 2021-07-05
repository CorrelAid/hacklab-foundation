
# draw the network of technologies "co-used" together.
# set working directory to this file

library(tidyverse)
library(ggraph)
library(tidygraph)
library(graphlayouts)

skills <- rio::import("../data/clean/skills_final.csv") %>% 
  select(-V1) %>%  # artifact due to a save as .csv to remove (at least on a Mac?)
  mutate_all(na_if,"") %>%
  drop_na(level) %>%
  filter(level != "Want to work with NEXT year") %>%
  select(-level) %>%
  mutate( # group those
    tool = ifelse(tool == "HTML", "HTML/CSS", tool),
    tool = ifelse(tool == "CSS", "HTML/CSS", tool),
    # rename this:
    tool = ifelse(tool == "IBM Cloud or Watson", "IBM Cloud/Watson", tool)
    )


#Creating a node list, id = tool id 
# the number of times each tool is mentioned is the weight of the node
# it will determine the size of the bubbles
nodes <- skills %>%  
  group_by(tool) %>%
  summarise(weight = n()) 

#adding a new column with the category
languages = c("C", "C#", "C++", "HTML/CSS", "Java", "JavaScript", 
              "Kotlin", "PHP", "Python", "Ruby", "SQL", "Swift", "TypeScript")

platforms = c("Android", "Arduino", "AWS", "Docker", "Google Cloud Platform", 
              "Heroku", "IBM Cloud/Watson", "iOS", "Linux", "MacOS", 
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
  count(source_target, name = "weight") %>% # get the weights
  separate(source_target, into = c("source", "target"), sep = "_", ) # re-create source/target
# not used:  
  # we remove all the rows with duplicates "source_target":
  # distinct(source_target, .keep_all = TRUE) %>% # keep_all to keep all columns
  # IT WORKS! woot woot!
  # we can now keep only what we need: source and target:
  # select(source, target)
# FIN.



####Plotting the network ----

#Only the interesting tools
# we apply a filter on the nodes to keep only the tools 
#   that were mentioned by more than 20 respodents.
nodes_filter <- nodes_new %>%
  filter(category != "other") %>%
  filter(weight > 20)

edges_filter <- edges %>%
  filter(source %in% nodes_filter$tool,
         target %in% nodes_filter$tool)

# #Excluding the tools that no longer have any connections
# nodes_filter <- nodes_filter %>%
#   filter(is.element(tool, edges_filter$target) |
#            is.element(tool, edges_filter$source)) 

HL_colors = c("#DF4B4F", "#AF8CDE", "#59B0E3","#6fe38e") # green instead of yellow, easier to see

graph <- as_tbl_graph(edges_filter, directed = FALSE, vertices = nodes_filter) 
# create the layout for the graph:
set.seed(100)
lay <- ggraph::create_layout(graph, layout = "dh") %>%
  left_join(nodes, by = c("name" = "tool"))



set.seed(4)
# net_backbone <- ggraph(graph, layout = "backbone")+
net <- ggraph(graph, layout = "dh") +
  geom_edge_link(aes(width = edges_filter$weight, color = edges_filter$weight), alpha = 0.2)+
  geom_node_point(aes(color = nodes_filter$category, size=nodes_filter$weight), shape = 19)+
  geom_node_text(aes(label = nodes_filter$tool), repel = TRUE, size = 5)+
  scale_edge_width_continuous(range = c(0.1, 5), name = "Co-uses") + # control size
  scale_edge_colour_continuous(
    low = "#ffffff",
    # low = "#d3d3d3",
    high = "#000000",
    space = "Lab",
    na.value = "grey50",
    guide = NULL
  ) +
  scale_size_continuous(name = "Number of respondents", range = c(1, 10)) +
  scale_color_manual(name = "Category", values = HL_colors, 
                     labels = c("Languages", "Platforms", "Web Frameworks", 
                                "Other Frameworks"))+
  coord_fixed()+
  theme_graph() +
  theme(legend.position = "right", 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17)
        # legend.key = element_rect(size = 5)
        ) + 
  guides(colour = guide_legend(override.aes = list(size=8)))

net

ggsave(filename = "backbone-weighted.png", plot = net, width = 12, height = 10, dpi=720)
ggsave(filename = "backbone-weighted.svg", plot = net, width = 15, height = 8)

