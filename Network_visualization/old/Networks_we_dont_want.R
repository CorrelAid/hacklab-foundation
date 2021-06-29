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
#   https://igraph.org/c/doc/igraph-Layout.html
#two-d-layout-generators
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


##Centrality layout?

ggraph(graph,layout = "centrality",cent = graph.strength(graph))+
  geom_edge_link(alpha = 0.05,edge_colour = "grey66")+
  geom_node_point(aes(color=nodes_filter$category,size=nodes_filter$weight),shape = 19)+
  geom_node_text(aes(label = nodes_filter$tool))+
  scale_edge_width_continuous(range = c(0.2,0.9))+
  scale_size_continuous(range = c(1,8))+
  scale_fill_manual(values = got_palette)+
  coord_fixed()+
  theme_graph()+
  theme(legend.position = "none")
#Don't like this one



###Including another cutoff
cut.off <- mean(edges_filter$n)
graph_threshold <-delete.edges(graph,E(graph)[edges_filter$n<cut.off])

lay <- ggraph::create_layout(graph_threshold, layout = "dh") %>%
  left_join(nodes, by = c("name" = "tool"))

ggnet <- ggraph(lay) + 
  geom_edge_link(alpha = 0.05) + 
  geom_node_point(
    aes(size = weight, color = nodes_filter$category), 
    shape = 19,
    show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE) + # remove the labels if want to plot faster (comment this line)
  theme_graph() + 
  scale_fill_manual(values = got_palette) +
  scale_edge_width(range = c(0.2,3))
ggnet