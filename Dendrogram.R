####################################################################################################
## Title: Dndrogram for AFP
## Author: Allen Baumgardner-Zuzik
## Date: 2019/07/22
####################################################################################################

##### Preliminaries #####

## libraries

#install.packages("igraph")
#install.packages("ggraph")
library(tidyverse)
library(ggplot2)
library(ggraph)
library(igraph)
library(RColorBrewer)


##### Example 1 #####
# create an edge list data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,5), sep=""))
d2=data.frame(from=rep(d1$to, each=5), to=paste("subgroup", seq(1,25), sep="_"))
edges=rbind(d1, d2)

# Create a graph object 
mygraph <- graph_from_data_frame( edges )

# Basic tree
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()

##### Example 2 #####
# create a data frame 
data=data.frame(
  level1="CEO",
  level2=c( rep("boss1",4), rep("boss2",4)),
  level3=paste0("mister_", letters[1:8])
)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_text(aes(label = name, filter = leaf), hjust = 0.5, nudge_y = -0.03) +
  geom_node_point() +
  theme_void()
#  coord_flip()

# Create a graph object
mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
  geom_edge_diagonal(edge_color = "cyan") +
  geom_node_text(aes(label = name, filter = leaf, color = group), angle = 90, hjust = 1, nudge_y = -0.01) +
  geom_node_point(aes(filter = leaf, color = group), alpha = 0.6) +
  ylim(-0.4, NA)


##### Example 3 #####
theme_set(theme_void())

# data: edge list
d1=data.frame(from="origin", to=paste("group", seq(1,7), sep=""))
d2=data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))
edges=rbind(d1, d2)

# We can add a second data frame with information for each node!
name=unique(c(as.character(edges$from), as.character(edges$to)))
vertices=data.frame(
  name=name,
  group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value=sample(seq(10,30), length(name), replace=T)
)

# Create a graph object
mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
  geom_edge_diagonal(edge_color = "cyan") +
  geom_node_text(aes(label = name, filter = leaf, color = group), angle = 90, hjust = 1, nudge_y = -0.01) +
  geom_node_point(aes(filter = leaf, color = group), alpha = 0.6) +
  ylim(-0.4, NA)


##### Example 4 #####

# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges=rbind(d1, d2)

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(111)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name, edges$to ) ]


#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm")
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))




##### Build the AFP Dendograms #####

## Graph 1 - Income ##
# Import the data
data = read_csv(file = "funding.csv", col_names = TRUE)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edges_level3_4 = data %>% select(level3, level4) %>% unique %>% rename(from=level3, to=level4)
edge_list=rbind(edges_level1_2, edges_level2_3, edges_level3_4)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
# ggraph(mygraph, layout = 'partition', circular = FALSE) + # For Dendrogram 
ggraph(mygraph, layout = 'partition', circular = FALSE) + # For Dendro-tree
  geom_edge_diagonal(n = 1500, colour = "gray47") +
  geom_node_label(aes(label = name, filter = leaf == FALSE),
                 size = 3, nudge_x = 1, label.padding = unit(0.10, "lines"), label.size = 0) +
  geom_node_text(aes(label = name, filter = leaf == TRUE),
                 size = 3, nudge_y = 0.03, hjust = 0) +
  geom_node_point() +
  expand_limits(x = c(-0.1, 0.1), y = c(-0.1, 5.5)) +
  coord_flip() +
#  scale_y_reverse() +
  theme_void()


## Graph 2 - M&E Impediments ##
# Import the data
data = read_csv(file = "m&e_impediments.csv", col_names = TRUE)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edges_level3_4 = data %>% select(level3, level4) %>% unique %>% rename(from=level3, to=level4)
edges_level4_5 = data %>% select(level4, level5) %>% unique %>% rename(from=level4, to=level5)
edge_list=rbind(edges_level1_2, edges_level2_3, edges_level3_4, edges_level4_5)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
# ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + # for dendrogram
ggraph(mygraph, layout = 'partition', circular = FALSE) + # for Dendro-tree
  geom_edge_diagonal(n = 1500, colour = "gray47") +
  geom_node_label(aes(label = name, filter = leaf == FALSE),
                  size = 3, nudge_x = 1.4, label.padding = unit(0.10, "lines"), label.size = 0) +
  geom_node_text(aes(label = name, filter = leaf == TRUE),
                 size = 3, nudge_y = 0.02, hjust = 0) +
  geom_node_point() +
  expand_limits(x = c(-2.0, 2.0), y = c(-0.1, 7.0)) +
  coord_flip() +
#  scale_y_reverse() +
  theme_void()


## Graph 3 - Needs For Reform ##
# Import the data
data = read_csv(file = "needs_for_reform.csv", col_names = TRUE)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edges_level3_4 = data %>% select(level3, level4) %>% unique %>% rename(from=level3, to=level4)
edges_level4_5 = data %>% select(level4, level5) %>% unique %>% rename(from=level4, to=level5)
edge_list=rbind(edges_level1_2, edges_level2_3, edges_level3_4, edges_level4_5)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
# ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + # For Dendrogram
ggraph(mygraph, layout = 'partition', circular = FALSE) + # For Dendro-tree 
  geom_edge_diagonal(n = 1500, colour = "gray47") +
  geom_node_label(aes(label = name, filter = leaf == FALSE),
                  size = 3, nudge_x = 1.8, label.padding = unit(0.10, "lines"), label.size = 0) +
  geom_node_text(aes(label = name, filter = leaf == TRUE),
                 size = 3, nudge_y = 0.04, hjust = 0) +
  geom_node_point() +
  expand_limits(x = c(-1.1, 1.1), y = c(-0.1, 10.0)) +
  coord_flip() +
#  scale_y_reverse() +
  theme_void()


## Graph 4 - opportunities_for_future ##
# Import the data
data = read_csv(file = "opportunities_for_future.csv", col_names = TRUE)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edges_level3_4 = data %>% select(level3, level4) %>% unique %>% rename(from=level3, to=level4)
edges_level4_5 = data %>% select(level4, level5) %>% unique %>% rename(from=level4, to=level5)
edge_list=rbind(edges_level1_2, edges_level2_3, edges_level3_4, edges_level4_5)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
# ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + # For Dendrogram
ggraph(mygraph, layout = 'partition', circular = FALSE) + # For Dendro-tree
  geom_edge_diagonal(n = 1500, colour = "gray47") +
  geom_node_label(aes(label = name, filter = leaf == FALSE),
                  size = 3, nudge_x = 1.3, label.padding = unit(0.10, "lines"), label.size = 0) +
  geom_node_text(aes(label = name, filter = leaf == TRUE),
                 size = 3, nudge_y = 0.03, hjust = 0) +
  geom_node_point() +
  expand_limits(x = c(-2.0, 2.0), y = c(-0.1, 8.0)) +
  coord_flip() +
#  scale_y_reverse() +
  theme_void()


## Graph 5 - Threats to Future ##
# Import the data
data = read_csv(file = "threats_to_future.csv", col_names = TRUE)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edges_level3_4 = data %>% select(level3, level4) %>% unique %>% rename(from=level3, to=level4)
edges_level4_5 = data %>% select(level4, level5) %>% unique %>% rename(from=level4, to=level5)
edge_list=rbind(edges_level1_2, edges_level2_3, edges_level3_4, edges_level4_5)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
# ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + # For Dendrogram
ggraph(mygraph, layout = 'partition', circular = FALSE) + # For Dendro-tree
  geom_edge_diagonal(n = 1500, colour = "gray47") +
  geom_node_label(aes(label = name, filter = leaf == FALSE),
                  size = 3, nudge_x = 1.3, label.padding = unit(0.10, "lines"), label.size = 0) +
  geom_node_text(aes(label = name, filter = leaf == TRUE),
                 size = 3, nudge_y = 0.03, hjust = 0) +
  geom_node_point() +
  expand_limits(x = c(-0.5, 0.5), y = c(-0.1, 8.0)) +
  coord_flip() +
#  scale_y_reverse() +
  theme_void()


## Graph 6 - Circular Dendrogram ##
# Import the data
data = read_csv(file = "circular.csv", col_names = TRUE)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list = rbind(edges_level1_2, edges_level2_3)

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edge_list$from), as.character(edge_list$to))), 
  value = runif(36)) 

# Add a column with the group of each name
vertices$level = edge_list$from[ match( vertices$name, edge_list$to ) ]

# Add information concerning the added label: angle, horizontal adjustement and potential flip
# Calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edge_list$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
#vertices$angle<-ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)

# creating a plot object
mygraph <- graph_from_data_frame( edge_list, vertices = vertices )

# plot the graph
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(n = 100, colour = "gray47") +
  scale_edge_color_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.15, y = y*1.15, filter = leaf, label = name, angle = angle, 
                     hjust = 0, color = level),
                 size = 3) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y = y*1.07, color = level)) +
  scale_color_manual(values = rep(brewer.pal(9, "Paired"), 30)) +
  scale_size_continuous(range = c(0.1, 10)) +
  expand_limits(x = c(-3.0, 3.0), y = c(-3.0, 3.0)) +
  theme_void() +
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")
  )


## Graph 7 - Sunburst##


