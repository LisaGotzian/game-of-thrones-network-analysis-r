###################################################################-
# Analyzing a social network graph of Game of Thrones Characters
# 
# Lisa Gotzian, September 5, 2018
# 
# Purpose: Created a table of the social connections among the 150
# most important characters in GoT, rating them according to their
# positive/negative connection and the strength of said connection.
# This code then analyzes the network based on this table with
# igraph.
# 1) a meaningful plot of the entire network 
# 2) further network analysis
# 3) a plot of different centrality measures 
##################################################################-


#-------------------------- Preliminaries ---------------------------
library(igraph)
library(data.table) # to export R data as a csv file
library(ggplot2)
library(ggthemes) # for the dark plot
library(dichromat) # for colourblind accessibility

# Please make sure to have the following files in your current working directory:
# * GOT_network.csv
# * GOT_nodesdead.csv
# * GOT_coord.RDS

#---------------------------- The data ------------------------------
GOTdata <- read.csv2("GOT_network.csv")
GOTgraph <- graph_from_data_frame(GOTdata, directed = FALSE)
observations <- nrow(GOTdata)

redcolors <- c("paleturquoise3", "rosybrown2")

#------------------------- Visualizing the network --------------------
# The following describes the emergence of a meaningful plot of the entire
# network GOTdata, positive relationships (edges) are blue, negative are red. Dead
# people (nodes) are grey.

### 1a. Assign meaningful colors to "+" and "-": "+" becomes blue, "-" becomes red
for (i in 1:observations){
  #this could be done by the function gsub() as well, but this way, it seems clearer to me.
  if (E(GOTgraph)$Type[i] == "+"){
    E(GOTgraph)$Type[i] <- redcolors[1]
  } else {
    E(GOTgraph)$Type[i] <- redcolors[2]
  }
}


### 1b. Add node attributes: if the character is dead, the node is white.
# fwrite(as.list(V(GOTgraph)), file = "GOT_nodes.csv") # export the vertex list, add the dead/alive
## status and...
GOTdead <- read.csv2("GOT_nodesdead.csv") # ... import the new list.
sum(GOTdead$Dead)/nrow(GOTdead) # only 37% of the characters introduced in season 1 are alive
# in season 7.

# As the vertices are still sorted the wrong way, the next step is to order the vertices in alphabetical
# order.
permuteVerticesGOT <- as.data.frame(V(GOTgraph)$name) # A dataframe of the vertices.

lengthVertices <- nrow(permuteVerticesGOT)
permuteVerticesGOT$order <- 1:lengthVertices # The original order
permuteVerticesGOT <- permuteVerticesGOT[order(permuteVerticesGOT),] #sort by the alphabet
# For some reason, the function spits out 150 NAs as well, we'll get rid of them.
permuteVerticesGOT <- permuteVerticesGOT[(lengthVertices + 1):300,]
# The new column order gives away the order in which the original graph has to be ordered.
permuteVerticesGOT$index <- 1:lengthVertices # the new order
permuteVerticesGOTdone <- permuteVerticesGOT[order(permuteVerticesGOT$order),] #back to original order

GOTgraph <- permute(GOTgraph, permuteVerticesGOTdone$index) #use the new order to sort the graph

# After the order has been corrected, I can add the GOTdead attributes.
V(GOTgraph)$Dead <- GOTdead$Dead #assign the attribute to the vertices.

for (i in 1:lengthVertices){
  if (V(GOTgraph)$Dead[i] == "0"){
    V(GOTgraph)$Deadcolor[i] <- "gray47" # dead characters are grey.
  } else {
    V(GOTgraph)$Deadcolor[i] <- "black" # alive characters are black.
  }
}


### 1c. Increase the edge size based on the weight.
# As the biggest edge size is 291, the upper limit is chosen by hand.
max(E(GOTgraph)$weight) # 291
threshold <- 50
for (i in 1:observations){
  if (E(GOTgraph)$weight[i] >threshold){ # if it's bigger than the threshold
    E(GOTgraph)$weightadj[i] <- threshold/2 # assign a new value, divided by scalar for readability
  }else{
    E(GOTgraph)$weightadj[i] <- E(GOTgraph)$weight[i]/2 # keep the old value, divided by scalar
  }
}


### 1d. Increase the node size based on the number of connections overall, the degree,
# have them come into the foreground.
GOTnodedegree <- igraph::degree(GOTgraph)
V(GOTgraph)$nodedegree <- GOTnodedegree # add the degree to the graph

# Take the edge weights into account as well:
GOTnodestrength <- igraph::strength(GOTgraph, weights = E(GOTgraph)$weight)


### 1e. Find out if there are subgroups
# Community strucure via short random walks
GOTcluster <- cluster_walktrap(GOTgraph, weights = E(GOTgraph)$weight)

# based on the betweenness
GOTclusterbetween <- cluster_edge_betweenness(GOTgraph, weights = E(GOTgraph)$weight)

## Print the results
# print(GOTcluster) # returns 12 cluster
# plot(membership(GOTcluster))
# plot(membership(GOTclusterbetween))
# plot(GOTcluster, GOTgraph)
# compare(membership(GOTcluster), membership(GOTclusterbetween))



### 1f. Adjust the position of the nodes in the graph. Commented out as it is no longer needed.
# Instead, GOTcoord.RDS is supplied.

# tkplot(GOTgraph, edge.color=E(GOTgraph)$Type,# step 1
#             vertex.color = V(GOTgraph)$Deadcolor, # step 2
#             vertex.label.color = V(GOTgraph)$Deadcolor, # step 2
#             edge.width = E(GOTgraph)$weightadj/3, # step 3
#             vertex.size = V(GOTgraph)$nodedegree, #step 4 (without log for this step to have
#        # a better handling on the nodes)
#             vertex.label.cex = log(V(GOTgraph)$nodedegree)/2, # step 4
#             vertex.label.dist = 1, # step 4, to have the labels next to the node
#             mark.groups = membership(GOTclusterbetween), # step 5
#             mark.col = gray.colors(12, alpha = 0.2), mark.border = NA, layout = GOTcoord) # step 5
# GOTcoord <- tk_coords(13)
# saveRDS(GOTcoord, file="GOT_coord.RDS")
# GOTcanvas <- tk_canvas(13)
# tk_close(13)

GOTcoord <- readRDS("GOT_coord.RDS") # this reads in the coordinates determined above

### 1g. Creating the final plot
plot(GOTgraph, edge.color=E(GOTgraph)$Type,# step 1: positive edges and negative edges get different colors
     vertex.color = V(GOTgraph)$Deadcolor, # step 2: dead characters become grey
     vertex.label.color = V(GOTgraph)$Deadcolor, # step 2
     edge.width = E(GOTgraph)$weightadj/3, # step 3: edges become thicker if two characters know each other
     # better
     
     vertex.size = 1, #step 4: the nodes as such become small,...
     vertex.label.cex = log(GOTnodestrength)/5, # step 4 .... only the names become bigger based on their
     # degree times the edge weights. The label size has been scaled by taking the natural logarithm of half
     # the degree. This way, the small nodes don't diminish.
     vertex.label.dist = 1, # step 4, to have the labels next to the node
     
     mark.groups = GOTcluster, # step 5: mark the clusters
     mark.col = gray.colors(12, start = 0.6, alpha = 0.1), mark.border = NA, # step 5
     
     layout = GOTcoord, asp=0, #step 6,
     # with the coordinates by hand from tkplot() and without an aspect ratio
     
     ### Some layout adjustments
     vertex.frame.color = NA, # remove the frame from the vertex
     edge.curved = TRUE, # curved edges
     main = "A Game of Networks", # add a title
     sub = "Network of Game of Thrones Characters based on the first season of the series.
     Grey names depict dead characters by season 7.") 
# View this graph in the zoomed plot window and pull it to full size. The aspect ratio has been removed.

#-------------------------- 2) Analyzing the network ------------------------
### Global measures
# Order/number of vertices and edges
gsize(GOTgraph) # number of edges: 614
gorder(GOTgraph) # number of vertices: 150

# The density
edge_density(GOTgraph) # density is 0.055

# Shortest paths
diameter(GOTgraph, directed = FALSE) # diameter is 48
farthest_vertices(GOTgraph, directed = FALSE ) # from node Mord to Will-(prologue)
mean_distance(GOTgraph, directed = FALSE) # the average shortest path between all vertices
# overall, the characters are <d> = 2,79 vertices away from each other
graphics.off();require(graphics)
plot(distance_table(GOTgraph, directed = FALSE)$res,
     type = "b",
     xlab = "Length of a shortest path",
     ylab = "Frequency", 
     main = "Distribution of shortest paths")
abline(v = mean_distance(GOTgraph, directed = FALSE), lty = 3) # add a line for the mean

# Transitivity
# the probability that the adjacent vertices of a vertex are connected
transitivity(GOTgraph, type = "global", weights = E(GOTgraph)$weight) # 0.365

### General centrality measures and figures
# Degree and strength
# View(GOTnodedegree)
hist(GOTnodedegree, freq = FALSE,# pk on y, k on x
     col = "darkslategray3",
     border = "darkslategray4",
     xlab = "Degree of a vertex k",
     ylab = "Probability pk",
     main = "Distribution of degrees")
abline(v = mean(GOTnodedegree), col = "darkslategray4", lty = 3) # add a line for the mean

# Average degree
mean(GOTnodedegree) # <k> = 8.187
fit_power_law(GOTnodedegree) # can't reject that it's a power law distribution

# View(GOTnodestrength)

# Betweenness centrality
GOTbetweenness <- data.frame(igraph::betweenness(GOTgraph))
# View(GOTbetweenness) # for the reader: to view and sort it by betweenness


# Closeness
GOTcloseness <- igraph::closeness(GOTgraph)
# View(GOTcloseness) # for the reader: to view and sort it by closeness

# Eigenvector centrality
GOTeigen <- igraph::eigen_centrality(GOTgraph)
# View(GOTeigen$vector)


# Ratio of supportive vs. aversive relationships
GOTgraphplus <- subgraph.edges(GOTgraph, eids = which(E(GOTgraph)$Type==redcolors[1]),
                               delete.vertices = FALSE)
GOTgraphminus <- subgraph.edges(GOTgraph, eids = which(E(GOTgraph)$Type==redcolors[2]),
                                delete.vertices = FALSE)

GOTratio <- strength(GOTgraphplus, weights = E(GOTgraphplus)$weight)/
  strength(GOTgraphminus, weights = E(GOTgraphminus)$weight)
#View(GOTratio)

GOTstrengthplus <- strength(GOTgraphplus, weights = E(GOTgraphplus)$weight)
GOTstrengthminus <- strength(GOTgraphminus, weights = E(GOTgraphminus)$weight)
# View(GOTstrengthplus)
# View(GOTstrengthplus)


#--------------------- 3) Centrality measures comparison -----------------
### All the local measures into one dataframe for comparison
GOTcomparison <- cbind.data.frame(GOTnodedegree, GOTnodestrength,
                                  GOTbetweenness, GOTcloseness, GOTeigen$vector,
                                  GOTstrengthplus, GOTstrengthminus, GOTratio,
                                  GOTdead$Dead)
colnames(GOTcomparison) <- c("Degree", "Strength", "Betweenness", "Closeness",
                             "Eigencentrality", "StrengthPlus", "StrengthMinus", "StrengthRatio", 
                             "Dead")
# This now assigns ranks instead of values to each column of the comparison.
# For simplicity reasons, the Names column is recovered afterwards.
GOTcomparisonrank <- setDT(GOTcomparison)[,lapply(.SD, frankv, order=-1L, ties.method="dense")]
GOTcomparisonrank$Names <- rownames(GOTbetweenness)

### Plot the comparison for the 15 nodes with the highest degrees
GOTcomparisonrank <- GOTcomparisonrank[order(GOTcomparisonrank$Degree),] # sort by degree
GOTcomparisonrank15 <- GOTcomparisonrank[1:15,] # choose the top 15 nodes with the highest degrees
GOTcomparisonrank15m <- melt(GOTcomparisonrank15, id.vars = "Names") # melt it for ggplot

# Set the colors from the dichromat package as the filling colors.
cols <- colorschemes$BluetoDarkOrange.18[3:17]
names(cols) <- GOTcomparisonrank15$Names

ggplot(GOTcomparisonrank15m, aes(x=as.numeric(variable), y=value, fill=Names, colour = Names)) +
  # specify the aesthetics. As.numeric() to have a continous line while the rank for the centrality
  # measures is plotted as a y value, one line for each name.
  
  geom_line()+
  scale_colour_manual(values = cols, breaks = rev(names(cols)))+
  # specify the colors and the order of the legend (reversing it by rev())
  
  scale_x_continuous(breaks = 1:length(GOTcomparison), labels = names(GOTcomparison))+ # specify the x axis
  scale_y_log10() + # log scale to have a better comparison
  ggtitle("Comparison of the different local measures")+
  xlab("Measure") + ylab("Rank")+
  theme_dark()+
  theme(legend.position = "left") # has to be the very last argument to be invoked


