install.packages("igraph") 
install.packages("network") 
install.packages("ndtv")
install.packages("ggplot2")

library(igraph)
library(ggplot2)
# Set the working directory to the folder containing the example data
setwd("/Users/Yanish/Documents/winter_2016/Integ_275/final") 


dat <- read.csv("/Users/Yanish/Documents/winter_2016/Integ_275/final/Data/FinalAdjacencyMatrix.csv")
matrix <- as.matrix(read.csv("/Users/Yanish/Documents/winter_2016/Integ_275/final/Data/FinalAdjacencyMatrix.csv", sep=",", header=TRUE, row.names=1))
network1 <- graph.adjacency(matrix,weighted=TRUE)

network <- delete.vertices(network1, which(degree(network1) < 1))
network <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)

head(matrix)

set.seed(875)
par(mar=c(0,0,0,0)+.1)
l1 = layout.kamada.kawai(network)
l2 = layout.fruchterman.reingold(network)
plot(network, layout = l1, 
     vertex.label = NA, 
     vertex.color = "orange", 
     vertex.size = 3,
     edge.arrow.size = 0.3)
