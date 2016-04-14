install.packages("igraph") 
install.packages("network") 
install.packages("ggplot2")

library(igraph)
library(network)
library(ggplot2)
library(statnet)
# Set the working directory to the folder containing the example data
setwd("/Users/Yanish/Documents/winter_2016/Integ_275/final") 

# Read in adjacency matrix CSV file
dat <- read.csv("Data/FinalAdjacencyMatrix.csv")
# Read in CSV file with attributes
att <- read.csv("Data/EnronEmployeeInformation.csv")

# Create Matrix from CSV file
matrix <- as.matrix(read.csv("Data/FinalAdjacencyMatrix.csv", sep=",", header=TRUE, row.names=1))

summary(matrix)
matrix
#network1 <- delete.vertices(network1, which(degree(network1) < 1))
#network1 <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)

# Create graph from adjacency matrix
network <- graph.adjacency(matrix,weighted=TRUE)

summary(network)
# Clean graph
network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)

summary(network)

# Add vertex attributes by creating new variable (V(Network)$variable <assign attribute from file/ source etc)
V(network)$degree <- igraph::degree(network, v = V(network), mode = c("all"))
V(network)$gender <- as.character(att$Sex)#as.character needed to keep attribute in string format
V(network)$sex <- att$Sex
V(network)$dept <- as.character(att$Department)
V(network)$department <- att$Department
V(network)$email <- as.character(att$EmailID)
V(network)$title <- as.character(att$Title)
V(network)$betweenness <- centralization.betweenness(network)$res
V(network)$eigen <- eigen_centrality(network, directed = TRUE)$vector


data_df <- data.frame(V(network)$name, 
                      V(network)$gender, 
                      V(network)$dept, 
                      V(network)$title,
                      V(network)$degree,
                      V(network)$betweenness, 
                      V(network)$eigen)

head(data_df)
View(data_df)

summary(network)

lay1 = layout.kamada.kawai(network, kkconst = vcount(network)/10)
lay2 <- layout_with_kk(network)
lay3 <- layout_nicely(network)
lay4 <- layout.fruchterman.reingold(network)
lay5 <- layout.drl(network)

set.seed(875)
par(mar=c(0,0,0,0)+.1)


pdf("plots/plot7.pdf")
plot(network, 
     layout = lay4, 
     vertex.label = NA, 
     vertex.color = V(network)$sex, 
     vertex.size = 8,
     edge.arrow.size = 0.3,
     edge.color = "slate grey")

dev.off()


net.undir <- as.undirected(network)
l1 <- layout_with_kk(net.undir)
l2 <- layout_nicely(net.undir)
l3 <- layout.fruchterman.reingold(net.undir)
comdet <- cluster_louvain(net.undir)
net.undir <- simplify(net.undir, remove.multiple = TRUE, remove.loops = TRUE)

len <- length(comdet) # how many are there
lou_size <- sizes(comdet) # how many inside each?
memb <- membership(comdet)
ls.df <- as.data.frame(lou_size) # make it a dataframe
head(memb)
colnames(ls.df) <- c("Community_ID", "Num_Nodes") # fix the column names
head(ls.df) # take a peak

pdf("plots/memplot1.pdf")
set.seed(986)

par(mar=c(0,0,0,0)+.1)
plot(comdet, net.undir, layout = l1, vertex.label=NA, vertex.size=5)
dev.off()


ggplot(data_df, aes(V(network)$gender)) +
  geom_histogram(aes(y = ..gender..), bandwidth=0.1) + theme_bw()







