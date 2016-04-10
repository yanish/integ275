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

#network1 <- delete.vertices(network1, which(degree(network1) < 1))
#network1 <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)

# Create graph from adjacency matrix
network <- graph.adjacency(matrix,weighted=TRUE)

# Clean graph
network <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)

# Add vertex attributes by creating new variable (V(Network)$variable <assign attribute from file/ source etc)
V(network)$degree <- igraph::degree(network, v = V(network), mode = c("all"))
V(network)$gender <- as.character(att$Sex)#as.character needed to keep attribute in string format
V(network)$sex <- att$Sex
V(network)$dept <- as.character(att$Department)
V(network)$department <- att$Department
V(network)$email <- as.character(att$EmailID)
V(network)$title <- as.character(att$Title)

data_df <- data.frame(V(network)$name, V(network)$degree, V(network)$gender, V(network)$dept, V(network)$title)

head(data_df)

colrs <- c("red", "gold")
V(network)$color <- colrs[V(network)$gender]

head(V(network)$email)
# Check network
summary(network)

set.seed(875)
par(mar=c(0,0,0,0)+.1)
l1 = layout.kamada.kawai(network, kkconst = vcount(network)/10)

pdf("plots/plot2.pdf")
plot(network, layout = l1, 
     vertex.label = NA, 
     vertex.color = V(network)$department, 
     vertex.size = 8,
     edge.arrow.size = 0.3,
     edge.color = "slate grey")

dev.off()











