install.packages("igraph") 
install.packages("network") 
install.packages("ggplot2")
install.packages("ndtv")

library(igraph)
library(network)
library(ggplot2)
library(statnet)
library(ndtv)

# Set the working directory to the folder containing the example data
setwd("/Users/Yanish/Documents/winter_2016/Integ_275/final") 
#setwd("/Users/emilymac/Desktop/integ275")
#setwd("/Users/rachelwood/Documents/integ275")
#setwd("/Users/ianhamilton/Desktop/INTEG\ 275\ Final\ Project") 


# Read in adjacency matrix CSV file
dat <- read.csv("Data/FinalAdjacencyMatrix.csv")
# Read in CSV file with attributes
att <- read.csv("Data/EnronEmployeeInformation.csv")
View(att)
# Create Matrix from CSV file
matrix <- as.matrix(read.csv("Data/FinalAdjacencyMatrix.csv", sep=",", header=TRUE, row.names=1))

summary(matrix)

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
V(network)$id <- att$ID
V(network)$dept <- as.character(att$Department)
V(network)$department <- att$Department
V(network)$email <- as.character(att$EmailID)
V(network)$title <- as.character(att$Title)
V(network)$betweenness <- centralization.betweenness(network)$res
V(network)$eigen <- eigen_centrality(network, directed = TRUE)$vector
colrs <- c("orange2", "sky blue2") 
V(network)$color <- colrs[V(network)$sex]



data_df <- data.frame(V(network)$id,
                      V(network)$name, 
                      V(network)$gender, 
                      V(network)$dept, 
                      V(network)$title,
                      V(network)$email,
                      V(network)$degree,
                      V(network)$betweenness, 
                      V(network)$eigen)

colnames(data_df) <- c("User_ID", 
                       "Name", 
                       "Gender",
                       "Department",
                       "Title",
                       "E-mail",
                       "Degree",
                       "Betweenness_Centrality",
                       "Eigenvector_Centrality")



head(data_df)
View(data_df)

write.csv(data_df, "Data/network_dat.csv")

summary(network)

lay1 = layout.kamada.kawai(network, kkconst = vcount(network)/10)
lay2 <- layout_with_kk(network)
lay3 <- layout_nicely(network)
lay4 <- layout_with_fr(network)
lay5 <- layout.drl(network)
lay6 <- layout.sphere(network)
lay7 <- layout.auto(network)
lay8 <- layout_with_dh(network)

#plots
set.seed(875)
par(mar=c(0,0,0,0)+0.1)
#Degree Centrality plot
pdf("plots/Degree_Centrality.pdf")
plot(network,
     layout = lay2, 
     vertex.label = ifelse(V(network)$degree > 45, V(network)$title, NA),
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.color = V(network)$color,
     vertex.frame.color = "white",
     vertex.size = V(network)$degree*0.2,
     edge.arrow.size = 0.3,
     edge.color = "grey",
     main="Degree Centrality")
dev.off()

#Betweenness Centrality plot
pdf("plots/Betweenness_Centrality.pdf")
plot(network,
     layout = lay2, 
     vertex.label = ifelse(V(network)$betweenness > 750, V(network)$title, NA),
     vertex.label.color = "black",
     vertex.color = V(network)$color,
     vertex.frame.color = "white",
     vertex.size = V(network)$betweenness*0.012,
     edge.arrow.size = 0.3,
     edge.color = "grey",
     main ="Betweenness Centrality")
dev.off()

#Eigenvector Centrality plot
pdf("plots/Eigenvector_Centrality.pdf")
plot(network,
     layout = lay2, 
     vertex.label = ifelse(V(network)$eigen > 4.8e-02, V(network)$title, NA),
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.color = V(network)$color,
     vertex.frame.color = "white",
     vertex.size = ifelse(V(network)$eigen > 4.8e-02, 15, 4),
     edge.arrow.size = 0.3,
     edge.color = "grey",
     main = "Eigenvector Centrality")
dev.off()


#Side by side plots
pdf("plots/Centrality_measures_top_10.pdf")
par(mfrow=c(1,3), mar=c(0,0,1.5,0))
plot(network,
     layout = lay2, 
     vertex.label = NA,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.color = ifelse(V(network)$degree > 45, V(network)$color, "slate grey"),
     vertex.frame.color = "white",
     vertex.size = ifelse(V(network)$degree > 45, V(network)$degree*0.2, 3),
     edge.arrow.size = 0.3,
     edge.color = "grey",
     main= "Degree",
     frame = T)

plot(network,
     layout = lay2, 
     vertex.label = NA,
     vertex.label.color = "black",
     vertex.color = ifelse(V(network)$betweenness > 750, V(network)$color, "slate grey"),
     vertex.frame.color = "white",
     vertex.size = ifelse(V(network)$betweenness > 750, V(network)$betweenness*0.015, 3),
     edge.arrow.size = 0.3,
     edge.color = "grey",
     main="Betweenness",
     frame = F)

plot(network,
     layout = lay2, 
     vertex.label = NA,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.color = ifelse(V(network)$eigen > 4.8e-02, V(network)$color, "slate grey"),
     vertex.frame.color = "white",
     vertex.size = ifelse(V(network)$eigen > 4.8e-02, 15, 4),
     edge.arrow.size = 0.3,
     edge.color = "grey",
     main="Eigenvector",
     frame = F)
dev.off()

pdf("plots/Cumulative_degree_distribution.pdf")
par(mar=c(1,1,1,1)+4)
dd <- degree.distribution(network, cumulative=T, mode="all")
plot(dd, pch=19, cex=1, col=V(network)$color, xlab="Degree", ylab ="Cumulative Frequency")
dev.off()


#Community detection stuff
#net.undir <- as.undirected(network)

net.undir <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
l1 <- layout_with_kk(net.undir)
l2 <- layout_nicely(net.undir)
l3 <- layout.fruchterman.reingold(net.undir)


comdet <- cluster_louvain(net.undir)
len <- length(comdet) # how many are there
lou_size <- sizes(comdet) # how many inside each?
memb <- membership(comdet)
V(net.undir)$membership <- memb


summary(net.undir)

ls.df <- as.data.frame(lou_size) # make it a dataframe
colnames(ls.df) <- c("Community_ID", "Num_Nodes") # fix the column names
View(ls.df) # take a peak

write.csv(ls.df, "Data/Community_sizes_dat.csv")

head(memb)

Memb_df <- data.frame(V(net.undir)$id,
                      V(net.undir)$name, 
                      V(net.undir)$gender, 
                      V(net.undir)$dept, 
                      V(net.undir)$title,
                      V(net.undir)$membership)

colnames(Memb_df) <- c("User_ID", 
                       "Name", 
                       "Gender",
                       "Department",
                       "Title",
                       "Community_ID")

View(Memb_df)
write.csv(data_df, "Data/Membership_dat.csv")


pdf("plots/Communities_1.pdf")
set.seed(986)

par(mar=c(0,0,0,0)+.1)
plot(comdet, net.undir, layout = l1, edge.arrow.size = 0.2, vertex.label=NA, vertex.size=5)
dev.off()

pdf("plots/Communities_nice_layout.pdf")
set.seed(986)

par(mar=c(0,0,0,0)+.1)
plot(comdet, net.undir, layout = l2, edge.arrow.size = 0.2, vertex.label=NA, vertex.size=5)
dev.off()


#Disregard below
net <- simplify(network, remove.multiple = FALSE, remove.loops = TRUE)
summary(net)
net <- as.undirected(net)
summary(net)
net.undir <- igraph::delete.vertices(net.undir, lou_size < 2)
#add membership as a variable
#isolate by variable
#delete vertices according to that variable etc

summary(net.undir)

set.seed(986)

par(mar=c(0,0,0,0)+.1)
plot(comdet, net.undir, layout = l2, edge.arrow.size = 0.3, vertex.label=NA, vertex.size=5)



pdf("plots/Communities_2.pdf")
set.seed(986)

par(mar=c(0,0,0,0)+.1)
plot(comdet, net.undir, layout = l2, vertex.label=NA, vertex.size=5)
dev.off()




#Scrappies

ggplot(data_df, aes(V(network)$gender)) +
  geom_histogram(data = V(network)$sex, stat = "count", position = "stack", binwidth = NULL, bins = 2, na.rm = FALSE, show.legend = "right") + theme_bw()

geom_histogram(mapping = NULL, data = NULL, stat = "bin", position = "stack", ..., binwidth = NULL, bins = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)



edge.start <- igraph::get.edges(network, 1:ecount(network))[,1] 
edge.col <- V(network)$sex[edge.start]
colrs <- c("orange1", "sky blue2") 
V(network)$color <- colrs[V(network)$sex]


