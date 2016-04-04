install.packages("igraph") 
install.packages("network") 
install.packages("ndtv")
install.packages("ggplot2")

library(igraph)
library(ggplot2)
# Set the working directory to the folder containing the example data
setwd("/Users/Yanish/Documents/winter_2016/Integ_275/final") 

dat <- read.csv("/Users/Yanish/Documents/winter_2016/Integ_275/final/Data/FinalAdjacencyMatrix.csv")

head(dat)
