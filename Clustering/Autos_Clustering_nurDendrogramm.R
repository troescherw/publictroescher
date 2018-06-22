library(ggplot2)
library(dplyr)
library(ggdendro)
library(dendextend)

autos <- data.frame(row.names=row.names(mtcars),mpg=mtcars$mpg, wt=mtcars$wt)

autos.scaled <- scale(autos)
autos.dist <- dist(autos.scaled)
autos.dist

modell <- hclust(autos.dist, method="average")
d <- as.dendrogram(modell, rotate=TRUE)
d <-color_branches(modell,k=4, col=c("red", "green", "blue", "orange"))
plot(d)


