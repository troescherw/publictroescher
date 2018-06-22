library(ggplot2)
library(dplyr)


###BEISPIEL 1 ##################
coordsX <- c(1,2,4,5)
coordsY <- c(1,1,3,4)
daten <- data.frame(x=coordsX,y=coordsY)
plot(daten)

model <- kmeans(daten, centers = 2)
model$centers
plot(daten)
points(model$centers, pch=22, col="red")

################################

autos <- data.frame(row.names=row.names(mtcars),mpg=mtcars$mpg, wt=mtcars$wt)
kmodell <- kmeans(autos, centers = 4)

#x- und y-Werte der Center in data.frame
#tapply: Wendet Funktion auf die Elemente des ersten Vektors an,
#fasst zusammen bzgl. der Zuordnung im zweiten Vektor (hier: Cluster-Nummern)
centers <- data.frame(kmodell$centers)


autos %>% ggplot(aes(x=mpg, y=wt)) +
  geom_point(color=kmodell$cluster, pch=1, size=3) +
  geom_point(data=centers, size=6, 
             aes(mpg,wt), color="orange",
             pch=15) + geom_text(aes(label=row.names(autos)))

######################################
#Verwendung der Funktion fviz_cluster
######################################

if(!require("factoextra")) install.packages("factoextra")
if(!require("cluster")) install.packages("cluster")
if(!require("NbClust")) install.packages("NbClust")

library(factoextra)
library(cluster)
library(NbClust)

fviz_cluster(kmodell, data=autos.dist, geom=c("point", "text"), ellipse.type = "norm", repel=TRUE, show.clust.cent = TRUE)

#Bestimmung des optimalen k mit der Ellbow-Methode

#Erstelle Funktion, die die Summe der quadriereten Abstände der Punkte
#von ihren jeweiligen Cluster-Zentren in einem Dataframe speichert (k von 1 bis 10)
#Für das k, an dem die Funktion einen "Knick" macht (Ellbogen), befindet sich das optimale k

estimate_k <- function(x){
  dist <- numeric(10)
  for(k in 1:10){
    m  <- kmeans(x, centers = k)
    dist[k] <- m$tot.withinss
  }
  
  return(data.frame(k=c(1:10),dist))
}

plot(estimate_k(autos.dist))

