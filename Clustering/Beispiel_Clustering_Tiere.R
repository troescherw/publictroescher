#Beispiel Clustering Agglomeratives Hierarchisches Linkage Average Clustering
#Dataset msleep (Package ggplot2)
#Verwendete Merkmale: name, vore (Carnivore, Omnivore, Herbivore, Insektenfresser), sleep_total


#Package für ggplot2 und dplyr
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

#Packages für Hierarchisches Cluster
if(!require("ggdendro")) install.packages("ggdendro")
library(ggdendro)

#Packages für k-means
if(!require("factoextra")) install.packages("factoextra")
if(!require("cluster")) install.packages("cluster")
if(!require("NbClust")) install.packages("NbClust")

library(factoextra)
library(cluster)
library(NbClust)

###################################
#Hierarchisches Clustern
###################################

#Erstelle neuen Dataframe ohne NAs
tiere_ <- msleep %>% select (name,vore,sleep_total) %>% filter(!is.na(vore) & !is.na(sleep_total))
tiere <- data.frame(row.names=tiere_$name, Gruppe=as.numeric(factor(tiere_$vore)), Schlafzeit=tiere_$sleep_total)
tiere

#Normalisieren der Werte
tierestd <- scale(tiere)

#Distanzmatrix erstellen
tieredist <- dist(tierestd, method = "euclidian")

#Modell erstellen
model <- hclust(tieredist, method = "average")

#Dendrogramm zeichnen
ggdendrogram(model, rotate = TRUE)

#Vergleich Mensch und "African Giant Pouched Rat" (Gambische Riesenhamsterratte):
tiere %>% filter(row.names(tiere) %in% c("African giant pouched rat", "Human"))

#Weitere Spezialisierung: Wir nehmen das Gewicht des Gehirns (brainwt) hinzu
tiere_ <- msleep %>% select (name,vore,sleep_total,brainwt) %>% filter(!is.na(vore) & !is.na(sleep_total) & !is.na(brainwt))
tiere  <- data.frame(row.names=tiere_$name, Gruppe=as.numeric(factor(tiere_$vore)), Schlafzeit=tiere_$sleep_total, GewichtGehirn=tiere_$brainwt)
tiere

#Werte normalisieren
tierestd <- scale(tiere)

#Distanzmatrix erstellen
tieredist <- dist(tierestd, method = "euclidian")

#Modell erstellen
model <- hclust(tieredist, method = "average")

#Dendrogramm zeichnen
ggdendrogram(model, rotate = TRUE)

###############################
#k-Means-Clustering
###############################

#Ermittle optimale Anzahl Zentren mit der "Ellbogenmethode": Ermittle für k= 1..10 die Summe der Quadratischen Abstände,
#Plotte die Punkte - wo die Funktion einen "Ellbogen" macht (Wendepunkt), befindet sich das optimale k

#Erstelle Funktion für k=1..10

estimateK <- function(){
  results <- NULL
  for(k in 1:10){
    modelkmeans <- kmeans(tieredist, centers = k)
    results <- rbind(results,modelkmeans$tot.withinss) #Ergebnis hinzufügen
  }
  res <- data.frame(x=1:10,y=results) #Erstelle dataframe
  print(res)
  return(res)
  
}

plot(estimateK())
# ==> 4 Cluster!

#Modell erstellen mit 4 Zentren

modelkmeans <- kmeans(tieredist, centers = 4)

#Zeichnen
fviz_cluster(modelkmeans, data=tierestd, geom = c("text","point"), ellipse.type = "norm", repel=TRUE, show.clust.cent = TRUE)
