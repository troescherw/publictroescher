#mtcars: Statistiken über Autos:
#mpg - Miles/Gallon
#cyl - Anzahl Zylinder
#am - Getriebe (0=Automatic, 1=Manuell)
#gear - Anzahl der Vorwärtsgänge
#wt - Gewicht


data(mtcars)
str(mtcars)

#Installation und Aktivierung von ggplot2
install.packages("ggplot2")

#ggrepel: Verbessert die Beschriftung der Labels (ohne Überlappung)
#dendextend: Ermöglicht u.a. das Einfärben der Äste (Branches) im 
#Dendrogramm
install.packages("ggrepel")
install.packages("dendextend")
library(ggplot2)
library(dplyr)
library(ggrepel)
library(dendextend)


#Datenframe erstellen und standardisieren
datenkm <- data.frame(Verbrauch=mtcars$mpg, Gewicht=mtcars$wt)
datenkm <- data.frame(scale(datenkm))

#Clustern!
cl = kmeans(data.matrix(datenkm), 4)

#Grafik plotten
ggplot(datenkm, aes(x=Gewicht, y=Verbrauch)) + geom_point(col=cl$cluster,size=4, alpha=0.5) + 
  geom_text_repel(label=row.names(mtcars)) + ggtitle("K-means Cluster für Fahrzeuge")


#Hierarchisches Linkage Average Clustering

#Teile aus mtcars für Clustering extrahieren
daten.cars <- data.frame(Gewicht=mtcars$wt, Verbrauch=mtcars$mpg)
row.names(daten.cars) <- rownames(mtcars)
daten.scaled = scale(daten.cars)
daten.dist <- dist(daten.scaled)
daten.clust <- hclust(daten.dist, method="average")

#Grafik erstellen (farbige Branches für 4 Cluster), Dicke der Äste definieren, Punkte der Blätter def., Schriftgröße setzen
d1 <- color_branches(daten.clust, k=4) %>% set("branches_lwd", 4) %>% set("nodes_pch", 19) %>% set("labels_cex", 0.8)

#Dendrogramm plotten
plot_horiz.dendrogram(d1, main="Dendrogramm für Fahrzeuge")



