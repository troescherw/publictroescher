##############################
#Beispiele Lineare Regression#
##############################

#x-Werte definieren (Zahlen 1 bis 20)
x <- 1:20

#y-Werte definieren
y <- c(2,4,6,9,12,24,16,22,34,36,40,45,46,60,70,72,80,85,100,101)

#Erstelle Dataframe mit x und y - Werten
daten <- data.frame(x=x, y=y)

#Scatterplot mit Regressionsgeraden
ggplot(daten, aes(x,y)) + geom_point() + geom_smooth(method = "lm", se=FALSE)

#Lineares Modell erstellen und ausgeben
model <- lm(y~x)
model

#Korrelationskoeffizient berechnen
cor(x,y)

#Detaillierte Informationen über Korrelation
cor.test(x,y)

##############################################
#Gegenbeispiel mit Zufallszahlen für y-Werte #
##############################################

y <- runif(20,-20,20)
daten <- data.frame(x=x, y=y)

#Scatterplot mit Regressionsgeraden
ggplot(daten, aes(x,y)) + geom_point() + geom_smooth(method = "lm", se=FALSE)

#Lineares Modell erstellen und ausgeben
model <- lm(y~x)
model

#Korrelationskoeffizient berechnen
cor(x,y)

#Detaillierte Informationen über Korrelation
cor.test(x,y)

