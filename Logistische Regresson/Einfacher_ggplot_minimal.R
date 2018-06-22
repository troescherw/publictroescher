str(titanic)
library(dplyr)
library(ggplot2)



titanic %>% filter(Pclass==2 & !is.na(Age)) %>% ggplot(aes(x=Age, y=Survived)) + geom_point() +
  geom_smooth(method = "glm", method.args=list(family="binomial"), se=FALSE)


