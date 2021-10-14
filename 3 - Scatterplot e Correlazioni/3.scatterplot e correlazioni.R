
dati<-read.csv("energy output.csv")
attach(dati)

plot(Temperature,Pressure)
covarianza <- sum( (Temperature-mean(Temperature))*(Pressure-mean(Pressure)) )/(length(Temperature)-1)
covarianza

cov(Temperature,Pressure)


pearson.rho <- cov(Temperature,Pressure)/ (sd(Temperature)*sd(Pressure))
pearson.rho
cor(Temperature,Pressure)

plot(Temperature,Vacuum)
cov(Temperature,Vacuum)
cor(Temperature,Vacuum)

library(tidyverse)

dati<-datasauRus::datasaurus_dozen

filter(dati,dataset!="x_shape") %>% 
  group_by(dataset) %>% 
  summarise(muX=mean(x),
            muY=mean(y),
            rhoXY= cor(x,y))



ggplot(data=filter(dati,dataset!="x_shape"))+
  geom_point(aes(x=x,y=y))+
  facet_wrap(~dataset,
             nrow = 3,
             ncol = 4)




#non parametriche

dati <- read.csv("gare.csv",sep=";")
dati
dati$pos.100m<- rank(dati$m100)
dati$pos.maratona<- rank(dati$Maratona)
dati
dati$diff<-dati$pos.100m - dati$pos.maratona

n=dim(dati)[1]
  
cor.spearman <- 1 - 6 * (sum((dati$diff)^2) / (n*(n^2-1)))

cor(dati$m100,dati$Maratona,method = "spearman")
cor(dati$m100,dati$Maratona,method = "kendall")
