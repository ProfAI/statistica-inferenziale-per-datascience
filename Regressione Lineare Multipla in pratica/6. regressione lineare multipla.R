dati<-read.csv("energy output.csv")
attach(dati)
summary(dati)
n<-nrow(dati)


moments::skewness(Energy.output)
moments::kurtosis(Energy.output)-3
shapiro.test(Energy.output)


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y))
  txt <- format(c(r, 1), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
}
#correlazioni
pairs(dati,lower.panel=panel.cor, upper.panel=panel.smooth)



#stimiamo un modello lineare con tutte le variabili
mod<-lm(Energy.output~Humidity+Pressure+Temperature+Vacuum+I(Vacuum^2),
        data=dati)
summary(mod)

mod2<-update(mod,~.-Pressure)
summary(mod2)

anova(mod2,mod)


mod3 <- lm(Energy.output~Humidity+Temperature+Vacuum,data=dati)
summary(mod3)

mod4 <- lm(Energy.output~Humidity+Temperature,data=dati)
summary(mod4)

mod.interaz <- lm(Energy.output~Humidity+Temperature+Humidity:Temperature)
summary(mod.interaz)

anova(mod.interaz,mod4)

install.packages("car")

car::vif(mod4)

AIC(mod,mod2,mod3,mod4,mod.interaz)
BIC(mod,mod2,mod3,mod4,mod.interaz)

MASS::stepAIC(mod,
              direction = "both",
              k=log(n))

#analisi residui
par(mfrow=c(2,2))
plot(mod4)


lmtest::bptest(mod4)
lmtest::dwtest(mod4)
shapiro.test(mod4$residuals)
plot(density(residuals(mod4)))

car::crPlots(mod4)

mod5<-lm(lm(Energy.output~Humidity+I(Humidity^2)+Temperature,data=dati))
summary(mod5)
#NO


#leverage
lev<-hatvalues(mod4)
plot(lev)
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

#outliers
plot(rstudent(mod4))
abline(h=c(-2,2))
car::outlierTest(mod4)

#distanza di cook
cook<-cooks.distance(mod4)
plot(cook,ylim = c(0,1)) 

summary(mod4)


mod_76 <- lm(Energy.output~Humidity+Temperature,data=dati[-76,])
summary(mod_76)

car::scatter3d(Energy.output~Humidity+Temperature)


MSE<-function(y_oss,y_prev){
  return(sum((y_oss-y_prev)^2)/length(y_prev))
}


mse_train<-MSE(dati$Energy.output, fitted(mod4))
mean(mod4$residuals^2)
deviance(mod4)/n

dati.test <- read.csv("energy.test.csv")
oss.test <- dati.test$Energy.output
prev.test<- predict(mod4,newdata = dati.test)
mse_test<-MSE(oss.test,prev.test)

mse_train;mse_test

plot(oss.test,prev.test)
abline(a=0,b=1)



