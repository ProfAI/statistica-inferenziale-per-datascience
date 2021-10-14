dati<-read.csv("energy output.csv")
attach(dati)
summary(dati)
#centrale termoelettrica con sensori che misurano le variabili ambientali intorno alla centrale
#energy in megawatt
#temperatura in gradi
#pressione in millibar
#umidità relativa e vapori di scarico


plot(Temperature,Energy.output)
cor(Temperature,Energy.output)

b1<- cov(Temperature,Energy.output)/var(Temperature)
b0<- mean(Energy.output)-b1*mean(Temperature)
b0;b1  
  
mod_lin<-lm(Energy.output~Temperature, data=dati)  #[-76,]
mod_lin$coefficients

summary(mod_lin)
abline(mod_lin,col=2)




#par(mfrow=c(1,2))
plot(residuals(mod_lin))
abline(h=mean(residuals(mod_lin)))



plot(density(residuals(mod_lin)))

shapiro.test(mod_lin$residuals)
lmtest::bptest(mod_lin)
lmtest::dwtest(mod_lin)

which.min(Energy.output)


predict(mod_lin,data.frame(Temperature=40))



plot(Pressure,Energy.output)
cor(Pressure,Energy.output)
mod_lin2<-lm(Energy.output~Pressure, data=dati)  #[-76,]
#fai vedere con dollaro

summary(mod_lin2)
abline(mod_lin2,col=2)

#par(mfrow=c(1,2))
plot(residuals(mod_lin2))
abline(h=mean(residuals(mod_lin2)))



plot(density(residuals(mod_lin2)))

shapiro.test(mod_lin2$residuals)
lmtest::bptest(mod_lin2)
lmtest::dwtest(mod_lin2)



predict(mod_lin2,data.frame(Pressure=1000))
