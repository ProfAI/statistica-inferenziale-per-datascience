
dati<-read.csv("stop smoking.csv",stringsAsFactors = T,sep=";")
dati$reddito <- rchisq(nrow(dati),20)
set.seed(42)
dati<-dati[sample(1:nrow(dati),150),]


dati<-read.csv("stopsmoking150.csv",stringsAsFactors = T,sep=";")


summary(dati)
attach(dati)
moments::skewness(peso)
moments::kurtosis(peso)-3
n <- nrow(dati)

shapiro.test(peso)

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


par(mfrow=c(1,2))
boxplot(peso)
boxplot(peso~sesso)
t.test(peso~sesso)

boxplot(peso)
boxplot(peso~sport)
t.test(peso~sport)

mod<-lm(peso~altezza+età+mesistop+sesso+sport+reddito)
summary(mod)

mod2<-lm(peso~altezza+mesistop+sesso+sport)
summary(mod2)

anova(mod2,mod)
BIC(mod,mod2)
car::vif(mod2)


#analisi residui
par(mfrow=c(2,2))
plot(mod2)


lmtest::bptest(mod2)
lmtest::dwtest(mod2)
shapiro.test(mod2$residuals)
plot(density(residuals(mod2)))


#leverage
lev<-hatvalues(mod2)
plot(lev)
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

#outliers
plot(rstudent(mod2))
abline(h=c(-2,2))
car::outlierTest(mod2)

#distanza di cook
cook<-cooks.distance(mod2)
plot(cook,ylim = c(0,1)) 


summary(età)
età_cl<-cut(età,breaks=c(25,40,55,65))


mod3<-lm(peso~altezza+mesistop+sesso+sport+età_cl,data=dati)
summary(mod3)
BIC(mod2,mod3)


library(ggplot2)
ggplot(data=dati)+
  geom_point(aes(x=mesistop,
               y=peso,
               col=sesso),position = "jitter")+
  geom_smooth(aes(x=mesistop,
                  y=peso,
                  col=sesso),se=F,method = "lm")
  
