#distribuzione sotto H0
QI=rnorm(1000000,
         mean = 100,
         sd=15)

plot(density(QI))

dati<-read.csv("dati QI.csv")
View(dati)

z_test = function(x, mu0, stdev, alfa){

  x = na.omit(x)
  mu_cap = mean(x)
  n = length(x)
  
  Z = (mu_cap - mu0) / (stdev / sqrt(n))
  valori.soglia = qnorm(c(alfa/2, 1-alfa/2))
  
  CI <- c(mu_cap+qnorm(alfa/2)*(stdev/sqrt(n)),
          mu_cap-qnorm(alfa/2)*(stdev/sqrt(n)))
    
  return(
    list(media.campionaria= mu_cap,
         stat.test = Z,
         valori.soglia = valori.soglia,
         pvalue = 2*pnorm(-abs(Z)),
         Int.Conf. = CI,
         
         grafico=plot(density(rnorm(1000000,0,1))),
         abline(v=valori.soglia,col=2))
         )
  
  
}

z_test(dati$c3,100,15,0.05)

points(x=-2.25,y=0,pch=20,col=4,cex=3)


install.packages("TeachingDemos")

TeachingDemos::z.test(na.omit(dati$c2),
                      100,
                      stdev =  15,
                      alternative = "two",
                      conf.level = 0.95)


#t-test

plot(density(rt(100000,5)),xlim=c(-4,4))
lines(density(rt(100000,10)),col=2)
lines(density(rt(100000,30)),col=3)
lines(density(rt(100000,100)),col=4)
abline(v=qt(0.025,100))

qt(0.025,5)
qt(0.025,20)
qt(0.025,30)
qt(0.025,100)
qt(0.025,1000)
qnorm(0.025)


campione_TC <- c("testa","testa","testa","croce","testa",
                 "croce","croce","testa","croce","testa")
campione_TC <- ifelse(campione_TC=="croce",1,0) 
campione_TC
mean(campione_TC)


qt(0.025,9)


(0.4 - 0.5) / sqrt( ( (0.5)*(1-0.5))/10 ) #COME LO FACCIO IO
2*pnorm(-abs(0.63))


0.4-(2.26 * sqrt( (0.5*(1-0.5))/10) )
0.4+(2.26 * sqrt( (0.5*(1-0.5))/10) )


t.test(campione_TC, 
       mu = 0.5,
       conf.level = 0.95, 
       alternative = "two")


(0.4 - 0.5)/sqrt( ( (0.4)*(1-0.4))/9 ) #COME LO FA R



moneta<-c(1,0)
lanci<-sample(moneta, 100, prob=c(0.2,0.8), replace = T)
lanci
t.test(lanci,mu=0.5)


data("iris")

boxplot(Sepal.Width~Species)

pairwise.t.test(Sepal.Width, Species, 
                paired = F,
                pool.sd = T,
                p.adjust.method = "none")


boxplot(InsectSprays$count~InsectSprays$spray)
pairwise.t.test(InsectSprays$count,InsectSprays$spray)

pairwise.wilcox.test(InsectSprays$count,InsectSprays$spray)

data("iris")
attach(iris)



