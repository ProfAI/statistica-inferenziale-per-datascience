#teorema del limite centrale
set.seed(1)
sample.int(100,3)

reddito <- rchisq(100000,30)
plot(density(reddito))
summary(reddito)
moments::skewness(reddito)

mu_vero <- mean(reddito)
sigma_vero <- sd(reddito)
mu_vero
sigma_vero
abline(v=mu_vero)


campione <- sample(reddito, 30)
mean(campione)
abline(v=mean(campione),col=2)


medie <- c(NA)
dev.std <- c(NA)
#5,10,100   5
#30    10,20,100
#10000   10000    
N.campioni = 5
n = 5

set.seed(2)
for (i in 1:N.campioni){

campione <- sample(reddito, n)
medie[i] <- mean(campione) #fare prova con mediana
dev.std[i] <- sd(campione)

}

plot(density(medie))

mean(medie); mu_vero
mean(dev.std); sigma_vero

std.err <- sd(medie)/sqrt(n)
std.err
