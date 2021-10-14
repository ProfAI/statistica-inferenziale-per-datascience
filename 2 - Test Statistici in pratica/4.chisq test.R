tabella <- matrix(data = c(45,22,32,12,
                           53,24,21,30,
                           24,65,40,3,
                           12,43,2,2,
                           20,13,7,0),
                  nrow = 5,
                  ncol = 4,
                  byrow = T)


colnames(tabella)<- c("0","1","2","3+")

row.names(tabella) <- c("analfabeta",
                        "elementare",
                        "media",
                        "superiore",
                        "universitaria")

install.packages("ggpubr")
ggpubr::ggballoonplot(data=as.data.frame(tabella),
                      fill="blue")


margin.table(tabella,1)
margin.table(tabella,2)
margin.table(tabella)

attese<- tabella

for(i in 1:nrow(tabella)){
  for(j in 1:ncol(tabella)){
    
    attese[i,j] <- margin.table(tabella,1)[i] * margin.table(tabella,2)[j] / margin.table(tabella)
    
  }
}

osservate<-tabella


sum((osservate-attese)^2/attese)

plot(density(rchisq(1000000,df = 12)),xlim=c(0,130))
abline(v=qchisq(0.95,12),col=2)
points(123.89,0,cex=3,col=4)

test.indipendenza<- chisq.test(tabella)


test.indipendenza$expected



dati<-HairEyeColor[1,,]
ggpubr::ggballoonplot(data=as.data.frame(dati),
                      fill="blue")
chisq.test(dati)







tabella <- as.table(tabella)

countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}


dati_long <- countsToCases(as.data.frame(tabella))
colnames(dati_long)<-c("Istruzione","N.figli")
chisq.test( table(dati_long))

cor.test(as.numeric(dati_long$Istruzione),
    as.numeric(dati_long$N.figli),
    method = "kendall")

