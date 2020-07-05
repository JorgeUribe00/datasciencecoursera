#Programa primer wuu

#Cargar los primeros
#Tot<-matrix(0,0,4)
for (i in 1:9){
  A<-read.csv(paste("0","0",i,".csv", sep=""))
  Tot <- rbind(Tot,A)
}
#Cargar los primeros 100
for (i in 10:99){
  A<-read.csv(paste("0",i,".csv", sep=""))
  Tot <- rbind(Tot,A)
}
#Cargar los ultimos 300
for (i in 100:300){
  A<-read.csv(paste(i,".csv", sep=""))
  Tot <- rbind(Tot,A)
}

pollutionmean <- function(contaminante, rango, removeNA=TRUE){
  polm <- matrix(0,0,4)
  for (i in rango){
    Temporal <- Tot[Tot$ID==100,]
    polm <- rbind(polm, Temporal)
  }
  mean(polm[,contaminante], na.rm=T)
}



columnmean <- function(y, removeNA=TRUE){
  nc <- ncol(y)
  means <- numeric(nc)
  for (i in 1:nc) {
    means[i] <- mean(y[,i], na.rm=removeNA)
  }
  means
}


B<-Tot$ID==300
Tot[B,]



