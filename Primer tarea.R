#Programa primer wuu

#Cargar los primeros
Tot<-matrix(0,0,4)
for (i in 1:9){
  A<-read.csv(paste("0","0",i,".csv", sep=""))
  Tot <- rbind(Tot,A)
  rm(A)
}
#Cargar los primeros 100
for (i in 10:99){
  A<-read.csv(paste("0",i,".csv", sep=""))
  Tot <- rbind(Tot,A)
  rm(A)
}
#Cargar los ultimos 300
for (i in 100:332){
  A<-read.csv(paste(i,".csv", sep=""))
  Tot <- rbind(Tot,A)
  rm(A)
  }

pollutionmean <- function(contaminante, rango=1:332, removeNA=TRUE){
  polm <- matrix(0,0,4)
  for (i in rango){
    polm <- rbind(polm, Tot[Tot$ID==i,])
  }
  mean(polm[,contaminante], na.rm=removeNA)
}
complete <- function(rango=1:332){
  m <- matrix(0,0,2)
  colnames(m) <- c("id","nobs")
  for (i in rango){
    #DEBES AGARRAR O SEPARAR DEL PRINCIPAL TODO
    MLQ <- Tot$ID==i
    DE <- Tot[MLQ,]
    #REMOVER LOS NA
    nq <- is.na(DE$sulfate)
    es <- DE[!nq,]
    #REMOVER MAS NA
    nq2 <- is.na(es$nitrate)
    es2 <- es[!nq,]
    m1 <- c(i, length(es2$Date))
    m <- rbind(m,m1)
  }
  print(m)
}


corr <- function(threshold=0){
  m <- matrix(0,0,2)
  colnames(m) <- c("id","nobs")
  for (i in 1:332){
    MLQ <- Tot$ID==i
    DE <- Tot[MLQ,]
    nq <- is.na(DE$sulfate)
    es <- DE[!nq,]
    nq2 <- is.na(es$nitrate)
    es2 <- es[!nq,]
    m1 <- c(i, length(es2$Date))
    m <- rbind(m,m1)
  }
  th<-m[,2]>threshold
  m[th,1]
  mpc <- matrix(0,0,2)
  colnames(mpc) <- c("sulfate","nitrate")
  vcor=0
  for (i in m[th,1]){
    mpc <- rbind(mpc, Tot[Tot$ID==i,])
    #QUITAR LOS NA DE CADA UNO DE LAS LISTAS
    prov <- Tot[Tot$ID==i,]
    nas <- is.na(prov$sulfate)
    snas <- prov[!nas,]
    nan <- is.na(snas$nitrate)
    snan <- snas[!nan,]
    vcor <- c(vcor, cor(snan$sulfate,snan$nitrate))
  }
  vcor <- vcor[!(is.na(vcor))]
  return(vcor[2:length(vcor)])
}



cr <- corr()
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


length(cr)



cr <- corr(129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr(2000)                
n <- length(cr)     

cr <- corr(1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))







