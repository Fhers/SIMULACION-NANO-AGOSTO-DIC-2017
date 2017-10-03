Tinicial=Sys.time()
library(doParallel)
#registerdoPrallel(makeCluster(detectores() - 1))

library(testit) # para pruebas, recuerda instalar antes de usar
k <- 10000
n <- 1000000
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
assert(min(cumulos) > 0)
diferencia <- n - sum(cumulos)
if (diferencia > 0) {
  for (i in 1:diferencia) {
    p <- sample(1:k, 1)
    cumulos[p] <- cumulos[p] + 1
  }
} else if (diferencia < 0) {
  for (i in 1:-diferencia) {
    p <- sample(1:k, 1)
    if (cumulos[p] > 1) {
      cumulos[p] <- cumulos[p] - 1
    }
  }
}
assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
assert(sum(cumulos) == n)
print(sum(cumulos))
c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva



rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}

union <- function(x) {
  return (exp(-x / c))
}

romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { 
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  stopifnot(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}

unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    stopifnot(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}
freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 5
digitos <- floor(log(duracion, 10)) + 1

#Sintaxis para Funcion romper

rompiendo <- function(){
  cumulos <- integer()
  urna <- freq[i,]
  if (urna$tam > 1) { # no tiene caso romper si no se puede
    cumulos <- c(cumulos, romperse(urna$tam, urna$num))
  } else {
    cumulos <- c(cumulos, rep(1, urna$num))
  }
  return(cumulos) 
}
#Sintaxis para Función Uniendo

uniendo <- function(){
  cumulos <- integer()
  urna <- freq[i,]
  cumulos <- c(cumulos, unirse(urna$tam, urna$num))
  return(cumulos)
}

uniendose<- function(){
  #for (i in 1:floor(nt / 2) ) {
  tcumulos <- juntarse[2*i-1] + juntarse[2*i]
  #}
  return(tcumulos)
}

for (paso in 1:duracion) {
  
  #comienza a Rompiendo  
  
  assert(sum(cumulos) == n)
  
  
  cumulos=foreach(i=1:dim(freq)[1], .combine = c) %dopar% rompiendo ()
  
  
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  
  # comienza a Uniendo
  cumulos=foreach(i=1:dim(freq)[1], .combine = c) %dopar% uniendo ()
  
  
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  juntarse <- -cumulos[cumulos < 0]
  positivos <- cumulos[cumulos > 0]#
  assert(sum(positivos) + sum(juntarse) == n)#
  nt <- length(juntarse)
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      
      
      cu=foreach(i=1:floor(nt / 2),.combine = c)%dopar% uniendose()
      cumulos<-c(positivos,cu)
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cu,positivos, juntarse[nt])
      
    }
  }
  
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  tl <- paste(paso, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p8p_ct", tl, ".png", sep=""), width=300, height=300)
  tope <- 50 * ceiling(max(cumulos) / 50)
  hist(cumulos, 
  breaks=seq(0, tope, 50), 
       main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
      ylim=c(0, 0.3), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
  graphics.off()
  
  
}
stopImplicitCluster() 
Tfinal=Sys.time()
TiempoT=Tfinal-Tinicial
print(TiempoT)