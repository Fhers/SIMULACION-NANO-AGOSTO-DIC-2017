#Practica 10
# Algoritmo Genetico


library(testit)
library(parallel) # Libreria para paralelizar

knapsack <- function(cap, peso, valor) 
  {
  n <- length(peso)
  pt <- sum(peso) # deben ser enteros en este caso
  assert(n == length(valor))
  vt <- sum(valor) # pueden ser lo que sea
  if (pt < cap) {  # cabe todo
    return(vt)
  } 
  else 
    {
    filas <- cap + 1   # una para cada posible peso acumulado desde cero hasta cap
    cols <- n + 1  # una para cada objeto y una extra al inicio para no llevar nada
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) # al inicio todo vale negativo infinito
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 # todas las filas tienen un cero al inicio (no llevar nada da cero valor)
    }
    rownames(tabla) <- 0:cap # filas corresponden a pesos acumulados posibles
    colnames(tabla) <- c(0, valor) # columnas corresponden a objetos considerados
    for (objeto in 1:n) {  # consideremos a cada objeto por turno
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) #buscar la opcion factible
  {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores)
  {
  return(sum(seleccion * valores))
}
##### se crea la funcion data
normalizar <- function(data) 
  {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max)
  {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + max
  return(valores)
}

poblacion.inicial <- function(n, tam) {   # tamaño poblacion inicial
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutar <- function(sol, n) 
  {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n)
  {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  
  return(c(xy, yx))
}

evolucion <- function(i)
  {
  if (runif(1) < pm) {
    a <- mutar(p[i,], n)
    return(a)
  }
  else{
    return(NULL)
  }
}
procrear <- function(i)
  {
  papas <- sample(1:tam, 2, replace=FALSE)
  hijo <- reproduccion(p[papas[1],], p[papas[2],], n)
  # se procede a la reporduccion
  a <- hijo[1:n] # hijo 1
  b <- hijo[(n+1):(2*n)] # hijo 2
  c <- rbind(a, b)
  
  return(as.data.frame(c))
}
objetivos <- function(i)
  {
  return(objetivo(p[i,], valores))
}
probables.t <- function(i)
  {
  return(factible(p[i,], pesos, capacidad))
}
n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
init <- 200
p <- poblacion.inicial(n, init)
tam <- dim(p)[1]
assert(tam == init)
pm <- 0.05
rep <- 50
tmax <- 200
mejores <- double()

#################################
cluster <- makeCluster(detectCores()- 1)
clusterExport(cluster, c("procrear", "reproducir", "objetivos", "valores", "pesos", "capacidad" ,"probables.t", "factible", "objetivo", "pm", "mutar", "reproduccion", "n"))
################################
for (iter in 1:tmax) {
  p$obj <- NULL
  p$fact <- NULL
  
  clusterExport(cluster, "p")
  
  muta <- parSapply(cluster, 1:tam, procrear)
  for(i in 1:length(muta))
    { 
    if(!is.null(muta[[i]]))
      {
      p <- rbind(p, muta[[i]])
    }
  }
  ########### 
  clusterExport(cluster, c("p", "tam"))
  h <- parSapply(cluster, 1:repetir , reproducir)
  for(i in 1:repetir)
    {
    a <- as.data.frame(h[,i])
    b <- a[1, 1:50]
    colnames(b) <- names(p)
    c <- a[1, 51:200]
    colnames(c) <- names(p)
    p <- rbind(p, b)
    p <- rbind(p, c)
  }
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  ###########
  clusterExport(cluster, c("p", "tam"))
  obj <- parSapply(cluster, 1:tam, objetivos)
  fact <- parSapply(cluster, 1:tam, FUN = factibles.f)
  p <- cbind(p, obj)
  p <- cbind(p, fact)
  
  contiene <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  p <- p[contiene,]
  tam <- dim(p)[1]
  assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
}
stopCluster(cluster)
png("p10-1.png", width=600, height=300)
plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
points(1:tmax, mejores, pch=15)
abline(h=optimo, col="green", lwd=3)
graphics.off()
print(paste(mejor, (optimo - mejor) / optimo))