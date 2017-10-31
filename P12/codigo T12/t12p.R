#practica 12
Timeinicial=Sys.time()
code <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g']<- 0.92
modelos[modelos=='b']<- 0.002

r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)

###rownames(contadores) <- 0:tope
###colnames(contadores) <- c(0:tope, NA)
n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  cuadro <- runif(dim) < modelos[d + 1,]
  correcto <- code(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    total <- sum(w * cuadro) >= 0
    if (deseada != total) {
      ajuste <- tasa * (deseada - total)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * cuadro
    }
  }
}
simul<-function(){
  d <- sample(0:tope, 1)
  cuadro <- runif(dim) < modelos[d + 1,] 
  correcto <- code(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    total <- sum(w * cuadro) >= 0
    salida[i] <- total
  }
  r <- min(decimal(salida, n), k) 
  # todos los no-existentes van al final
  if (r==d){
    return(TRUE)
  }
}

aciertos<-foreach(t=1:a, .combine = c) %dopar% simul()

AciertosP<-round((sum(aciertos)/a)*100,2)
print(paste("% de Acierto",AciertosP,"%"))
Timefinal=Sys.time()
Tiempo=Timefinal-Timeinicial
print(Tiempo)