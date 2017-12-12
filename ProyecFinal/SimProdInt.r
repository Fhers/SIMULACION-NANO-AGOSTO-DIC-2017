
# Producto Integrador
#Fernando Gonzalez
#Clase de simulacion de Nanoestructuras
# Tribocarga de dos materiales
tinicial<-Sys.time()

library(parallel)

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

mA <- read.csv("matrizA", sep=" ", header=FALSE, stringsAsFactors=F)
mB <- read.csv("matrizB", sep=" ", header=FALSE, stringsAsFactors=F)


#k es la carga

tiempo <- 0:50/50

matplot(tiempo, outer(tiempo, 1:5, function(tiempo, CARGA) sin(CARGA*pi * tiempo)),
        
        ylim = c(-1.2,1.2), type = "plobcsSh",
        main= "Intensidad de carga Triboelectrica")


matplot((-4:5)^2, main = "Cuadro 1") 

# Graficar señala distinas capas

voltaje <- "outer"(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))

matplot(voltaje, pch = 1:4, type = "o", col = rainbow(ncol(voltaje)))
matplot(voltaje, type = "b", pch = 21:23, col = 2:5, bg = 2:5,
        main = "Intencidad de carga por Capas")


library(rgl)

source('~/Prod.Int. Simulacion/ProyecFinal/matrizA.csv', encoding = 'UTF-8')
source('~/Prod.Int. Simulacion/ProyecFinal/matrizB.csv', encoding = 'UTF-8')

#simulacion del fenomeno en 3d

x <- sort(rnorm(1000000))
y <- rnorm(1000000)
z <- rnorm(1000000) + atan2(x, y)

open3d()

plot3d(x, y, z, col = rainbow(1000000))

tfinal<-Sys.time()

TiempoTotal<-(tfinal-tinicial)
print(TiempoTotal)