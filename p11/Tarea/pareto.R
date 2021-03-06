#Practica 11
#simulacion

library(parallel)

pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount)
{
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms)
{
  value <- 0.0
  for (t in 1:terms) 
  {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) 
{
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster,c("poli","pick.one","eval","domin.by"))

vc <- 4
md <- 3
tc <- 5

clusterExport(cluster,c("vc","md","tc"))

k <- 2 # cuantas funciones objetivo
n <- 300 # cuantas soluciones aleatorias
datos=data.frame()
for(n in c(100,200,300,400,500)) {
  
  for(rep in 1:30){
    
    res <- matrix(runif(vc * n), nrow=n, ncol=vc)
    startP=Sys.time()
    
    obj <- parLapply(cluster,1:k,function(k)
    {return (poli( md, vc, tc))
    })
    
    minim <- (runif(k) > 0.5)
    var1 <- (1 + -2 * minim)
    
    
    clusterExport(cluster,c("k","n","obj","res","var1")) #determinar opciones
    num=matrix(parSapply(cluster,1:(k*n),function(pos){
      i <- floor((pos - 1) / k) + 1
      j <- ((pos - 1) %% k) + 1
      return(eval(obj[[j]], res[i,], tc))
    }), nrow=n, ncol=k, byrow=TRUE)
    
    clusterExport(cluster,"num")  # Localizar soluciones bajo nivel
    temp=parSapply(cluster,1:n,function(i)
    {
      d <- logical()
      for (j in 1:n) 
      {
        d <- c(d, domin.by(var1 * num[i,], var1 * num[j,], k))
      }
      cuantos <- sum(d)
      return(c(cuantos,cuantos==0))
    })
    dominadores = temp[1,]
    no.dom = as.logical(temp[2,])
    rm(temp)
    
    frente <- subset(num, no.dom) 
    
    endP=Sys.time()
    
    startS=Sys.time()
    num <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    for (i in 1:n)
    { 
      for (j in 1:k) 
      { 
        num[i, j] <- eval(obj[[j]], res[i,], tc)
      }
    }
    
    no.dom <- logical()
    dominadores <- integer()
    for (i in 1:n) {
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(var1 * num[i,], var1 * num[j,], k))
      }
      cuantos <- sum(d)
      dominadores <- c(dominadores, cuantos)
      no.dom <- c(no.dom, cuantos == 0) 
    }
    frente <- subset(num, no.dom) 
    
    endS=Sys.time()
    
    
    datos=rbind(datos,c(n=n,secuencial=difftime(endS,startS,units='secs'),paralelo=difftime(endP,startP,units='secs')))
  }
}
stopCluster(cluster)

names(datos)=c("n","Codigo en Secuencia","Codigo en Paralelo") 
datos1=datos[,1:2]
names(datos1)=c("n","tiempo") 
datos1$Método="Codigo en Secuencia" 
datos2=datos[,c(1,3)]
names(datos2)=c("n","tiempo") 
datos2$Método="Codigo en Paralelo" 
datos=rbind(datos1,datos2)

library(ggplot2)
png("Diferencia.png",width = 1200,height = 800)
ggplot(datos, aes(factor(n), tiempo)) + 
  geom_boxplot() + facet_grid(. ~ Método)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  ylab('Tiempo (seg)')+
  xlab('Cantidad de Soluciones')+
  labs(title='Comparación')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=26,angle = 90, hjust = 1),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32))
graphics.off()

