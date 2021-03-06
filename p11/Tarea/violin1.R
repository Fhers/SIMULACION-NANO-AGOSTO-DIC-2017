suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Datos<-data.frame()
Tp<-numeric()

for(corrida in 1:50) {
  for(k in seq(2, 14, 2)){
    for(n in c(300)){
      
      source('~/p11/Tarea/pareto.R', encoding = 'UTF-8')
      
      Tp <- cbind(dim(frente)[1], k, n)
      
      
      Datos<-  rbind(Datos, Tp)
    }  
  }
}
stopImplicitCluster()

colnames (Datos)= c("Dominantes","Objetivos", "Soluciones")
Datos$Objetivos=as.factor(Datos$Objetivos)

library(ggplot2)
#png("p11T_violinplot3.png",width = 700, height = 350)
ggplot(data=Datos, aes(x=Objetivos, y=(Dominantes/n)*100)) + 
  geom_violin(scale="width",fill="darkolivegreen1", color="black")+
  geom_boxplot(width=0.2, fill="dodgerblue3", color="darkblue")+ 
  xlab("Funcion objetivo") +
  ylab("Porcentaje de solucion")+
  ggtitle("Cantidad de solucion")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p11T_violin1.png",width = 10, height = 5)