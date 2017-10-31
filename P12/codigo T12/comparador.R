for (a in seg(,500,1000)){
  for (r in 1:20){
    source('~/P12/P12/codigo T12/t12p.R', encoding = 'UTF-8')
    Tpara-cbind(a,r,"o",Tiempo,Acierto)
    
    source('~/P12/P12/codigo T12/t12orig.R', encoding = 'UTF-8')
    Torig-cbind(a,r,"o",Tiempo,Acierto)
    total-rbind(total,Tpara,Torig)
  }
}