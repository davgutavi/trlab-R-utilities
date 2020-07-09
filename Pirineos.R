source("LoadEnvironment.R")
source("utils/general_graphs.R")
require(gtable)

# Configuración general  ----
Xsize <- 27
Ysize <- 17
root30 <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra30"
root29 <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra29"
tot29Path <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/dataset/totales_29.csv"
tot30Path <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/dataset/totales_30.csv"
 
# Mapas blanco y negro ----
outPutPath <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/mapas/t08" 
pintarByN<-function(current,w=40,h=40){
  sol29 <- paste0(root29,"/",current,"/",current,".sol")
  title29 <- paste0("29-",current)
  eps29 <- paste0("a29_",current,".eps")
  pdf29 <- paste0("a29_",current,".pdf")
  sol30 <- paste0(root30,"/",current,"/",current,".sol")
  title30 <- paste0("30-",current)
  eps30 <- paste0("a30_",current,".eps")
  pdf30 <- paste0("a30_",current,".pdf")
  
  maps29<-cluster_maps(sol29,Xsize,Ysize,title29)
  ggsave(eps29,maps29,"eps",outPutPath,width=w,height=h,units="cm")
  ggsave(pdf29,maps29,"pdf",outPutPath,width=w,height=h,units="cm")
  #grid.draw(maps29)
  
  maps30<-cluster_maps(sol30,Xsize,Ysize,title30)
  ggsave(eps30,maps30,"eps",outPutPath,width=w,height=h,units="cm")
  ggsave(pdf30,maps30,"pdf",outPutPath,width=w,height=h,units="cm")
}

# Totales  ----
outPutPath <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/mapas/totales"
pintarTotales<-function(current,w=40,h=40){
  sol29 <- paste0(root29,"/",current,"/",current,".sol")
  title29 <- paste0("29-",current)
  eps29 <- paste0("a29_",current,".eps")
  pdf29 <- paste0("a29_",current,".pdf")
  sol30 <- paste0(root30,"/",current,"/",current,".sol")
  title30 <- paste0("30-",current)
  eps30 <- paste0("a30_",current,".eps")
  pdf30 <- paste0("a30_",current,".pdf")
  
  maps29<-cluster_maps_total(sol29,tot29Path,Xsize,Ysize,title29)
  #ggsave(eps29,maps29,"eps",outPutPath,width=w,height=h,units="cm")
  ggsave(pdf29,maps29,"pdf",outPutPath,width=w,height=h,units="cm")
  #grid.draw(maps29)
  
  maps30<-cluster_maps_total(sol30,tot30Path,Xsize,Ysize,title30)
  #ggsave(eps30,maps30,"eps",outPutPath,width=w,height=h,units="cm")
  ggsave(pdf30,maps30,"pdf",outPutPath,width=w,height=h,units="cm")
}

# Pruebas rápidas ----
pintarTotales("t07_p01")
pintarTotales("t07_p02")
pintarTotales("t07_p03")
pintarTotales("t07_p04")
pintarTotales("t08_p01")





solPath <- paste0(root29,"/t04_p02/t04_p02.sol")
patterns <- buildCellPatternPlots(solPath)
patterns[[1]]




pintarByN("t08_p01")
plotPatterns(paste0(root29,"/t07_p04/t07_p04.sol"))



# Pruebas en bloque ----
e01_29 <- paste0(root29,"/experiment_01/experiment_01.sol")
e02_29 <- paste0(root29,"/experiment_02/experiment_02.sol")
e03_29 <- paste0(root29,"/experiment_03/experiment_03.sol")
e04_29 <- paste0(root29,"/experiment_04/experiment_04.sol")
e05_29 <- paste0(root29,"/experiment_05/experiment_05.sol")

e01_30 <- paste0(root30,"/e01/e01.sol")
e02_30 <- paste0(root30,"/e02/e02.sol")
e03_30 <- paste0(root30,"/e03/e03.sol")
e04_30 <- paste0(root29,"/e04/e04.sol")
e05_30 <- paste0(root29,"/e05/e05.sol")
# Mapas 29 ----
maps<-cluster_maps(e01_29,Xsize,Ysize,"29-e01")
ggsave("29-e01.eps",maps,"eps",outPutPath)
ggsave("29-e01.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e02_29,Xsize,Ysize,"29-e02")
ggsave("29-e02.eps",maps,"eps",outPutPath)
ggsave("29-e02.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e03_30,Xsize,Ysize,"29-e03")
ggsave("29-e03.eps",maps,"eps",outPutPath)
ggsave("29-e03.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e04_29,Xsize,Ysize,"29-e04")
ggsave("29-e04.eps",maps,"eps",outPutPath)
ggsave("29-e04.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e05_29,Xsize,Ysize,"29-e05")
ggsave("29-e05.eps",maps,"eps",outPutPath)
ggsave("29-e05.pdf",maps,"pdf",outPutPath)

# Mapas 30 ----
maps<-cluster_maps(e01_30,Xsize,Ysize,"30-e01")
ggsave("30-e01.eps",maps,"eps",outPutPath)
ggsave("30-e01.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e02_30,Xsize,Ysize,"30-e02")
ggsave("30-e02.eps",maps,"eps",outPutPath)
ggsave("30-e02.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e03_30,Xsize,Ysize,"30-e03")
ggsave("30-e03.eps",maps,"eps",outPutPath)
ggsave("30-e03.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e04_30,Xsize,Ysize,"30-e04")
ggsave("30-e04.eps",maps,"eps",outPutPath)
ggsave("30-e04.pdf",maps,"pdf",outPutPath)

maps<-cluster_maps(e05_30,Xsize,Ysize,"30-e05")
ggsave("30-e05.eps",maps,"eps",outPutPath)
ggsave("30-e05.pdf",maps,"pdf",outPutPath)
