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

# Nuevos mapas con zonas pequeñas 
outPutPath <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/mapas/nuevos"
pintarTotales29<-function(current,w=40,h=40){
  sol29 <- paste0(root29,"/",current,"/",current,".sol")
  title29 <- paste0("29-",current)
  eps29 <- paste0("a29_",current,".eps")
  pdf29 <- paste0("a29_",current,".pdf")
  
  pl29<-cluster_total_plot_list(sol29,tot29Path,Xsize,Ysize,title29)
  maps29<-marrangeGrob(pl29,nrow=2, ncol=2,top=title29)
  ggsave(pdf29,maps29,"pdf",outPutPath,width=w,height=h,units="cm")

  #ggsave(eps29,maps29,"eps",outPutPath,width=w,height=h,units="cm")
  #grid.draw(maps29)
}

test<-cluster_total_plot_list("/Users/davgutavi/Desktop/test4/test4.sol",tot29Path,Xsize,Ysize,"Test")
testmap<-marrangeGrob(test,nrow=2, ncol=2,top="Test")
ggsave("test4.pdf",testmap,"pdf","/Users/davgutavi/Desktop",width=40,height=40,units="cm")




# Ficheros de coordenadas ----
outPutPath <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/mapas/coordenadas"
ficherosCoordenadas<-function(current,w=40,h=40){
  sol29 <- paste0(root29,"/",current,"/",current,".sol")
  get_coordinate_files(sol29,outPutPath)
  sol30 <- paste0(root30,"/",current,"/",current,".sol")
  get_coordinate_files(sol30,outPutPath)
}
ficherosCoordenadas("t01_p01")
ficherosCoordenadas("t01_p02")
ficherosCoordenadas("t01_p03")
ficherosCoordenadas("t01_p04")
ficherosCoordenadas("t01_p05")
ficherosCoordenadas("t02_p01")
ficherosCoordenadas("t02_p02")
ficherosCoordenadas("t02_p03")
ficherosCoordenadas("t02_p04")
ficherosCoordenadas("t02_p05")
ficherosCoordenadas("t03_p01")
ficherosCoordenadas("t03_p02")
ficherosCoordenadas("t03_p03")
ficherosCoordenadas("t03_p04")
ficherosCoordenadas("t03_p05")
ficherosCoordenadas("t04_p01")
ficherosCoordenadas("t04_p02")
ficherosCoordenadas("t04_p03")
ficherosCoordenadas("t04_p04")
ficherosCoordenadas("t04_p05")
ficherosCoordenadas("t05_p01")
ficherosCoordenadas("t05_p02")
ficherosCoordenadas("t05_p03")
ficherosCoordenadas("t05_p04")
ficherosCoordenadas("t05_p05")
ficherosCoordenadas("t06_p01")
ficherosCoordenadas("t06_p02")
ficherosCoordenadas("t06_p03")
ficherosCoordenadas("t06_p04")
ficherosCoordenadas("t06_p05")
ficherosCoordenadas("t07_p01")
ficherosCoordenadas("t07_p02")
ficherosCoordenadas("t07_p03")
ficherosCoordenadas("t07_p04")
ficherosCoordenadas("t08_p01")


# Mapas con totales ----
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
  
  pl29<-cluster_total_plot_list(sol29,tot29Path,Xsize,Ysize,title29)
  maps29<-marrangeGrob(pl29,nrow=2, ncol=2,top=title29)
  ggsave(pdf29,maps29,"pdf",outPutPath,width=w,height=h,units="cm")
  
  pl30<-cluster_total_plot_list(sol30,tot30Path,Xsize,Ysize,title30)
  maps30<-marrangeGrob(pl30,nrow=2, ncol=2,top=title30)
  ggsave(pdf30,maps30,"pdf",outPutPath,width=w,height=h,units="cm")
  
  #ggsave(eps29,maps29,"eps",outPutPath,width=w,height=h,units="cm")
  #ggsave(eps30,maps30,"eps",outPutPath,width=w,height=h,units="cm")
  #grid.draw(maps29)
  #grid.draw(maps30)
}
pintarTotales("t01_p01")
pintarTotales("t01_p02")
pintarTotales("t01_p03")
pintarTotales("t01_p04")
pintarTotales("t01_p05")
pintarTotales("t02_p01")
pintarTotales("t02_p02")
pintarTotales("t02_p03")
pintarTotales("t02_p04")
pintarTotales("t02_p05")
pintarTotales("t03_p01")
pintarTotales("t03_p02")
pintarTotales("t03_p03")
pintarTotales("t03_p04")
pintarTotales("t03_p05")
pintarTotales("t04_p01")
pintarTotales("t04_p02")
pintarTotales("t04_p03")
pintarTotales("t04_p04")
pintarTotales("t04_p05")
pintarTotales("t05_p01")
pintarTotales("t05_p02")
pintarTotales("t05_p03")
pintarTotales("t05_p04")
pintarTotales("t05_p05")
pintarTotales("t06_p01")
pintarTotales("t06_p02")
pintarTotales("t06_p03")
pintarTotales("t06_p04")
pintarTotales("t06_p05")
pintarTotales("t07_p01")
pintarTotales("t07_p02")
pintarTotales("t07_p03")
pintarTotales("t07_p04")
pintarTotales("t08_p01")

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
