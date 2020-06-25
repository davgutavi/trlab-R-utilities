source("utils/general_graphs.R")

# Configuración general ----
root30 <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra30"
root29 <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra29"
Xsize <- 27
Ysize <- 17

e01_29 <- paste0(root29,"/experiment_01/experiment_01.sol")
e02_29 <- paste0(root29,"/experiment_02/experiment_02.sol")
e03_29 <- paste0(root29,"/experiment_03/experiment_03.sol")
e04_29 <- paste0(root29,"/experiment_04/experiment_04.sol")
e05_29 <- paste0(root29,"/experiment_05/experiment_05.sol")

e01_30 <- paste0(root30,"/e01/e01.sol")
e02_30 <- paste0(root30,"/e02/e02.sol")
e03_30 <- paste0(root30,"/e03/e03.sol")

# Mapas 29 ----
maps1<-cluster_maps(e01_29,Xsize,Ysize,"29-e01")
ggsave("29-e01.eps",maps1,"eps",root29)

maps2<-cluster_maps(e02_29,Xsize,Ysize,"29-e02")
print(maps2)
maps3<-cluster_maps(e03_30,Xsize,Ysize,"29-e03")
print(maps3)
maps4<-cluster_maps(e04_29,Xsize,Ysize,"29-e04")
print(maps4)
maps5<-cluster_maps(e05_29,Xsize,Ysize,"29-e05")
print(maps5)




# Mapas 30 ----
maps1<-cluster_maps(e01_30,Xsize,Ysize,"30-e01")
print(maps1)
maps3<-cluster_maps(e03_30,Xsize,Ysize,"30-e03")
print(maps3)
