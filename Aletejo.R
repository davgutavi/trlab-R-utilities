# Graficar todas las series temporales de todos los puntos en un único panel ---- 
source("utils/GraphUtilities.R")



soco4gr <- buildTimeSeriesPlots("/home/david/soco04/soco04.sol","NDVI")



soco5gr <- buildTimeSeriesPlots("/home/david/soco05/soco05.sol","NDVI")
soco6gr <- buildTimeSeriesPlots("/home/david/soco06/soco06.sol","NDVI")
soco7gr <- buildTimeSeriesPlots("/home/david/soco07/soco07.sol","NDVI")

i<-1
for (g in soco5gr) {
  print(g)
  ggsave(paste0("soco5_",i),g,path="/home/david/Escritorio/PrecissionAgriculture/Plots",
         device = "eps",width = 8, height = 5)
  i<-i+1
}

i<-1
for (g in soco6gr) {
  print(g)
  ggsave(paste0("soco6_",i),g,path="/home/david/Escritorio/PrecissionAgriculture/Plots",
         device = "eps",width = 8, height = 5)
  i<-i+1
}
  
i<-1
for (g in soco7gr) {
  print(g)
  ggsave(paste0("soco7_",i),g,path="/home/david/Escritorio/PrecissionAgriculture/Plots",
         device = "eps",width = 8, height = 5)
  i<-i+1
}


























# Construir dataset de puntos para clustering (k-means) a partir de la imágenes  ---- 
# folder<-"/home/cluster/Escritorio/Milho"
# paths<- dir(folder, full.names = T)
# nt <- length(paths)
# 
# dfs <- list()
# for (i in c(1:nt)){
#   dfs[[i]] <- read.csv(paths[i],header=F,sep=";")
# }
# 
# nc <- ncol(dfs[[1]])
# nr <- nrow(dfs[[1]])
# 
# r<-setNames(data.frame(matrix(ncol = nt, nrow = 0)),c("1":"19"))
# 
# for (i in c(1:nr)){
#   for (j in c(1:nc)){
#     aux<-list()
#     for (t in c(1:nt)){
#       aux[[t]]<-dfs[[t]][i,j]
#     }
#     ra <- data.frame(aux)
#     colnames(ra)<-c("1":"19")
#     r<-rbind(r,ra)
#     print(paste0("i= ", i," j= ",j))
#     print(head(ra))
#   }
# }
# 
# write.csv(r,file="/home/cluster/Escritorio/MilhoDataset.csv",col.names = F,row.names = F,sep=";")
