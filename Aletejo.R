# Graficar todas las series temporales de todos los puntos en un único panel ---- 
source("utils/Configuration.R")
source("utils/SolUtilities.R")
require(ggplot2)
  input <- "/home/david/TrLab/test/test.sol"
##***Lectura del fichero .sol
props <- read.properties(input)
##***Obtener información del dataset
datasetInfo <- getDataset(props$dataset)
#****Obtener los paths de las listas de genes, condiciones y tiempos
paths <- getGSTtags(datasetInfo)
#****Obtener las etiquetas de genes, condiciones y tiempos en forma de vector
genesL <- as.vector(read.table(paths["genes"],sep = "\n")$V1)
samplesL <- as.vector(read.table(paths["samples"],sep = "\n")$V1)
timesL <- as.vector(read.table(paths["times"],sep = "\n")$V1)
#****Obtener valores del dataset
dataset <- getDatasetValues(datasetInfo)
#****Obtener los puntos [gen,condición,tiempo,expresión génica] de cada solución
solutions <-getTriclusters(props,dataset)
for (tri in solutions){
  tri$p <- paste0("(",tri$s,",",tri$g,")")
   gr<-ggplot(tri, aes(x=t,y=el,color=p))+
    geom_line()+ 
    theme_minimal() + 
    theme(legend.position = "none",
          panel.border = element_rect(color="black", fill=NA), 
          strip.background = element_rect(fill=NA, color="black"))
   print(gr)
}


tri <- solutions[[1]]
tri$p <- paste0("(",tri$s,",",tri$g,")")


ggplot(tri, aes(x=t,y=el,color=p))+
  geom_line()
ggplot(tri, aes(x = t,y = el,group=g)) +
  geom_line() +
  facet_grid(s~.,labeller = label_both)

  # theme_minimal() + 
  # theme(panel.border = element_rect(color="black", fill=NA), 
  #       strip.background = element_rect(fill=NA, color="black"),
  #       axis.title = element_text(size = 5, face = "bold"),
  #       axis.text = element_text(size = 4),
  #       strip.text = element_text(size = 5),
  #       axis.text.x = element_text(angle = 90, hjust = 0.5)




# Construir dataset de puntos para clustering (k-means) a partir de la imágenes  ---- 
folder<-"/home/cluster/Escritorio/Milho"
paths<- dir(folder, full.names = T)
nt <- length(paths)

dfs <- list()
for (i in c(1:nt)){
  dfs[[i]] <- read.csv(paths[i],header=F,sep=";")
}

nc <- ncol(dfs[[1]])
nr <- nrow(dfs[[1]])

r<-setNames(data.frame(matrix(ncol = nt, nrow = 0)),c("1":"19"))

for (i in c(1:nr)){
  for (j in c(1:nc)){
    aux<-list()
    for (t in c(1:nt)){
      aux[[t]]<-dfs[[t]][i,j]
    }
    ra <- data.frame(aux)
    colnames(ra)<-c("1":"19")
    r<-rbind(r,ra)
    print(paste0("i= ", i," j= ",j))
    print(head(ra))
  }
}

write.csv(r,file="/home/cluster/Escritorio/MilhoDataset.csv",col.names = F,row.names = F,sep=";")
