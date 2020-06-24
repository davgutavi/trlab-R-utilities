source("utils/general_graphs.R")

# ---- Mapa con todos los clusters (solapados)
Xsize <- 27
Ysize <- 17
path<-"/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra30/e01/e01.sol"

map <- buildMap(path,Xsize,Ysize)
print(map)



experiment <-loadExperiment(path)
solutions <- experiment$solutions

general <- data.frame(x=0,y=seq(0,Ysize-1),v=0)
for (i in c(1:(Xsize-1))){
  general<-rbind(general,data.frame(x=i,y=seq(0,Ysize-1),v=0))
}

clusters<-list()
clusters[[1]] <- cbind(unique(data.frame(x=solutions[[1]]$s,y=solutions[[1]]$g)),v=1)

for(i in c(1:length(solutions))){
  clusters[[i]] <-cbind(unique(data.frame(x=solutions[[i]]$s,y=solutions[[i]]$g)),v=i)
}

gdata <- list()
for(j in c(1:length(clusters))){
  clus <- clusters[[j]]
  for (i in c(1:nrow(clus))){
    general[which(general$x==clus[i,]$x&general$y==clus[i,]$y),3]=clus[i,]$v
  }
}




general$x<- as.character(general$x)
general$y<- as.character(general$y)
general$v<- as.character(general$v)

ggplot(general, aes(x = x, y = y, fill=v, colour=v)) + 
  geom_tile() + 
  scale_x_discrete(limits = unique(general$x))+
  scale_y_discrete(limits = unique(general$y))+
  labs(x="X", y="Y", title="Map") 

