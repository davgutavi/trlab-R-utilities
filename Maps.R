# Library
library(ggplot2)

folder<-"/Users/davgutavi/Desktop/Milho"
paths<- dir(folder, full.names = T)
nt <- length(paths)

dfs <- list()
for (i in c(1:nt)){
  dfs[[i]] <- read.csv(paths[i],header=F,sep=";")
}

nc <- ncol(dfs[[1]])
nr <- nrow(dfs[[1]])
np <- nx*ny

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
    print(head(r))
  }  
}


write.csv(r,file="/Users/davgutavi/Desktop/MilhoDataset.csv",col.names = F,row.names = F,sep=";")


aux<-list()
for (t in c(1:nt)){
  aux[[t]]<-dfs[[t]][1,1]
}
r<- data.frame(aux)
colnames(r)<-c("1":"19")


