source("LoadEnvironment.R")
source("utils/general_graphs.R")
# Configuración general  ----
Xsize <- 30
Ysize <- 20
totPath <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/dataset/pirineos/totales/pirineos29_tot.csv"
solPath <- "/Users/davgutavi/Desktop/test/test.sol"
totals <- read.csv(totPath,sep=";",header=F)
experiment <-loadExperiment(solPath)
solutions <- experiment$solutions
solution<-solutions[[1]]
datasetInfo <- experiment$dataset_info
paths <- getGSTtags(datasetInfo)
features_tags <- as.vector(read.table(paths["times"],sep = "\n")$V1)
paleta <- c("#7FFFD4","#FFE4C4","#8B7355","#7FFF00","#FF7F24","#00BFFF","#FFC0CB","#CD4F39","#A8A8A8","#6495ED","#FF4040","#FF1493","#0000EE","#00CD00","#8968CD","#CD00CD","#98F5FF","#006400","#FFFACD")
no_seleccionado <- "#000000"
solapamiento <- "#FFFFFF"

# Tests  ----
aux_insert_cluster_in_frame<-function(cluster, frame){
  
  for (i in c(1:nrow(cluster))){
    cv <- frame[which(frame$x==cluster[i,]$x&frame$y==cluster[i,]$y),3]
    if (length(cv)>0){
      if (cv!=0){
        frame[which(frame$x==cluster[i,]$x&frame$y==cluster[i,]$y),3]=-1
        frame[which(frame$x==cluster[i,]$x&frame$y==cluster[i,]$y),4]="#FFFFFF"
      }
      else{
        frame[which(frame$x==cluster[i,]$x&frame$y==cluster[i,]$y),3]=cluster[i,]$v
        frame[which(frame$x==cluster[i,]$x&frame$y==cluster[i,]$y),4]=cluster[i,]$c
      }
    }
  }
  
  return(frame)
}

aux_plot_frame<-function(frame,cluster_names,occurrences,palette,title=""){
  
  frame$x<- as.character(frame$x)
  frame$y<- as.character(frame$y)
  frame$v<- as.character(frame$v)
  
  color_map<-c("-1"="black","0"="white")
  for(n in cluster_names){
    i <- as.numeric(n)
    color_map<-c(color_map,palette[i])
    names(color_map)[length(color_map)]<-n
    
  }
  
  label_map<-c("-1"="Overlapping","0"="Not selected")
  for(n in cluster_names){
    oc <- as.numeric(n)
    label_map<-c(label_map,paste0("#",n," (",occurrences[oc],")"))
    names(label_map)[length(label_map)]<-n
  }
  
  gr <- ggplot(frame, aes(x, y)) +
    geom_tile(aes(fill = v), colour = "black")+
    scale_fill_manual(values=color_map,labels=label_map)+
    scale_x_discrete(limits = unique(frame$x),position = "top")+
    scale_y_discrete(limits = rev(unique(frame$y)))+
    labs(x="X", y="Y", title=title) +
    theme(legend.title = element_blank())
  
  return(gr)
  
}

plot_clusters_general_valid_small <- function(solutions, bias = 25){ 
  
  general_clusters <-list()
  valid_clusters <-list()
  small_clusters <- list()
  ocr <- c()
  vi <- 1
  si <- 1
  
  for(i in c(1:length(solutions))){
    oc <- cluster_ocurrences_value(solutions[[i]],totals)
    ocr[i] <- oc
    names(ocr)[i]<-as.character(i)
    
    general_clusters[[i]] <-cbind(unique(data.frame(x=solutions[[i]]$s,y=solutions[[i]]$g)),v=i,c=paleta[i])
    names(general_clusters)[i]<-as.character(i)
    
    if (oc<=bias){
      small_clusters[[si]]<- cbind(unique(data.frame(x=solutions[[i]]$s,y=solutions[[i]]$g)),v=i,c=paleta[i])
      names(small_clusters)[si]<-as.character(i)
      si<-si+1
    } else{
      valid_clusters[[vi]]<- cbind(unique(data.frame(x=solutions[[i]]$s,y=solutions[[i]]$g)),v=i,c=paleta[i])
      names(valid_clusters)[vi]<-as.character(i)
      vi<-vi+1
    }
    
  }
  
  general_frame <- data.frame(x=0,y=seq(0,Ysize-1),v=0,c=no_seleccionado)
  for (i in c(1:(Xsize-1))){
    general_frame<-rbind(general_frame,data.frame(x=i,y=seq(0,Ysize-1),v=0,c="#000000"))
  }
  
  valid_frame <- data.frame(x=0,y=seq(0,Ysize-1),v=0,c=no_seleccionado)
  for (i in c(1:(Xsize-1))){
    valid_frame<-rbind(valid_frame,data.frame(x=i,y=seq(0,Ysize-1),v=0,c="#000000"))
  }
  
  small_frame <- data.frame(x=0,y=seq(0,Ysize-1),v=0,c=no_seleccionado)
  for (i in c(1:(Xsize-1))){
    small_frame<-rbind(small_frame,data.frame(x=i,y=seq(0,Ysize-1),v=0,c="#000000"))
  }
  
  for(j in c(1:length(general_clusters))){
    
    gclus <- general_clusters[[j]]
    general_frame<-aux_insert_cluster_in_frame(gclus,general_frame)
    
    vclus <- valid_clusters[[as.character(j)]]
    if (!is.null(vclus)){
      valid_frame<-aux_insert_cluster_in_frame(vclus,valid_frame)
    }
    
    sclus <- small_clusters[[as.character(j)]]
    if (!is.null(sclus)){
      small_frame<-aux_insert_cluster_in_frame(sclus,small_frame)
    }
    
  }
  
  gr1 <- aux_plot_frame(general_frame,names(general_clusters),ocr,paleta,"General")
  gr2 <- aux_plot_frame(valid_frame,names(valid_clusters),ocr,paleta,"Valid")
  gr3 <- aux_plot_frame(small_frame,names(small_clusters),ocr,paleta,"Small")
  
  r<-list(general=gr1,valid=gr2,small=gr3)
  
  return(r)
  
}



grs <- plot_clusters_general_valid_small(solutions)
grs$general
grs$valid
grs$small







