require(lattice)
require(ggplot2)
require(gridExtra)
require(grid)
require(stringr)

# Patrones ----
paleta <- c("#7FFFD4","#FFE4C4","#8B7355","#7FFF00","#FF7F24","#00BFFF","#FFC0CB","#CD4F39","#A8A8A8","#6495ED","#FF4040","#FF1493","#0000EE","#00CD00","#8968CD","#CD00CD","#98F5FF","#006400","#FFFACD")


plotPatterns <- function(solPath){
  
  experiment <- loadExperiment(solPath)
  solutions <- experiment$solutions
  datasetInfo <- experiment$dataset_info
  
  #****Obtener los paths de las listas de genes, condiciones y tiempos
  paths <- getGSTtags(datasetInfo)
  
  #****Obtener los genes, condiciones y tiempos en forma de vector
  genesL <- as.vector(read.table(paths["genes"],sep = "\n")$V1)
  samplesL <- as.vector(read.table(paths["samples"],sep = "\n")$V1)
  timesL <- as.vector(read.table(paths["times"],sep = "\n")$V1)
  
  #****Output path
  aux1 <- paste0(unlist(strsplit(solPath, "/")))
  aux2 <- aux1[-length(aux1)]
  aux3 <-paste0(aux2,collapse="/")
  out1 <- paste0(aux3, "/graphs/")
  
  paintSolutionsCombo(solutions,genesL,samplesL,timesL,out1)
  
}


paintSolutionsCombo<- function(solutions,genesList,samplesList,timesList,outPath,
                          w=20,h=10,fsizeXaxis=0.7,fsizeYaxis=0.7,fsizeBoxes=1.0,lib=FALSE,color=TRUE){
  
  axis <-c(fsizeYaxis,fsizeXaxis)
  boxes<-list (cex = fsizeBoxes)
  gforctag <- "Genes for each Condition"
  tforctag <- "Times for each Condition"
  cforttag <- "Conditions for each Time"
  eltag <- "expression levels"
  timetag <- "times"
  gentag <- "genes"
  dir.create(outPath,showWarnings = FALSE)
  
  i <- 1
  for (sol in solutions){
  
    g  <- sol$g
    s  <- sol$s
    t  <- sol$t
    el <- sol$el
    
    fg<-factor(g)
    fs<-factor(s)
    ft<-factor(t)
    
    distance <- 2
    left <- distance
    div <- ceiling(length(levels(fg))/2)
    right<-length(levels(fg))-distance
    at<-c(left,div,right)
    labels<-c(g[left],g[div],g[right])
    
    
    genes <- c()
    for (gi in levels(fg)){
      genes<-append(genes, as.character(genesList[as.numeric(gi)+1]))
    }
    
    conditions <- c()
    for (si in levels(fs)){
      conditions<-append(conditions, as.character(samplesList[as.numeric(si)+1]))
    }

    times <- c()
    for (ti in levels(ft)){
      times<-append(times, as.character(timesList[as.numeric(ti)+1]))
    }

    out <- paste0(outPath,"graph_tri_",i,".eps")
    
    trellis.device(device="postscript", color)
    postscript(file = out,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h)
    
    # x=genes, o=conditions p=times  
    g1<-xyplot(el ~ fg | ft,sol,
               main= cforttag,
               xlab= gentag,
               ylab= eltag, 
               groups = s,
               type = "a",
               font = "arial",
               scales = list(x = list(at=at,labels=genes),cex=axis),
               layout = c(1, nlevels(ft)),
               strip=strip.custom(factor.levels=times,par.strip.text = boxes))
    
    print(g1,split=c(1,1,3,1),more = TRUE)
    
    # x=genes, o=times p=conditions  
    g2<-xyplot(el ~ fg | fs, sol,
               main=tforctag,
               xlab= gentag,
               ylab= eltag,  
               groups = t,
               type = "a",
               font = "arial",
               scales = list(x = list(at=at,labels=genes),cex=axis),
               layout = c(1, nlevels(fs)),
               strip=strip.custom(factor.levels=conditions,par.strip.text = boxes)
    ) 
    
    print(g2,split=c(2,1,3,1),more = TRUE)
    
    # x=times, o=genes p=conditions  
    g3<-xyplot(el ~ ft | fs, sol,
               main=gforctag,
               xlab= timetag,
               ylab= eltag, 
               groups = g, 
               type = "a",
               font = "arial",
               layout = c(1, nlevels(fs)),
               scales = list(x = list(labels=times),cex=axis),
               strip=strip.custom(factor.levels=conditions,par.strip.text = boxes)
    ) 
    
    print(g3,split=c(3,1,3,1),more = FALSE)
    
    dev.off()  
    
    show(paste (out," --> [",length(levels(fg)),",",length(levels(fs)),",",length(levels(ft)),"] printed"))
    
    i<-i+1
    
  }

}

cell_pattern_plot_list <-function(inputSolPath, xl="Features", yl="Occurrences"){
  experiment <-loadExperiment(inputSolPath)
  solutions <- experiment$solutions
  datasetInfo <- experiment$dataset_info
  paths <- getGSTtags(datasetInfo)
  features_tags <- as.vector(read.table(paths["times"],sep = "\n")$V1)
  res <- cell_pattern_plot_list_aux (solutions, features_tags,xl,yl)
  return (res)
} 

cell_pattern_plot_list_aux <-function(solutions, features_tags, xl="Features", yl="Occurrences"){
  res <- list()
  i <- 1
  for (tri in solutions){
    print(paste0("Plotting solution ",i,". Features = ",toString(levels(factor(tri$fl)))))
    gr<-cell_pattern_plot(tri,features_tags,xl,yl)
    res[[i]]<-gr
    i<-i+1
  }
  return (res)
}

cell_pattern_plot<-function(solution, features_tags, xl="Features", yl="Occurrences"){
  solution$p <- paste0("(",solution$s,",",solution$g,")")
  bre <-as.numeric(levels(factor(solution$t)))
  lab <- features_tags[bre+1]
  gr<-ggplot(solution, aes(x=t,y=el,color=p))+
    geom_line()+
    scale_x_continuous(breaks=bre, labels = lab)+
    ylab(yl)+
    xlab(xl)+
    theme_minimal() + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.border = element_rect(color="black", fill=NA), 
          strip.background = element_rect(fill=NA, color="black"))
  return (gr)
}

buildTimeSeriesPlots<-function(inputSolPath,yl=""){
  
  experiment <-loadExperiment(inputSolPath)
  solutions <- experiment$solutions
  datasetInfo <- experiment$dataset_info
  paths <- getGSTtags(datasetInfo)
  timesL <- as.vector(read.table(paths["times"],sep = "\n")$V1)
  
  
  res <- list()
  i <- 1
  
  
  for (tri in solutions){
    
    print(paste0("Plotting soluion ",i,". Time points = ",toString(levels(factor(tri$t)))))
    tri$p <- paste0("(",tri$s,",",tri$g,")")
    bre <-as.numeric(levels(factor(tri$t)))
    lab <- timesL[bre+1]
    gr<-ggplot(tri, aes(x=t,y=el))+
      geom_line(aes(color=p))+ 
      scale_x_continuous(breaks=bre, labels = lab)+
      ylab(yl)+
      theme_minimal() + 
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.border = element_rect(color="black", fill=NA), 
            strip.background = element_rect(fill=NA, color="black"))
    res[[i]]<-gr
    i<-i+1
  }
  
  return (res)
}


printPlotList <- function(pl){
  for (g in pl) {
    print(g)
  }
}

savePlotList <- function(pl,fileName,rootPath,d="eps",w=8,h=5,print=F){
  i<-1
  for (g in pl) {
    if (print==T){
      print(g)
    }
    ggsave(paste0(fileName,"_",i),g,path=rootPath,device=dv,width=w,height=h)
    i<-i+1
  }
}

# Análisis espeacio-temporal ----
buildTimeSeriesPlotsNormalized<-function(inputSolPath, yl="", xl=""){

  experiment <-loadExperiment(path)
  solutions <- experiment$solutions
  
  res <- list()
  i <- 1
  for (tri in solutions){
    tri$p <- paste0("(",tri$s,",",tri$g,")")
    
    mind<-as.numeric(min(tri$el))
    maxd<-as.numeric(max(tri$el))
    
    oldRange = maxd-mind
    newMin = -1 # Valor mínimo nuevo rango
    newMax = 1 # Valor máximo nuevo rango
    newRange = newMax - newMin
    
    bre <-as.numeric(levels(factor(tri$t)))
    lab <- timesL[bre+1]
    
    tri$norm <- (((tri$el-mind)*(newRange))/oldRange)+newMin # Formula cambiar rango eje y
    
    gr<-ggplot(tri, aes(x=t,y=norm,color=p))+
      geom_line()+ 
      scale_x_continuous(breaks=bre, labels = lab)+
      ylab(yl)+
      xlab(xl)+
      theme_minimal() + 
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.border = element_rect(color="black", fill=NA), 
            strip.background = element_rect(fill=NA, color="black")) +
      stat_summary(fun.y=mean, geom="line", colour="black", linetype=2, size=1.5) 
    
    
    
    res[[i]]<-gr
    i<-i+1
  }
  
  return (res)
}

# Mapas de calor ----
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

plot_clusters_general_valid_small <- function(solutions,totals,bias = 25){ 
  
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
  
  general_frame <- data.frame(x=0,y=seq(0,Ysize-1),v=0,c="#000000")
  for (i in c(1:(Xsize-1))){
    general_frame<-rbind(general_frame,data.frame(x=i,y=seq(0,Ysize-1),v=0,c="#000000"))
  }
  
  valid_frame <- data.frame(x=0,y=seq(0,Ysize-1),v=0,c="#000000")
  for (i in c(1:(Xsize-1))){
    valid_frame<-rbind(valid_frame,data.frame(x=i,y=seq(0,Ysize-1),v=0,c="#000000"))
  }
  
  small_frame <- data.frame(x=0,y=seq(0,Ysize-1),v=0,c="#000000")
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




zonification_plot_list <- function(solPath,totPath,Xsize,Ysize,title){
  
  totals <- read.csv(totPath,sep=";",header=F)
  
  experiment <-loadExperiment(solPath)
  solutions <- experiment$solutions
  datasetInfo <- experiment$dataset_info
  paths <- getGSTtags(datasetInfo)
  features_tags <- as.vector(read.table(paths["times"],sep = "\n")$V1)
  
  explored <- data.frame(x=0,y=seq(0,Ysize-1),value=0,level ="-1")
  
  for (j in c(1:(Xsize-1))){
    explored<-rbind(explored,data.frame(x=j,y=seq(0,Ysize-1),value=0,level ="-1"))
  }
  
  grs <- list()
  map_index <- 1
  i <- 1
  for (solution in solutions){
    
    gdata <- data.frame(x=0,y=seq(0,Ysize-1),value=-1,level =-1)
    
    for (j in c(1:(Xsize-1))){
      gdata<-rbind(gdata,data.frame(x=j,y=seq(0,Ysize-1),value=-1,level =-1))
    }
    
    cluster <- cbind(unique(data.frame(x=solution$s,y=solution$g)))
    
    for (j in c(1:nrow(cluster))){
      
      tv <- totals[cluster[j,]$y+1,cluster[j,]$x+1]
      
      if(tv==0){
        l = "0"
      }
      else if(tv>0&&tv<=11){
        l = "1"
      }
      else if(tv>11&&tv<=22){
        l = "2"
      }
      else if(tv>22&&tv<=33){
        l = "3"
      }
      else if(tv>33&&tv<=44){
        l = "4"
      }
      else if(tv>44&&tv<=100){
        l = "5"
      }
      else if(tv>100){
        l = "6"
      }
      gdata[which(gdata$x==cluster[j,]$x&gdata$y==cluster[j,]$y),3]=tv
      gdata[which(gdata$x==cluster[j,]$x&gdata$y==cluster[j,]$y),4]=l
      explored[which(explored$x==cluster[j,]$x&explored$y==cluster[j,]$y),3]=tv
      explored[which(explored$x==cluster[j,]$x&explored$y==cluster[j,]$y),4]=l
    }
    
    sum <- cluster_ocurrences_value(solution,totals)
    grtitle <- paste0("Cluster #",map_index," Total occurrences = ",sum)
    
    gdata$x<- as.character(gdata$x)
    gdata$y<- as.character(gdata$y)
    
    print(paste0("Plotting map #",map_index,"/",length(solutions)))
    
    gr<-ggplot(gdata, aes(x, y)) +
      geom_tile(aes(fill = level), colour = "black")+
      scale_fill_manual(values=c("-1"="white","0"="black","1"="#C1CDCD","2"="#CDB79E","3"="#76EE00",
                                 "4"="#00BFFF","5"="#FF7F50","6"="#FF3030"),
                        labels=c("-1"="No selected","0"="0","1"="(0,11]","2"="(11,22]","3"="(22,33]",
                                 "4"="(33,44]","5"="(44,100]","6"=">100"))+
      scale_x_discrete(limits = unique(gdata$x),position = "top")+
      scale_y_discrete(limits = rev(unique(gdata$y)))+
      labs(x="X", y="Y", title=grtitle) +
      theme(legend.title = element_blank())
    
    patt <- cell_pattern_plot(solution,features_tags)
    
    grs[[i]]<-gr
    i <- i+1
    grs[[i]]<-patt
    i <- i+1
    map_index<-map_index+1
  }
  
  explored_title <- paste0("Total explored zone, total occurrences = ",sum(explored$value))
  explored$x<- as.character(explored$x)
  explored$y<- as.character(explored$y)
  
  ugr <- ggplot(explored, aes(x, y)) +
    geom_tile(aes(fill = level), colour = "black")+
    scale_fill_manual(values=c("-1"="white","0"="black","1"="#C1CDCD","2"="#CDB79E","3"="#76EE00",
                               "4"="#00BFFF","5"="#FF7F50","6"="#FF3030"),
                      labels=c("-1"="No selected","0"="0","1"="(0,11]","2"="(11,22]","3"="(22,33]",
                               "4"="(33,44]","5"="(44,100]","6"=">100"))+
    scale_x_discrete(limits = unique(explored$x),position = "top")+
    scale_y_discrete(limits = rev(unique(explored$y)))+
    labs(x="X", y="Y", title=explored_title)+
    theme(legend.title = element_blank())
  
  grs[[length(grs)+1]]<-ugr
  
  occurrences <- cluster_occurrences_plot_aux(solutions,totals)
  grs[[length(grs)+1]]<-occurrences
  
  grframes <- plot_clusters_general_valid_small(solutions,totals)
  
  grs[[length(grs)+1]]<-grframes[[1]]
  grs[[length(grs)+1]]<-grframes[[2]]
  grs[[length(grs)+1]]<-grframes[[3]]
  
  return(grs)
  
}

cluster_occurrences_plot<-function(solPath,totPath){
  totals <- read.csv(totPath,sep=";",header=F)
  experiment <-loadExperiment(solPath)
  solutions <- experiment$solutions
  gr <- cluster_occurrences_plot_aux(solutions,totals)
  return(gr)
}

cluster_ocurrences_value<-function(solution,totals){
  coordinates <- unique(data.frame(x=solution$s,y=solution$g))
  sum <- 0
  for (i in c(1:nrow(coordinates))){
    sum <- sum + totals[coordinates[i,2]+1,coordinates[i,1]+1]
  }
  return(sum)
}

cluster_occurrences_plot_aux<-function(solutions,totals){
  id <- 1
  summation <- data.frame(id=character(), sum=numeric())
  padn <- 2
  if (length(solutions)>=100){
    padn <- 3
  }
  for (solution in solutions){
    coordinates <- unique(data.frame(x=solution$s,y=solution$g))
    sum <- 0
    for (i in c(1:nrow(coordinates))){
      sum <- sum + totals[coordinates[i,2]+1,coordinates[i,1]+1]
    }
    
    summation <- rbind(summation,data.frame(id=paste0("#",str_pad(id,padn,"left",pad="0")), sum=sum))
    id <- id +1
  }
  
  gr <- ggplot(summation,aes(x=id, y=sum))+
    geom_col()+
    xlab("Cluster")+
    ylab("Total occurrences")+
    geom_text(aes(label=sum), vjust=-0.3, size=3.5)
  
  return(gr)
}

get_coordinate_files<-function(solPath,outputFolder){
  experiment <-loadExperiment(solPath)
  solutions <- experiment$solutions
  dir.create(outputFolder,showWarnings = FALSE)
  rootFileName<-paste0(outputFolder,"/",strsplit(tail(strsplit(solPath,"/")[[1]],1),".sol")[[1]][1])
  save_coordinate_dfs(solutions,rootFileName)
}

save_coordinate_dfs<-function(solutions,rootFileName){
  padn <- 2
  if (length(solutions)>=100){
    padn <- 3
  }
  id <- 1
  for (solution in solutions){
    write.table(unique(data.frame(x=solution$s,y=solution$g)),
                sep=";",
                file = paste0(rootFileName,"_",str_pad(id,padn,"left",pad="0"),".csv"),
                row.names = F,
                quote = F
    )
    id <- id +1
  }
}

get_coordinate_dfs<-function(solutions){
  id <- 1
  dfl <- list()
  for (solution in solutions){
    dfl[[id]]<-unique(data.frame(x=solution$s,y=solution$g))
    id <- id +1
  }
  return(dfl)
}
