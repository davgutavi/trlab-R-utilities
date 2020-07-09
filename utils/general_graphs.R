require(lattice)
require(ggplot2)
require(gridExtra)
require(grid)
require(stringr)

# Patrones ----

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

buildCellPatternPlots <-function(inputSolPath, xl="Features", yl="Occurrences"){
  
  experiment <-loadExperiment(inputSolPath)
  solutions <- experiment$solutions
  datasetInfo <- experiment$dataset_info
  paths <- getGSTtags(datasetInfo)
  timesL <- as.vector(read.table(paths["times"],sep = "\n")$V1)
  
  
  res <- list()
  i <- 1
  
  
  for (tri in solutions){
    
    tri$p <- paste0("(",tri$s,",",tri$g,")")
    tri$fl <- timesL[tri$t+1]
    
    print(paste0("Plotting solution ",i,". Features = ",toString(levels(factor(tri$fl)))))
   
     gr<-ggplot(tri, aes(x=fl,y=el,group=p,color=p))+
      geom_line()+
      ylab(yl)+
      xlab(xl)+
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
buildMap<-function(path,Xsize,Ysize){
 
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
  
  for(j in c(1:length(clusters))){
    clus <- clusters[[j]]
    for (i in c(1:nrow(clus))){
      cv <- general[which(general$x==clus[i,]$x&general$y==clus[i,]$y),3]
      if (length(cv)>0){
        if (cv!=0){
          general[which(general$x==clus[i,]$x&general$y==clus[i,]$y),3]=-1
        }
        else{
          general[which(general$x==clus[i,]$x&general$y==clus[i,]$y),3]=clus[i,]$v
        }
      }
    }
  }
  
  general$x<- as.character(general$x)
  general$y<- as.character(general$y)
  general$v<- as.character(general$v)
  
  gr<-ggplot(general, aes(x = x, y = y, fill=v, colour=v)) + 
    geom_tile() + 
    scale_x_discrete(limits = unique(general$x))+
    scale_y_discrete(limits = unique(general$y))+
    labs(x="X", y="Y", title="Map") 
  
  return(gr)
}

cluster_maps <- function(solPath,Xsize,Ysize,title){
  
  experiment <-loadExperiment(solPath)
  solutions <- experiment$solutions
  
  explored <- data.frame(x=0,y=seq(0,Ysize-1),v=0)
  
  for (j in c(1:(Xsize-1))){
    explored<-rbind(explored,data.frame(x=j,y=seq(0,Ysize-1),v=0))
  }
  
  grs <- list()
  i <- 1
  for (solution in solutions){
    
    print(paste0("Plotting map #",i,"/",length(solutions)))
    
    gdata <- data.frame(x=0,y=seq(0,Ysize-1),v=0)
    
    for (j in c(1:(Xsize-1))){
      gdata<-rbind(gdata,data.frame(x=j,y=seq(0,Ysize-1),v=0))
    }
    
    cluster <- cbind(unique(data.frame(x=solution$s,y=solution$g)),v=i)
    
    for (j in c(1:nrow(cluster))){
      gdata[which(gdata$x==cluster[j,]$x&gdata$y==cluster[j,]$y),3]=cluster[j,]$v
      explored[which(explored$x==cluster[j,]$x&explored$y==cluster[j,]$y),3]=1
    }
    
    gdata$x<- as.character(gdata$x)
    gdata$y<- as.character(gdata$y)
    gdata$v<- as.character(gdata$v)
    
    if(length(levels(factor(gdata$v)))==2){
      clr1 <- c("white","black")
      clr2 <- c("black","white")
    }else{
      if(levels(factor(gdata$v))>1){
        clr1 <- c("black","white")
        clr2 <- c("white","black")
      }
    }
    
    gr <- ggplot(gdata, aes(x = x, y = y, fill=v, colour=v)) + 
      geom_tile() + 
      scale_fill_manual(values = clr1)+
      scale_color_manual(values = clr2)+
      scale_x_discrete(limits = unique(gdata$x))+
      scale_y_discrete(limits = unique(gdata$y))+
      labs(x="X", y="Y", title=paste0("Cluster ",i))+
      theme(legend.position = "none")
    
    grs[[i]]<-gr
    
    i <- i+1
  }
  
  if(length(levels(factor(explored$v)))==2){
    clr1 <- c("white","black")
    clr2 <- c("black","white")
  }else{
    if(levels(factor(explored$v))==1){
      clr1 <- c("black","white")
      clr2 <- c("white","black")
    }
  }
  
  explored$x<- as.character(explored$x)
  explored$y<- as.character(explored$y)
  explored$v<- as.character(explored$v)
  
  ugr <- ggplot(explored, aes(x = x, y = y, fill=v, colour=v)) + 
    geom_tile() + 
    scale_fill_manual(values = clr1)+
    scale_color_manual(values = clr2)+
    scale_x_discrete(limits = unique(explored$x))+
    scale_y_discrete(limits = unique(explored$y))+
    labs(x="X", y="Y", title=paste0("Explored"))+
    theme(legend.position = "none") 
  
  
  grs[[length(grs)+1]]=ugr
 
  clusterMaps <- do.call(arrangeGrob,c(grs,ncol=floor(sqrt(length(grs))),top=title))
  
  return(clusterMaps)
  
}


cluster_maps_total <- function(solPath,totPath,Xsize,Ysize,title){
  
  totals <- read.csv(totPath,sep=";",header=F)
  
  experiment <-loadExperiment(solPath)
  solutions <- experiment$solutions
  
  explored <- data.frame(x=0,y=seq(0,Ysize-1),level="0")
  
  for (j in c(1:(Xsize-1))){
    explored<-rbind(explored,data.frame(x=j,y=seq(0,Ysize-1),level="0"))
  }
  
  grs <- list()
  i <- 1
  for (solution in solutions){
    
    print(paste0("Plotting map #",i,"/",length(solutions)))
    
    gdata <- data.frame(x=0,y=seq(0,Ysize-1),value=-1,level =-1)
    
    for (j in c(1:(Xsize-1))){
      gdata<-rbind(gdata,data.frame(x=j,y=seq(0,Ysize-1),value=-1,level =-1))
    }
    
    cluster <- cbind(unique(data.frame(x=solution$s,y=solution$g)))
    
    for (j in c(1:nrow(cluster))){
      
      explored[which(explored$x==cluster[j,]$x&explored$y==cluster[j,]$y),3]="1"
      
      tv <- totals[cluster[j,]$y+1,cluster[j,]$x+1]
      
      if(tv==0){
        l = "0"
      }
      else if(tv>0&&tv<=25){
        l = "1"
      }
      else if(tv>25&&tv<=50){
        l = "2"
      }
      else if(tv>=50){
        l = "3"
      }
      gdata[which(gdata$x==cluster[j,]$x&gdata$y==cluster[j,]$y),3]=tv
      gdata[which(gdata$x==cluster[j,]$x&gdata$y==cluster[j,]$y),4]=l
    }
    
    gdata$x<- as.character(gdata$x)
    gdata$y<- as.character(gdata$y)
    
    gr<-ggplot(gdata, aes(x, y)) +
      geom_tile(aes(fill = level), colour = "black")+
      scale_fill_manual(values=c("-1"="white","0"="black","1"="green","2"="blue","3"="red"),
                        labels=c("-1"="No selected","0"="0","1"="(0,25]","2"="(25,50]","3"=">50"))+
      scale_x_discrete(limits = unique(gdata$x),position = "top")+
      scale_y_discrete(limits = rev(unique(gdata$y)))+
      labs(x="X", y="Y", title=paste0("Cluster ",i))+
      theme(legend.title = element_blank())
    
    
    grs[[i]]<-gr
    
    i <- i+1
  }
  
  explored$x<- as.character(explored$x)
  explored$y<- as.character(explored$y)
  
  ugr <- ggplot(explored, aes(x, y)) +
    geom_tile(aes(fill = level), colour = "black")+
    scale_fill_manual(values=c("0"="white","1"="black"),
                      labels=c("0"="No selected","1"="Selected"))+
    scale_x_discrete(limits = unique(explored$x),position = "top")+
    scale_y_discrete(limits = rev(unique(explored$y)))+
    labs(x="X", y="Y", title="Explored")+
    theme(legend.title = element_blank())
  
  grs[[length(grs)+1]]<-ugr
  
  occurrences <- cluster_occurrences_aux(solutions,totals)
  grs[[length(grs)+1]]<-occurrences
  
  clusterMaps <- do.call(arrangeGrob,c(grs,ncol=floor(sqrt(length(grs))),top=title))
  
  return(clusterMaps)
  
}

cluster_occurrences<-function(solPath,totPath){
  totals <- read.csv(totPath,sep=";",header=F)
  experiment <-loadExperiment(solPath)
  solutions <- experiment$solutions
  gr <- cluster_occurrences_aux(solutions,totals)
  return(gr)
}

cluster_occurrences_aux<-function(solutions,totals){
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






