require(lattice)
require(ggplot2)
source("utils/Configuration.R")
source("utils/SolUtilities.R")

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
      genes<-append(genes, as.character(genesL[as.numeric(gi)+1]))
    }
    conditions <- c()
    for (si in levels(fs)){
      conditions<-append(conditions, as.character(samplesL[as.numeric(si)+1]))
    }
    times <- c()
    for (ti in levels(ft)){
      times<-append(times, as.character(timesL[as.numeric(ti)+1]))
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

# ----


buildTimeSeriesPlots<-function(inputSolPath,yl=""){
  
  ##***Lectura del fichero .sol
  props <- read.properties(inputSolPath)
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









