require(dplyr)
require(ggplot2)

loadDfsFromPath <- function(vectorOfFilePaths,type="grq") {

  res<-list()
  res<-list(read.csv(vectorOfFilePaths[1], sep = ";"))
  print(paste0("Df loaded: ",vectorOfFilePaths[1]))

  for(i in c(2:length(vectorOfFilePaths))){

    aux <-  read.csv(vectorOfFilePaths[i], sep = ";")
    res<- bind_rows(res,aux)
    print(paste0("Df loaded: ",vectorOfFilePaths[i]))

  }
  
  res <- transformLoadedDf(res,type)
  return (res)
}

loadDfsFromVectorOfFilePaths <- function(vectorOfFilePaths) {

res<-list()
runCounter     <- 1

runFolderPath <- vectorOfFilePaths[runCounter]
res <- list(buildRunDf(runFolderPath,runCounter))

for(runCounter in c(2:length(vectorOfFilePaths))){
  
  res[[runCounter]]<-buildRunDf(vectorOfFilePaths[runCounter],runCounter)
  
}
return(res)
}

buildRunDf <- function(runFolderPath,runCounter){
  
  csvPathList <- dir(runFolderPath, full.names = T)
  res <- cbind(read.csv(csvPathList[1], sep = ";"), run = paste0("Run#",runCounter))
  
  for (i in c(2:length(csvPathList))){
    res <-  bind_rows(res,cbind(read.csv(csvPathList[i], sep = ";"), run = paste0("Run#",runCounter)))
  }
  
  return(res)
  
}

buidGlobalSyntheticDf <- function(sintheticDfList) {
  
  mi <- 1
  res<-sintheticDfList[[mi]]
  
  for (mi in (2:length(sintheticDfList))){
    res<-bind_rows(res,sintheticDfList[[mi]])
  }
  
  res <- transformLoadedSyntheticDf(res)
  
  return(res)
  
}


transformLoadedDf<- function(loadedDf, type="grq"){
  
  tric.column <- loadedDf$tric
  new.tric.column <- c()
  for (e in tric.column){
    new.tric.column <- c(new.tric.column, paste0("Tricluster#",e))
  }
  
  ndf<-loadedDf[,-ncol(loadedDf)]
  ndf<-cbind(ndf,tri = new.tric.column)
  
  if (type=="grq"){
    names(ndf)<-c("GRQ","t","tri") 
  }
  else if (type=="comp"){
    names(ndf)<-c("f1","acc","t","tri") 
  }
  
  return(ndf)
  
}

transformLoadedSyntheticDf<- function(loadedSyntheticDf){
  
  tric.column <- loadedSyntheticDf$tric
  new.tric.column <- c()
  for (e in tric.column){
    new.tric.column <- c(new.tric.column, paste0("Tricluster#",e))
  }
  
  ndf<-loadedSyntheticDf[,-4]
  ndf<-cbind(ndf,tri = new.tric.column)
  ndf<-cbind(ndf,series = paste0(ndf$run,"-",ndf$tri))
  names(ndf)<-c("t","f1", "acc","run","tri","series") 
  
  return(ndf)
  
}

accPlot <- function(sdf,ylimits=NULL){
  
  gr <- ggplot(sdf, aes(x = t, y = acc, group = series)) +
    geom_line(aes(color =series)) +
    scale_y_continuous(limits=ylimits)+
    labs(y = "Accuracy", x="Time") +
    theme(legend.title = element_blank(),
          legend.position="top",
          ,legend.text = element_text(size = 5, face="bold",family = "mono")
          ,panel.border = element_rect(color="black", fill=NA)
          ,strip.background = element_rect(fill=NA, color="black")
          ,axis.title = element_text(size = 8, family = "mono")
          ,axis.text = element_text(size = 8, family = "mono")
          ,strip.text = element_text(size = 8, family = "mono"))+ 
    guides(col = guide_legend(ncol = 3))
  
  return(gr)
}

f1Plot <- function(sdf,ylimits=NULL){
  
  gr <- ggplot(sdf, aes(x = t, y = f1, group = series)) +
    geom_line(aes(color =series)) +
    labs(y = "F1 score", x="Time") +
    scale_y_continuous(limits=ylimits)+
    theme(legend.title = element_blank(),
          legend.position="top",
          ,legend.text = element_text(size = 5, face="bold",family = "mono")
          ,panel.border = element_rect(color="black", fill=NA)
          ,strip.background = element_rect(fill=NA, color="black")
          ,axis.title = element_text(size = 8, family = "mono")
          ,axis.text = element_text(size = 8, family = "mono")
          ,strip.text = element_text(size = 8, family = "mono"))+ 
    guides(col = guide_legend(ncol = 3))
  
  return(gr)
}

f1Box <- function (sdf,ylimits=NULL){
  
  gr <- ggplot(sdf, aes(x = tri, y = f1, group=tri))+
    stat_boxplot(geom ="errorbar")+
    scale_y_continuous(limits=ylimits)+
    geom_boxplot(outlier.colour = "blue", outlier.shape = 1) + 
    stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
    labs(x="",y="F1 score") +
    theme(legend.title = element_blank(),
          legend.position="top",
          ,legend.text = element_text(size = 5, face="bold",family = "mono")
          ,panel.border = element_rect(color="black", fill=NA)
          ,strip.background = element_rect(fill=NA, color="black")
          ,axis.title = element_text(size = 8, family = "mono")
          ,axis.text = element_text(size = 8, family = "mono")
          ,strip.text = element_text(size = 8, family = "mono"))+ 
    guides(col = guide_legend(ncol = 3))
  
  return(gr)
  
}

grqPlot <- function (grqDf,ylimits=NULL){
  
  gr <- ggplot(grqDf, aes(x = t, y = GRQ, group = tri)) +
    geom_line(aes(color =tri)) +
    scale_y_continuous(limits=ylimits)+
    labs(y = "GRQ", x="Time") +
    theme(legend.title = element_blank(),
          legend.position="top",
          ,legend.text = element_text(size = 5, face="bold",family = "mono")
          ,panel.border = element_rect(color="black", fill=NA)
          ,strip.background = element_rect(fill=NA, color="black")
          ,axis.title = element_text(size = 8, family = "mono")
          ,axis.text = element_text(size = 8, family = "mono")
          ,strip.text = element_text(size = 8, family = "mono"))+ 
    guides(col = guide_legend(ncol = 3))
  
  return(gr)
}

accF1Plot <- function (gcompDf,ylimits=NULL){
  
  gr <- ggplot(gcompDf, aes(x = t, y = v, group = serial)) +
    geom_line(aes(color =serial)) +
    scale_y_continuous(limits=ylimits)+
    geom_point(aes(color =serial)) +
    labs(y = "", x="Time") +
    theme(legend.title = element_blank(),
          legend.position="top",
          ,legend.text = element_text(size = 5, face="bold",family = "mono")
          ,panel.border = element_rect(color="black", fill=NA)
          ,strip.background = element_rect(fill=NA, color="black")
          ,axis.title = element_text(size = 8, family = "mono")
          ,axis.text = element_text(size = 8, family = "mono")
          ,strip.text = element_text(size = 8, family = "mono"))+ 
    guides(col = guide_legend(ncol = 2))
  
  return(gr)
  
}


accBox <- function (sdf,ylimits=NULL){
  
  gr <- ggplot(sdf, aes(x = tri, y = acc, group=tri))+
    stat_boxplot(geom ="errorbar")+
    scale_y_continuous(limits=ylimits)+
    geom_boxplot(outlier.colour = "blue", outlier.shape = 1) + 
    stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
    labs(x="",y="Accuracy") +
    theme(legend.title = element_blank(),
          legend.position="top",
          ,legend.text = element_text(size = 5, face="bold",family = "mono")
          ,panel.border = element_rect(color="black", fill=NA)
          ,strip.background = element_rect(fill=NA, color="black")
          ,axis.title = element_text(size = 8, family = "mono")
          ,axis.text = element_text(size = 8, family = "mono")
          ,strip.text = element_text(size = 8, family = "mono"))+ 
    guides(col = guide_legend(ncol = 3))
  
  return(gr)
  
}



