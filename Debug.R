solPath <-"/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra29/test_01/test_01.sol"
totPath <-"/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/dataset/totales_29.csv"
Xsize <- 27
Ysize <- 17
title <- "Test 01"
  
totals <- read.csv(totPath,sep=";",header=F)
experiment <-loadExperiment(solPath)
solutions <- experiment$solutions

explored <- data.frame(x=0,y=seq(0,Ysize-1),value=0,level ="-1")
 
for (j in c(1:(Xsize-1))){
    explored<-rbind(explored,data.frame(x=j,y=seq(0,Ysize-1),value=0,level ="-1"))
}
  
grs <- list()
i <- 1
for (solution in solutions){
    
  print(paste0("Plotting map #",i,"/",length(solutions)))
    
  gdata <- data.frame(x=0,y=seq(0,Ysize-1),value=-1,level =-1)
    
  for (j in c(1:(Xsize-1))){
      gdata<-rbind(gdata,data.frame(x=j,y=seq(0,Ysize-1),value=-1,level ="-1"))
  }
    
    cluster <- cbind(unique(data.frame(x=solution$s,y=solution$g)))
    
    for (j in c(1:nrow(cluster))){
      
      
      
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
      explored[which(explored$x==cluster[j,]$x&explored$y==cluster[j,]$y),3]=tv
      explored[which(explored$x==cluster[j,]$x&explored$y==cluster[j,]$y),4]=l
    }
    
    sum <- cluster_ocurrences_value(solution,totals)
    grtitle <- paste0("Cluster #",i," Total occurrences = ",sum)
    
    gdata$x<- as.character(gdata$x)
    gdata$y<- as.character(gdata$y)
    
    gr<-ggplot(gdata, aes(x, y)) +
      geom_tile(aes(fill = level), colour = "black")+
      scale_fill_manual(values=c("-1"="white","0"="black","1"="green","2"="blue","3"="red"),
                        labels=c("-1"="No selected","0"="0","1"="(0,25]","2"="(25,50]","3"=">50"))+
      scale_x_discrete(limits = unique(gdata$x),position = "top")+
      scale_y_discrete(limits = rev(unique(gdata$y)))+
      labs(x="X", y="Y", title=grtitle) +
      theme(legend.title = element_blank())
    
    
    grs[[i]]<-gr
    
    i <- i+1
  }
  
  #sum <- cluster_ocurrences_value(solution,totals)
  explored_title <- paste0("Total explored zone, total occurrences = ",sum(explored$value))
  explored$x<- as.character(explored$x)
  explored$y<- as.character(explored$y)
  
  ugr <- ggplot(explored, aes(x, y)) +
    geom_tile(aes(fill = level), colour = "black")+
    scale_fill_manual(values=c("-1"="white","0"="black","1"="green","2"="blue","3"="red"),
                      labels=c("-1"="No selected","0"="0","1"="(0,25]","2"="(25,50]","3"=">50"))+
    scale_x_discrete(limits = unique(explored$x),position = "top")+
    scale_y_discrete(limits = rev(unique(explored$y)))+
    labs(x="X", y="Y", title=explored_title)+
    theme(legend.title = element_blank())
  
  grs[[length(grs)+1]]<-ugr
  
  occurrences <- cluster_occurrences_plot_aux(solutions,totals)
  grs[[length(grs)+1]]<-occurrences
  
  tail(grs,2)[[1]]










































# ----
experiment <-loadExperiment(solPath)
solutions <- experiment$solutions
dir.create(outputFolder,showWarnings = FALSE)
rootFileName<-paste0(outputFolder,"/",strsplit(tail(strsplit(solPath,"/")[[1]],1),".sol")[[1]][1])


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



# Cluster totals ----
require(stringr)
solPath <-"/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra29/t04_p02/t04_p02.sol"
totPath <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/dataset/totales_29.csv"
outputFolder <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/mapas"
totals <- read.csv(totPath,sep=";",header=F)
experiment <-loadExperiment(solPath)
solutions <- experiment$solutions

id <- 1
dfl <- list()
for (solution in solutions){
coordinates <- unique(data.frame(x=solution$s,y=solution$g))
sum <- 0
for (i in c(1:nrow(coordinates))){
  sum <- sum + totals[coordinates[i,2]+1,coordinates[i,1]+1]
}
df <- data.frame(x=coordinates$x,y=coordinates$y,o=sum)
dfl[[id]]<-df
id <- id +1
}






id <- 1
summation <- data.frame(id=character(), sum=numeric())
padn <- 2
if (length(solutions)>=100){
  padn <- 3
}

tflist <- list()

for (solution in solutions){
  coordinates <- unique(data.frame(x=solution$s,y=solution$g))
  sum <- 0
  for (i in c(1:nrow(coordinates))){
    sum <- sum + totals[coordinates[i,2]+1,coordinates[i,1]+1]
  }
  

    
  summation <- rbind(summation,data.frame(id=paste0("#",str_pad(id,padn,"left",pad="0")), sum=sum))
  id <- id +1
}

ggplot(summation,aes(x=id, y=sum))+
  geom_col()+
  xlab("Cluster")+
  ylab("Total occurrences")+
  geom_text(aes(label=sum), vjust=-0.3, size=3.5)









# Cell patterns ----
inputSolPath<-"/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra29/t04_p02/t04_p02.sol"
yl="Occurrences"
  
experiment <-loadExperiment(inputSolPath)
solutions <- experiment$solutions
datasetInfo <- experiment$dataset_info
paths <- getGSTtags(datasetInfo)
timesL <- as.vector(read.table(paths["times"],sep = "\n")$V1)
tri<-solutions[[1]]
tri$p <- paste0("(",tri$s,",",tri$g,")")
tri$fl <- timesL[tri$t+1]


    
ggplot(tri, aes(x=fl,y=el,group=p,color=p))+
  geom_line()+
  ylab(yl)+
  xlab("Features")+
  theme_minimal() + 
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.border = element_rect(color="black", fill=NA), 
            strip.background = element_rect(fill=NA, color="black"))
   
  


