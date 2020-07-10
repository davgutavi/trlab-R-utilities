solPath <-"/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/andorra29/t04_p02/t04_p02.sol"
outputFolder <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/mapas/coordenadas"
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
   
  


