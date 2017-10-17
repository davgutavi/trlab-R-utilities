source("configuration.R")
source("SolUtilities.R")
require(lattice)

##***********************************************inputs
input <- "/Users/davgutavi/Desktop/soluciones_paper/elu/elu_msl_02.sol"
##***********************************************inputs

##***Lectura del fichero .sol
props <- read.properties(input)
##***Obtener información del dataset
datasetInfo <- getDataset(props$dataset)
show(paste("selected dataset =", datasetInfo$.attrs["name"]))

#****Obtener los paths de las listas de genes, condiciones y tiempos
paths <- getGSTtags(datasetInfo)
show(paths)

#****Obtener los genes, condiciones y tiempos en forma de vector
genesL <- as.vector(read.table(paths["genes"],sep = "\n")$V1)
samplesL <- as.vector(read.table(paths["samples"],sep = "\n")$V1)
timesL <- as.vector(read.table(paths["times"],sep = "\n")$V1)

#****Obtener valores del dataset
dataset <- getDatasetValues(datasetInfo)

#****Obtener los puntos [gen,condición,tiempo,expresión génica] de cada solución
solutions <-getTriclusters(props,dataset)

aux1 <- paste0(unlist(strsplit(input, "/")))
aux2 <- aux1[-length(aux1)]
aux3 <-paste0(aux2,collapse="/")
out1 <- paste0(aux3, "/graphs/")
out2 <- paste0(aux3, "/tri_02")


solution<-solutions[[2]]
outPath<-out2

####*****************************************************************************************************************************
####*****************************************************************************************************************************
####*****************************************************************************************************************************
####*****************************************************************************************************************************

w<-8
h<-8
fsizeXaxis<-0.7
fsizeYaxis<-0.7
fsizeBoxes<-1.0
color=TRUE
  
  axis <-c(fsizeYaxis,fsizeXaxis)
  boxes<-list (cex = fsizeBoxes)
  gforctag <- "Genes for each Condition"
  tforctag <- "Times for each Condition"
  cforttag <- "Conditions for each Time"
  eltag <- "expression levels"
  timetag <- "times"
  gentag <- "genes"
  
  
  
  
  g  <- solution$g
  s  <- solution$s
  t  <- solution$t
  el <- solution$el
  
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
  
  out_gct  <-paste0(outPath, "_gct.eps")
  out_gtc  <-paste0(outPath, "_gtc.eps")
  out_tgc  <-paste0(outPath, "_tgc.eps")
  
  # x=genes, o=conditions p=times  
  xyplot(el ~ fg | ft,solution,
               main= cforttag,
               xlab= gentag,
               ylab= eltag, 
               groups = s,
               type = "a",
               font = "arial",
               scales = list(x = list(at=at,labels=genes),cex=axis),
               layout = c(1, nlevels(ft)),
               strip=strip.custom(factor.levels=times,par.strip.text = boxes))

  dev.copy2eps(file = out_gct, width=w, height=h)
  
  
  # x=genes, o=times p=conditions  
  xyplot(el ~ fg | fs, solution,
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
  
  dev.copy2eps(file = out_gtc, width=w, height=h)
  
  # x=times, o=genes p=conditions  
  xyplot(el ~ ft | fs, solution,
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
 
  dev.copy2eps(file = out_tgc, width=w, height=h)