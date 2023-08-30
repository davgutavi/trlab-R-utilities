#library(lattice)


expGraph<-funtion(folderPath,w=8,h=5,lib=FALSE)

if (!lib){
  library(lattice)
}


paths<-list.files(folderPath,full.names = TRUE)

for (path in paths){
  
  aux<-unlist(strsplit(path, "\\."))
  aux2<-unlist(strsplit(aux[1], "\\/"))
  name<-aux2[length(aux2)]
  out1<-paste(aux[1], "_oGxCpT.eps", sep = "")
  out2<-paste(aux[1], "_oGxTpC.eps", sep = "")
  out3<-paste(aux[1], "_oTxGpC.eps", sep = "")
  out4<-paste(aux[1], "_oCxGpT.eps", sep = "")
  
  tricluster<-read.csv2(path)
  attach(tricluster)
  fs<-factor(s)
  ft<-factor(t)
  fg<-factor(g)
  
  nel<-as.numeric(levels(tricluster$el)[as.double(tricluster$el)]) 
  
  #Vista: O = Genes, X = Tiempos, P = Condiciones oGxTpC 
  postscript(file = out2,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h) 
  g2<-xyplot(nel ~ ft | fs, tricluster,
             main=paste(nombre," : Genes for each Condition",sep = ""),
             xlab="times",
             ylab="expression levels", 
             groups = g, 
             type = "a",
             layout = c(1, nlevels(fs)))
  print(g2)
  dev.off()
  
  #Vista: O = Tiempos, X = Genes, P = Condiciones oTxGpC  
  postscript(file = out3,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h)
  g3<-xyplot(nel ~ g | fs, tricluster,
             main=paste(nombre," : Times for each Condition",sep = ""),
             xlab="genes",
             ylab="expression levels", 
             groups = t,
             type = "a",
             layout = c(1, nlevels(fs))) 
  print(g3)
  dev.off()
  
  #Vista: O = Condiciones, X = Genes, P = Tiempos oCxGpT
  postscript(file = out4,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h)
  g4<-xyplot(nel ~ g | ft,
             main=paste(nombre," : Conditions for each Time",sep = ""),
             xlab="genes",
             ylab="niveles de expresión", 
             groups = s,
             type = "a",
             layout = c(1, nlevels(ft))) 
  print(g4)
  dev.off()  
}

rm(list = ls())

}

######################################################
num = 5

ws = "/Users/davgutavi/Desktop/OPTresults/analysis/r/tri-"


w = 8
h = 5

trellis.device(device="postscript", color = TRUE)
rutas <-c()
for (i in 1:num) rutas<-c(rutas,paste(ws,i,".csv",sep =""),recursive = TRUE)

#fel ~ f_ejex  | f_grafico
#groups = ...,#outline

#Vista: O = Genes, X = Condiciones, P = Tiempos oGxCpT 
#postscript(file = salida1,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h)
#g1<-xyplot(nel ~ fs | ft, tricluster,
#main=paste(nombre," : Genes por cada Tiempo",sep = ""),
#xlab="condiciones",
#ylab="niveles de expresión", 
#groups = g, 
#type = "a",
#layout = c(1, nlevels(ft)))
#print(g1)
#dev.off()
