#library(lattice)
num = 50

#ELUTRIATRION




#Ejecutandose:

#ws = "/Users/davgutavi/Desktop/resultados/elutriation/tanda7/elu4/erre/elu4_R_"

#ws = "/Users/davgutavi/Desktop/resultados/elutriation/sueltas/elu02/erre/elu02_R_"

#GDS4510

#ws = "/Users/davgutavi/Desktop/resultados/gds4510/tanda4/gds4/erre/gds4_R_"
#ws = "/Users/davgutavi/Desktop/resultados/gds4510/sueltas/gds15/erre/gds15_R_"

#CONTROL

#ws = "/Users/davgutavi/Desktop/resultados/contol/gds4510/controlGDS04/erre/controlGDS04_R_"
#ws ="/Users/davgutavi/Desktop/resultados/contol/sumadores2/controlsumadores201/erre/controlsumadores201_R_"
#ws = "/Users/davgutavi/Desktop/resultados/contol/levadura/controlLevadura01/erre/controlLevadura01_R_"
#ws = "/Users/davgutavi/Desktop/resultados/contol/humano/controlhumano01/erre/controlhumano01_R_"

#HUMANO

#ws = "/Users/davgutavi/Desktop/resultados/humano/tanda6/hum2/erre/hum2_R_"
#ws = "/Users/davgutavi/Desktop/resultados/levadura/tanda3/lev4/erre/lev4_R_"

#LEVADURA

#ws = "/Users/davgutavi/Desktop/resultados/humano/sueltas/hum01/erre/hum01_R_"
#ws = "/Users/davgutavi/Desktop/resultados/levadura/sueltas/lev03/erre/lev03_R_"




w = 8
h = 5

trellis.device(device="postscript", color = TRUE)
rutas <-c()
for (i in 1:num) rutas<-c(rutas,paste(ws,i,".csv",sep =""),recursive = TRUE)


for (ruta in rutas){
  
  aux = unlist(strsplit(ruta, "\\."))
  aux2 = unlist(strsplit(aux[1], "\\/"))
  nombre = aux2[length(aux2)]
  salida1 = paste(aux[1], "_oGxCpT.eps", sep = "")
  salida2 = paste(aux[1], "_oGxTpC.eps", sep = "")
  salida3 = paste(aux[1], "_oTxGpC.eps", sep = "")
  salida4 = paste(aux[1], "_oCxGpT.eps", sep = "")
  
  tricluster = read.csv2(ruta)
  attach(tricluster)
  fg = factor(g)
  fs = factor(s)
  ft = factor(t)
  fel = factor(el)
  
  #fel ~ f_ejex  | f_grafico
  #groups = ...,#outline
  
  #Vista: O = Genes, X = Condiciones, P = Tiempos oGxCpT 
  #postscript(file = salida1,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h)
  #g1<-xyplot(fel ~ fs | ft, tricluster,
  #           main=paste(nombre," : Genes por cada Tiempo",sep = ""),
  #           xlab="condiciones",
  #           ylab="niveles de expresión", 
  #           groups = g, 
  #           type = "a") 
            # scales = list(y = list(relation = "sliced", rot = 0, cex = 0.5)))
  #print(g1)
  #dev.off()
  
  #Vista: O = Genes, X = Tiempos, P = Condiciones oGxTpC 
  postscript(file = salida2,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h) 
  g2<-xyplot(fel ~ ft | fs, tricluster,
             main=paste(nombre," : Genes por cada Condición",sep = ""),
             xlab="tiempos",
             ylab="niveles de expresión", 
             groups = g, 
             type = "a") 
              #scales = list(y = list(relation = "sliced", rot = 0, cex = 0.5)))
  print(g2)
  dev.off()
  
  #Vista: O = Tiempos, X = Genes, P = Condiciones oTxGpC  
  postscript(file = salida3,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h)
  g3<-xyplot(fel ~ fg | fs, tricluster,
   main=paste(nombre," : Tiempo por cada Condición",sep = ""),
   xlab="genes",
   ylab="niveles de expresión", 
   groups = t,
   type = "a") 
   #scales = list(y = list(relation = "sliced", rot = 0, cex = 0.5)))
   print(g3)
   dev.off()
  
  #Vista: O = Condiciones, X = Genes, P = Tiempos oCxGpT
  postscript(file = salida4,colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=w, height=h)
  g4<-xyplot(fel ~ fg | ft, tricluster,
            main=paste(nombre," : Condiciones por cada Tiempo",sep = ""),
            xlab="genes",
            ylab="niveles de expresión", 
            groups = s,
            type = "a") 
            #scales = list(y = list(relation = "sliced", rot = 0, cex = 0.5)))
   print(g4)
   dev.off()  
}

rm(list = ls())




#ws = "/Users/davgutavi/Desktop/experimentos/lsl2/erre/lsl2_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/lslmaxmin2/erre/lslmaxmin2_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/lslmaxmin2/erre_transformado/lslmaxmin2_RT_"
#ws = "/Users/davgutavi/Desktop/experimentos/lsldesv2/erre/lsldesv2_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/lsldesv2/erre_transformado/lsldesv2_RT_"
#ws = "/Users/davgutavi/Desktop/experimentos/lslnorm2/erre/lslnorm2_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/lslnorm2/erre_transformado/lslnorm2_RT_"
#ws = "/Users/davgutavi/Desktop/experimentos/multi2/erre/multi2_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/multimaxmin2/erre/multimaxmin2_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/multimaxmin2/erre_transformado/multimaxmin2_RT_"
#ws = "/Users/davgutavi/Desktop/experimentos/multidesv2/erre/multidesv2_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/multidesv2/erre_transformado/multidesv2_RT_"
#ws = "/Users/davgutavi/Desktop/experimentos/multinorm1/erre/multinorm1_R_"
#ws = "/Users/davgutavi/Desktop/experimentos/multinorm1/erre_transformado/multinorm1_RT_"
#ws = "/Users/davgutavi/Documents/TRICLUSTERING/datos/sintetico2/objetivo"