#library(lattice)

#OPCIONES GENERALES

#Tam letra Ejes derecha y arriba
tamEjeX = 0.7

#Tam letra Ejes izquierda y abajo
tamEjeY = 0.7

tamEjes = c(tamEjeY,tamEjeX)

#Tam letra cajas

tamCajas = list (cex = 1.0)

#ELUTRIATION
num = "11"
ws = "/Users/davgutavi/Desktop/evobio/elutriation/resources/multi-elu-14/erre/multi-elu-14_R_"

condiciones = c("CH1I","CH1B")
tiempos = c("0 min","330 min","390 min")
salida1 = "/Users/davgutavi/Desktop/elu1.eps"
salida2 = "/Users/davgutavi/Desktop/elu2.eps"
salida3 = "/Users/davgutavi/Desktop/elu3.eps"

# #GDS4510-1
# num = "16"
# ws = "/Users/davgutavi/Desktop/experimentos/ida14/gds4510/lsl_gds4510_03/erre/lsl_gds4510_03_R_"
# condiciones = c("WTREP1", "RD1REP1", "RD1REP3")
# tiempos = c("day 2", "day 4")
# salida1 = "/Users/davgutavi/Desktop/gds4510-1_1.eps"
# salida2 = "/Users/davgutavi/Desktop/gds4510-1_2.eps"
# salida3 = "/Users/davgutavi/Desktop/gds4510-1_3.eps"

# #GDS4510-2
# num = "4"
# ws = "/Users/davgutavi/Desktop/experimentos/ida14/gds4510/lsl_gds4510_08/erre/lsl_gds4510_08_R_"
# condiciones = c("WTREP1", "WTREP3", "RD1REP3")
# tiempos = c("day 2", "day 4")
# salida1 = "/Users/davgutavi/Desktop/gds4510-2_1.eps"
# salida2 = "/Users/davgutavi/Desktop/gds4510-2_2.eps"
# salida3 = "/Users/davgutavi/Desktop/gds4510-2_3.eps"






ruta = paste(ws,num,".csv",sep ="")  

tricluster = read.csv2(ruta)
attach(tricluster)
fs = factor(s)
ft = factor(t)
fg = factor(g)
nel = as.numeric(levels(tricluster$el)[as.double(tricluster$el)]) 


left<-5
div <- ceiling(length(levels(fg))/2)
right<-length(levels(fg))-5
at<-c(left,div,right)
labels<-c(g[left],g[div],g[right]) 


#Vista: O = Condiciones, X = Genes, P = Tiempos oCxGpT
xyplot(nel ~ fg | ft,
       xlab="genes",
       ylab="", 
       #scales = list(cex=tamEjes),
       groups = s,
       type = "a",
       font = "arial",
       scales = list(x = list(at=at,labels=labels),cex=tamEjes),
       layout = c(1, nlevels(ft)),
       strip=strip.custom(factor.levels=tiempos,par.strip.text = tamCajas)
       )

dev.copy2eps(file = salida1)

#Vista: O = Tiempos, X = Genes, P = Condiciones oTxGpC  

xyplot(nel ~ fg | fs, tricluster,
       xlab="genes",
       ylab="", 
       #scales = list(cex=tamEjes),
       groups = t,
       type = "a",
       font = "arial",
       scales = list(x = list(at=at,labels=labels),cex=tamEjes),
       layout = c(1, nlevels(fs)),
       strip=strip.custom(factor.levels=condiciones,par.strip.text = tamCajas)
       ) 

dev.copy2eps(file = salida2)


#Vista: O = Genes, X = Tiempos, P = Condiciones oGxTpC 
xyplot(nel ~ ft | fs, tricluster,
       xlab="time",
       ylab="", 
       groups = g, 
       type = "a",
       font = "arial",
       layout = c(1, nlevels(fs)),
       scales = list(x = list(labels=tiempos),cex=tamEjes),
       strip=strip.custom(factor.levels=condiciones,par.strip.text = tamCajas)
       ) 

dev.copy2eps(file = salida3)



