#library(lattice)

ws = "/Users/davgutavi/Desktop/experimentosSinteticos/experimento6/exp6_10/exp6_10_informe_R"

aux2 = unlist(strsplit(ws, "\\/"))
nombre = aux2[length(aux2)]
ruta = paste(ws,".csv",sep ="")  
aux = unlist(strsplit(ruta, "\\."))
salida1 = paste(aux[1], "_tiempos.eps", sep = "")
salida2 = paste(aux[1], "_genes.eps", sep = "")
salida3 = paste(aux[1], "_condiciones.eps", sep = "")
salida4 = paste(aux[1], "_evaluacion.eps", sep = "")
informe = read.csv2(ruta)
attach(informe)
plot(s,t,
       xlab="soluciones",
       main=paste(nombre," : Número de tiempos por cada solución",sep = ""),
       ylab="número de tiempos",
       type = "o")
dev.copy2eps(file = salida1)
plot(s,g,
     main=paste(nombre," : Número de genes por cada solución",sep = ""),
     xlab="soluciones",
     ylab="número de genes",
     type = "o")
dev.copy2eps(file = salida2)
plot(s,c,
     main=paste(nombre," : Número de condiciones por cada solución",sep = ""),
     ylab="número de condiciones",
     xlab="soluciones",
     type = "o")
dev.copy2eps(file = salida3)
plot(s,as.numeric(as.vector(e)),
     main=paste(nombre," : Evaluación por cada solución",sep = ""),
     xlab="soluciones",
     ylab="función de evaluación",
     type = "o")
dev.copy2eps(file = salida4)
rm(list = ls())