################################################################
#########Descargar y tratar datasets de ArrayExpress############
################################################################
#PASO 0.a: Cargar librería
library("ArrayExpress")

sets = queryAE(keywords = "pneumonia", species = "homo+sapiens")

#objeto = ArrayExpress("E-GEOD-44287", path="/Users/davgutavi/Desktop/dataset/", save=TRUE)

p1 = ArrayExpress("E-UMCU-16")
p2 = ArrayExpress("E-GEOD-29375")

array = exprs(p2)



################################################################
##################Descargar y tratar datasets de GEO############
################################################################

#PASO 0: Cargar librerías
library(GEOquery)
library(limma)

#PASO 1: Con el código de acceso ("Accession") y el directorio de destino (opcional) descargar datos y crear obgeto gestor
gds <- getGEO("GDS4442", destdir="/Users/davgutavi/Desktop/dataset/")

#PASO 2: guardar los datos en un data.frame
datos = Table(gds)

#PASO 4: Obtener la lista de nombres de genes (Variable 2 del data.frame "datos") y escribir en fichero
genes = datos[2]
write.table(genes, file="/Users/davgutavi/Desktop/dataset/gds4442_genes.csv",row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)

#PASO 4: filtrar las columnas deseadas para construir el dataset

micro_t12  = subset(datos,select=c(3,4,5,12,13,14))
micro_t24  = subset(datos,select=c(6,7,8,15,16,17))
micro_t48  = subset(datos,select=c(9,10,11,18,19,20))


#PASO 5: guardar en fichero csv
#csv  => "." es punto decimal y "," separador
#csv2 => "," es punto decimal y ";" separador

write.table(micro_t12, file="/Users/davgutavi/Desktop/GDS4442/gds4442_t12.csv",row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(micro_t24, file="/Users/davgutavi/Desktop/GDS4442/gds4442_t24.csv",row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(micro_t48, file="/Users/davgutavi/Desktop/GDS4442/gds4442_t48.csv",row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(micro_t24, file="/Users/davgutavi/Desktop/GDS4262/gds4262_t24.csv",row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)

write.csv(micro_t0, file = "/Users/davgutavi/Desktop/micro_t0.csv",row.names = FALSE)















