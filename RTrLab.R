source("utils/Configuration.R")
source("utils/SolUtilities.R")
source("utils/GraphUtilities.R")

##***********************************************inputs

input <- "/home/david/TrLab/test/test.sol"
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
out2 <- paste0(aux3, "/tri_09")

##################SOLUTION COMBO#############################################################
paintSolutionsCombo(solutions,genesL,samplesL,timesL,out1)




















