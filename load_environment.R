# General variables and libraries ----
require(properties)
require(XML)
TrLabPath <- "/Users/davgutavi/TrLab"
rsFolderPath <- paste0(TrLabPath,"/resources")
rsFilePath <- paste0(TrLabPath,"/resources/resources.xml")
rsData <- xmlParse(rsFilePath)
rsList <<- xmlToList(rsData)

# Global functions ----
getResourcesFolderPath <- function(){
  return(rsFolderPath)
}

closeXmlResources <- function() {
  rsList <<- NULL
}






# Load an experiment: (solutions, dataset_info, dataset_values) from .sol file ----
loadExperiment <- function (solutionFilePath){
  
  #****Lectura del fichero .sol
  props <- read.properties(solutionFilePath)
  #****Obtener información del dataset
  datasetInfo <- getDataset(props$dataset)
  #****Obtener valores del dataset
  datasetValues <- getDatasetValues(datasetInfo)
  #****Obtener los puntos [gen,condición,tiempo,expresión génica] de cada solución
  solutions <-getTriclusters(props,datasetValues)
  
  #****Lectura del fichero de valoeres de TRIQ: experimentName_triq.csv
  return(list(solutions = solutions,dataset_info= datasetInfo,dataset_values=datasetValues))
}


load_experiment_with_loaded_dataset <- function(solution_path, loaded_dataset){
  props <- read.properties(solution_path)
  solutions <-getTriclusters(props,loaded_dataset$dataset_values)
  return(list(solutions = solutions,dataset_info= loaded_dataset$dataset_info,
              dataset_values=loaded_dataset$dataset_values))
}



load_dataset_by_name <- function(dataset_name){
  datasetInfo <- getDataset(dataset_name)
  datasetValues <- getDatasetValues(datasetInfo)
  return(list(dataset_info= datasetInfo,dataset_values=datasetValues))
}







# Triclustering parser ---- 
getTriclusters<-function(solProperties,datasetValues){
  
  aux <- unlist(strsplit(solProperties$solutions,"@"))
  
  triclusters <- list()
  
  print(paste0("Loading ",length(aux)," solutions"))
  i <- 1
  for(sol in aux){
    
    parts <- unlist(strsplit(sol,";"))
    
    gv <- as.numeric(unlist(strsplit(parts[1],",")))
    sv <- as.numeric(unlist(strsplit(parts[2],",")))
    tv <- as.numeric(unlist(strsplit(parts[3],",")))
    
    tri <- data.frame(g = numeric(0), s=numeric(0), t=numeric(0), el=numeric(0))
   
    print(paste0("Loading solution ",i," [",length(gv),",",length(sv),",",length(tv),"]"))
    
    for (g in gv){
      
      for (s in sv){
        
        for (t in tv){
          
          timeSlice = datasetValues[[t+1]]
          
          el = timeSlice[g+1,s+1]
          
          tri[nrow(tri)+1,]<-c(g,s,t,el)
          
        }
        
      }
      
    }
    
    # str(tri)
    
    triclusters[[length(triclusters)+1]]<-tri
    i<-i+1
  }
  
  return(triclusters)
  
}




getGSTtags <- function(datasetList) {
  rootPath <-
    paste0(getResourcesFolderPath(), "/", datasetList$.attr["id"])
  
  genesPath <- paste0(rootPath, "/", datasetList$genes)
  
  samplesPath <- paste0(rootPath, "/", datasetList$samples)
  
  timesPath <- paste0(rootPath, "/", datasetList$times)
  
  res <- c(genesPath, samplesPath, timesPath)
  
  names(res) <- c("genes", "samples", "times")
  
  return(res)
}

getDataset <- function(datasetname) {
  dataset <- NULL
  enc <- FALSE
  i <- 1
  
  while (i <= length(rsList) & !enc) {
    dt <- rsList[i]
    
    dtname <- dt$dataset$.attrs["name"]
    
    i <- i + 1
    
    if (dtname == datasetname) {
      dataset <- dt
      enc <- TRUE
    }
    
  }
  
  return (dataset$dataset)
  
}

getDatasetValues <- function(datasetInfo) {
  valuePaths <- datasetInfo$"resources"
  rootPath <- paste0(getResourcesFolderPath(), "/", datasetInfo$.attr["id"])
  sep <- valuePaths$.attrs
  
 
  
  values <- list()
  
  for (i in c(1:(length(valuePaths) - 1))) {
    path <- paste0(rootPath, "/", valuePaths[i])
    show(paste0("Procesing path: ", path))
    t <- read.csv(path, sep = sep, header = FALSE)
    values[[i]] <- t
    
  }
  
  
  v000 <- (values[[1]])[1, 1]
  v0C0 <- (values[[1]])[1, ncol(values[[1]])]
  vG00 <- (values[[1]])[nrow(values[[1]]), 1]
  vGC0 <- (values[[1]])[nrow(values[[1]]), ncol(values[[1]])]
  
  v00T <- (values[[length(values)]])[1, 1]
  v0CT <- (values[[length(values)]])[1, ncol(values[[1]])]
  vG0T <- (values[[length(values)]])[nrow(values[[1]]), 1]
  vGCT <- (values[[length(values)]])[nrow(values[[1]]), ncol(values[[1]])]
  
  show(paste0("[", 0, ",", 0, ",", 0, "] - [", 1, ",", 1, ",", 1, "] = ", v000))
  show(paste0("[",0,",",ncol(values[[1]]) - 1,",",0,"] - [",1,",",ncol(values[[1]]) - 1,",",1,"] = ",v0C0 ))
  show(paste0("[",nrow(values[[1]]) - 1,",",0,",",0,"] - [",nrow(values[[1]]) - 1,",",1,",",1,"] = ", vG00))
  show(paste0("[",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",0,"] - [",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",1,"] = ",vGC0))
  show(paste0("[",0,",",0,",",length(values) - 1,"] - [",1,",",1,",",length(values) - 1,"] = ",v00T))
  show(paste0("[",0,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] - [", 1,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] = ",v0CT))
  show(paste0("[",nrow(values[[1]]) - 1,",",0,",",length(values) - 1,"] - [",nrow(values[[1]]) - 1,",",1,",",length(values) - 1,"] = ",vG0T))
  show(paste0("[",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] - [",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] = ",vGCT))
  
  return(values)
  
}

