rsList <- NULL

loadXmlResources <- function(resourcesXmlPath) {
  rsData <- xmlParse(resourcesXmlPath)
  rsList <<- xmlToList(rsData)
}

closeXmlResources <- function(){
  rsList <<- NULL
}

getGSTtags <- function(datasetList){

  rootPath <- paste0(getResourcesFolderPath(),"/",datasetList$.attr["id"])

  genesPath <- paste0(rootPath,"/",datasetList$genes)

  samplesPath <- paste0(rootPath,"/",datasetList$samples)

  timesPath <- paste0(rootPath,"/",datasetList$times)

  res <- c(genesPath,samplesPath,timesPath)

  names(res)<- c("genes","samples","times")

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

getDatasetValues <- function(datasetInfo){
  
  valuePaths <- datasetInfo$"resources"
  rootPath <- paste0(getResourcesFolderPath(),"/",datasetInfo$.attr["id"])
  sep <- valuePaths$.attrs
  
  values <- list()
  
  for (i in c(1:(length(valuePaths)-1))){
    path<-paste0(rootPath,"/",valuePaths[i])
    show(paste0("Procesing path: ",path))
    
    t <- read.csv(path,sep= sep, header = FALSE)
    values[[i]] <- t  
    
  }
  
  
  v000 <- (values[[1]])[1,1]
  v0C0 <- (values[[1]])[1,ncol(values[[1]])]
  vG00 <- (values[[1]])[nrow(values[[1]]),1]
  vGC0 <- (values[[1]])[nrow(values[[1]]),ncol(values[[1]])]
  
  v00T <- (values[[length(values)]])[1,1]
  v0CT <- (values[[length(values)]])[1,ncol(values[[1]])]
  vG0T <- (values[[length(values)]])[nrow(values[[1]]),1]
  vGCT <- (values[[length(values)]])[nrow(values[[1]]),ncol(values[[1]])]
  
  show(paste0("[",0,",",0,",",0,"] - [",1,",",1,",",1,"] = ",v000))
  show(paste0("[",0,",",ncol(values[[1]])-1,",",0,"] - [",1,",",ncol(values[[1]]),",",1,"] = ",v0C0))
  show(paste0("[",nrow(values[[1]])-1,",",0,",",0,"] - [",nrow(values[[1]]),",",1,",",1,"] = ",vG00))
  show(paste0("[",nrow(values[[1]])-1,",",ncol(values[[1]])-1,",",0,"] - [",nrow(values[[1]]),",",ncol(values[[1]]),",",1,"] = ",vGC0))
  
  show(paste0("[",0,",",0,",",length(values)-1,"] - [",1,",",1,",",length(values),"] = ",v00T))
  show(paste0("[",0,",",ncol(values[[1]])-1,",",length(values)-1,"] - [",1,",",ncol(values[[1]]),",",length(values),"] = ",v0CT))
  show(paste0("[",nrow(values[[1]])-1,",",0,",",length(values)-1,"] - [",nrow(values[[1]]),",",1,",",length(values),"] = ",vG0T))
  show(paste0("[",nrow(values[[1]])-1,",",ncol(values[[1]])-1,",",length(values)-1,"] - [",nrow(values[[1]]),",",ncol(values[[1]]),",",length(values),"] = ",vGCT))
  
  return(values)
  
}

