rsList <- NULL

loadXmlResources <- function(resourcesXmlPath) {
  require(XML)
  rsData <- xmlParse(resourcesXmlPath)
  rsList <<- xmlToList(rsData)
 
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
