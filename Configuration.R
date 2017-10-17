source("DatasetUtilities.R")

#***********************************************configuration
require(properties)
require(XML)
TrLabPath <- "/Users/davgutavi/Desktop/TrLab_go_actualizado"
rsFolderPath <- paste0(TrLabPath,"/resources")
rsFilePath <- paste0(TrLabPath,"/resources/resources.xml")
loadXmlResources(rsFilePath)
#***********************************************configuration

#***********************************************functions

getResourcesFolderPath <- function(){
  return(rsFolderPath)
}

#***********************************************functions