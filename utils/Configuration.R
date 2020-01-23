source("utils/DatasetUtilities.R")

#***********************************************configuration
require(properties)
require(XML)
TrLabPath <- "/home/david/TrLab"
rsFolderPath <- paste0(TrLabPath,"/resources")
rsFilePath <- paste0(TrLabPath,"/resources/resources.xml")
loadXmlResources(rsFilePath)
#***********************************************configuration

#***********************************************functions

getResourcesFolderPath <- function(){
  return(rsFolderPath)
}

#***********************************************functions