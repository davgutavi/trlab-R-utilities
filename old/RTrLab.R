library(properties)
source("/Users/davgutavi/Documents/r_workspaces/research/RTrLabUtilities.R")

#inputs#######################################################################
rsPath <- "/Users/davgutavi/Desktop/TrLab/resources/resources.xml"
input <- "/Users/davgutavi/ant_experiments/log10/lsl_01/lsl_01.sol"
#inputs#######################################################################

props <- read.properties(input)

#rsList <- loadXmlResources(rsPath)
loadXmlResources(rsPath)

#datasetInfo <- getDataset(props$dataset, rsList)
datasetInfo <- getDataset(props$dataset)

show(paste("selected dataset = ", datasetInfo$.attrs["name"]))

