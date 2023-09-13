source("system/load_environment.R")
source("utils/experiment_analysis.R")
require(modeest)
require(ggplot2)

dataset <- loadDatasetByTrLabName("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration_2/it2_005/it2_005.sol"
experiment <- loadExperimentFromLoadedDataset(solPath,dataset)

instanceExperimentCoordinates <- getExperimentDymensionCoordinates("i",experiment)
instanceOverlappingData <- getOverlappingGraphData(instanceExperimentCoordinates,experiment$dataset_tags$instance_tags)
reducedInstanceTicks <- getReducedAxisTicks(instanceExperimentCoordinates,experiment$dataset_tags$instance_tags)
buildOverlappingGraph(instanceOverlappingData,"Instances",reduced_instance_ticks=reducedInstanceTicks)


experiment_dymension_coordinates <- instanceExperimentCoordinates
dymension_tag_list <- experiment$dataset_tags$instance_tags
reduced_axis_labels <- c(dymension_tag_list[[1]])
solution_index <- 1
while (solution_index <= length(experiment_dymension_coordinates)) {
  
  dymension_index_list <-
        experiment_dymension_coordinates[[solution_index]]
  
  reduced_axis_labels <-
        c(reduced_axis_labels, dymension_tag_list[[dymension_index_list[[1]]+1]])
  
  reduced_axis_labels <-
        c(reduced_axis_labels, dymension_tag_list[[dymension_index_list[[length(dymension_index_list)]]]])
      
  solution_index <- solution_index + 1
}

reduced_axis_labels <-
      c(reduced_axis_labels, dymension_tag_list[[length(dymension_tag_list)]])

  







attributeExperimentCoordinates <- getExperimentDymensionCoordinates("a",experiment)
attributeOverlappingData <- getOverlappingGraphData(attributeExperimentCoordinates,experiment$dataset_tags$attribute_tags)
attributeGraph <- buildOverlappingGraph(attributeOverlappingData,"Attributes","Months")
attributeGraph

layerExperimentCoordinates <- getExperimentDymensionCoordinates("l",experiment)
layerOverlappingData <- getOverlappingGraphData(layerExperimentCoordinates,experiment$dataset_tags$slide_tags)
layerGraph <- buildOverlappingGraph(layerOverlappingData,"Layers","Years")
layerGraph


source("utils/experiment_analysis.R")
dataset <- loadDatasetByTrLabName("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration_2/it2_004/it2_004.sol"
experiment <- loadExperimentFromLoadedDataset(solPath,dataset)
instanceExperimentCoordinates <- getExperimentDymensionCoordinates("i",experiment)
instanceOverlappingData <- getOverlappingGraphData(instanceExperimentCoordinates,experiment$dataset_tags$instance_tags)
reducedInstanceTicks <- getReducedAxisTicks(instanceExperimentCoordinates,experiment$dataset_tags$instance_tags)
instanceGraph <- buildOverlappingGraph(instanceOverlappingData,reduced_instance_ticks=reducedInstanceTicks,
                                       dymension_font_size = 20,
                                       tricluster_font_size = 14,
                                       yes_color="red",
                                       cell_line_size=0.01)



instanceGraph











