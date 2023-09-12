source("system/load_environment.R")
require(modeest)
require(ggplot2)

dataset <- loadDatasetByTrLabName("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration_2/it2_004/it2_004.sol"
experiment <- loadExperimentFromLoadedDataset(solPath,dataset)

getOverlappingGraphData <- function(experiment_dymension_coordinates, dymension_tag_list){
  heatmap_coordinates <- data.frame()
  solution_index <- 1
  while(solution_index <= length(experiment_dymension_coordinates)){
    dymension_index_list <-  experiment_dymension_coordinates[[solution_index]]
    tricluster <- paste0("Tricluster #",solution_index)
    dymension <- dymension_tag_list
    partial_heatmap_coordinates <- expand.grid(Tricluster=tricluster, Dymension=dymension)
    values <- c()
    dymension_tag_index <- 0
    while (dymension_tag_index < length(dymension_tag_list)){
      if (dymension_tag_index %in% dymension_index_list){
        values <- c(values, "1")
      }
      else{
        values <- c(values, "0")
      }
      dymension_tag_index <- dymension_tag_index + 1
    }
    partial_heatmap_coordinates$Values <- values
    heatmap_coordinates <- rbind(heatmap_coordinates,partial_heatmap_coordinates)
    solution_index <- solution_index + 1
  }
  return(heatmap_coordinates)
}

getReducedAxisTicks <- function(experiment_dymension_coordinates, dymension_tag_list){
  reduced_axis_labels <- c(dymension_tag_list[[1]])
  solution_index <- 1
  while(solution_index <= length(experiment_dymension_coordinates)){
    dymension_index_list <- experiment_dymension_coordinates[[solution_index]]
    reduced_axis_labels <- c(reduced_axis_labels,dymension_tag_list[[dymension_index_list[[1]]]])
    reduced_axis_labels <- c(reduced_axis_labels,dymension_tag_list[[dymension_index_list[[length(dymension_index_list)]]]])
    solution_index <- solution_index + 1
  }
  reduced_axis_labels <- c(reduced_axis_labels,dymension_tag_list[[length(dymension_tag_list)]])
  return(reduced_axis_labels)
}




buildOverlappingGraph <- function(dymension_overlapping_data, title="", dymension_label="", reduced_instance_ticks=NULL){
  gr <- ggplot(dymension_overlapping_data, aes(factor(Dymension), Tricluster)) +
    geom_tile(aes(fill = Values), colour = "black") +
    scale_fill_manual(values = c("0" = "white", "1" = "#C1CDCD"))+
    scale_y_discrete(limits=rev)+
    ggtitle(title)+
    xlab(dymension_label)+
    theme(
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 12,angle = 90, vjust = 0.5, hjust=1),
      legend.position = "none"
    )
  
  if (!is.null(reduced_instance_ticks)){
    gr <- gr + scale_x_discrete(breaks=reduced_instance_ticks,
                                labels=reduced_instance_ticks)
  }
  else{
    gr <- gr + scale_x_discrete(labels=levels(factor(dymension_overlapping_data$Dymension)))
  }
  return(gr)
}

instanceExperimentCoordinates <- getExperimentDymensionCoordinates("i",experiment)
instanceOverlappingData <- getOverlappingGraphData(instanceExperimentCoordinates,experiment$dataset_tags$instance_tags)
reducedInstanceTicks <- getReducedAxisTicks(instanceExperimentCoordinates,experiment$dataset_tags$instance_tags)
instanceGraph <- buildOverlappingGraph(instanceOverlappingData,"Instances",reduced_instance_ticks=reducedInstanceTicks)
instanceGraph

attributeExperimentCoordinates <- getExperimentDymensionCoordinates("a",experiment)
attributeOverlappingData <- getOverlappingGraphData(attributeExperimentCoordinates,experiment$dataset_tags$attribute_tags)
attributeGraph <- buildOverlappingGraph(attributeOverlappingData,"Attributes","Months")
attributeGraph

layerExperimentCoordinates <- getExperimentDymensionCoordinates("l",experiment)
layerOverlappingData <- getOverlappingGraphData(layerExperimentCoordinates,experiment$dataset_tags$slide_tags)
layerGraph <- buildOverlappingGraph(layerOverlappingData,"Layers","Years")
layerGraph









# lbl <- c(experiment$dataset_tags$instance_tags[[1]])
# coordinates <- data.frame()
# solution_index <- 1
# for (solution in experiment$solutions) {
#   instance_index_list <- levels(factor(solution$g))
#   tricluster <- paste0("Tricluster #",solution_index)
#   instances <- experiment$dataset_tags$instance_tags
#   partial_coordinates <- expand.grid(Tricluster=tricluster, Instances=instances)
#   values <- c()
#   intance_tag_index <- 0
#   instance_tag_occurences <- 0
#   while (intance_tag_index < length(experiment$dataset_tags$instance_tags)){
#     if (intance_tag_index %in% instance_index_list){
#       instance_tag_occurences <- instance_tag_occurences + 1
#       values <- c(values, "1")
#       if (instance_tag_occurences==1||instance_tag_occurences==length(instance_index_list)){
#         lbl <- c(lbl,experiment$dataset_tags$instance_tags[[intance_tag_index+1]])
#       }
#      
#     }
#     else{
#       values <- c(values, "0")
#     }
#     intance_tag_index <- intance_tag_index + 1
#   }
#   partial_coordinates$Values <- values
#   coordinates <- rbind(coordinates,partial_coordinates)
#   solution_index <- solution_index + 1
# }
# lbl <- c(lbl,experiment$dataset_tags$instance_tags[[length(experiment$dataset_tags$instance_tags)]])


# getReducedAxisTicks <- function(experiment_dymension_coordinates, dymension_tag_list){
#   reduced_axis_labels <- c(dymension_tag_list[[1]])
#   solution_index <- 1
#   while(solution_index <= length(experiment_dymension_coordinates)){
#     dymension_index_list <-  experiment_dymension_coordinates[[solution_index]]
#     dymension_tag_index <- 0
#     dymension_tag_occurences <- 0
#     while (dymension_tag_index < length(dymension_tag_list)){
#       if (dymension_tag_index %in% dymension_index_list){
#         dymension_tag_occurences <- dymension_tag_occurences + 1
#         if (dymension_tag_occurences==1||dymension_tag_occurences==length(dymension_tag_list)){
#           reduced_axis_labels <- c(reduced_axis_labels,dymension_tag_list[[dymension_tag_index+1]])
#         }
#       }
#       dymension_tag_index <- dymension_tag_index + 1
#     }
#     solution_index <- solution_index + 1
#   }
#   reduced_axis_labels <- c(reduced_axis_labels,dymension_tag_list[[length(dymension_tag_list)]])
#   return(reduced_axis_labels)
# }












