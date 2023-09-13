require(modeest)
require(ggplot2)

getOverlappingGraphData <-
  function(experiment_dymension_coordinates,
           dymension_tag_list) {
    heatmap_coordinates <- data.frame()
    solution_index <- 1
    while (solution_index <= length(experiment_dymension_coordinates)) {
      dymension_index_list <-
        experiment_dymension_coordinates[[solution_index]]
      tricluster <- paste0("Tricluster #", solution_index)
      dymension <- dymension_tag_list
      partial_heatmap_coordinates <-
        expand.grid(Tricluster = tricluster, Dymension = dymension)
      values <- c()
      dymension_tag_index <- 0
      while (dymension_tag_index < length(dymension_tag_list)) {
        if (dymension_tag_index %in% dymension_index_list) {
          values <- c(values, "1")
        }
        else{
          values <- c(values, "0")
        }
        dymension_tag_index <- dymension_tag_index + 1
      }
      partial_heatmap_coordinates$Values <- values
      heatmap_coordinates <-
        rbind(heatmap_coordinates, partial_heatmap_coordinates)
      solution_index <- solution_index + 1
    }
    return(heatmap_coordinates)
  }

getReducedAxisTicks <-
  function(experiment_dymension_coordinates,
           dymension_tag_list) {
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
    return(reduced_axis_labels)
  }


buildOverlappingGraph <- function(dymension_overlapping_data,
                                  title = "",
                                  dymension_label = "",
                                  dymension_font_size = 8,
                                  tricluster_label = "",
                                  tricluster_font_size = 8,
                                  cell_line_size = 0.5,
                                  yes_color = "black",
                                  no_color = "white",
                                  dymension_in_x_axis = F,
                                  reduced_instance_ticks = NULL) {
  
  x_axis <- dymension_overlapping_data$Tricluster
  x_lab <- tricluster_label
  x_font_size <- tricluster_font_size
  y_axis <- factor(dymension_overlapping_data$Dymension)
  y_lab <- dymension_label
  y_font_size <- dymension_font_size
  
  if (dymension_in_x_axis == T) {
    x_axis <- factor(dymension_overlapping_data$Dymension)
    x_lab <- dymension_label
    x_font_size <- dymension_font_size
    y_axis <- dymension_overlapping_data$Tricluster
    y_lab <- tricluster_label
    y_font_size <- tricluster_font_size
  }
  
  gr <- ggplot(dymension_overlapping_data, aes(x_axis, y_axis)) +
    geom_tile(aes(fill = Values), colour = "black", linewidth = cell_line_size) +
    scale_fill_manual(values = c("0" = no_color, "1" = yes_color)) +
    ggtitle(title) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme(
      axis.text.y = element_text(size = y_font_size),
      axis.text.x = element_text(
        size = x_font_size,
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill=NA, linewidth=2)
    )
  
  if (!is.null(reduced_instance_ticks)) {
    if (dymension_in_x_axis == F) {
      gr <- gr + scale_y_discrete(breaks = reduced_instance_ticks,
                                  labels = reducedInstanceTicks)
    }
    else{
      gr <- gr + scale_x_discrete(breaks = reduced_instance_ticks,
                                  labels = reduced_instance_ticks)
    }
  }
  else{
    if (dymension_in_x_axis == F) {
      gr <- gr + scale_y_discrete(labels = levels(y_axis))
    }
    else{
      gr <- gr + scale_x_discrete(labels = levels(x_axis))
    }
  }
  return(gr)
}


# instanceExperimentCoordinates <- 
#                           getExperimentDymensionCoordinates("i",experiment)
# instanceOverlappingData <- 
#                           getOverlappingGraphData(instanceExperimentCoordinates,
#                                                   experiment$dataset_tags$instance_tags)
# reducedInstanceTicks <- 
#                           getReducedAxisTicks(instanceExperimentCoordinates,
#                                               experiment$dataset_tags$instance_tags)
# instanceGraph <- buildOverlappingGraph(instanceOverlappingData,
#                                 "Instances",reduced_instance_ticks=reducedInstanceTicks)