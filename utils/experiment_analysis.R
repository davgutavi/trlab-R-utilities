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
      
      left_label <- dymension_tag_list[[dymension_index_list[[1]]+1]]
      right_label <- dymension_tag_list[[dymension_index_list[[length(dymension_index_list)]]]]
      
      if (!left_label %in% reduced_axis_labels){
        reduced_axis_labels <- c(reduced_axis_labels, left_label)
      }
      if (!right_label %in% reduced_axis_labels){
        reduced_axis_labels <- c(reduced_axis_labels, right_label)
      }
      solution_index <- solution_index + 1
    }
    reduced_axis_labels <-
      c(reduced_axis_labels, dymension_tag_list[[length(dymension_tag_list)]])
    return(reduced_axis_labels)
  }

trimReducedAxisTicks <- function(reduced_axis_ticks, dimension_tags, left_limit = 1,
                                 right_limit = 1){
  res <- c()
  index <- 1
  ordered_axis_ticks <- sortAxisTicks(reduced_axis_ticks, dimension_tags)
  print(ordered_axis_ticks)
  while (index<=length(ordered_axis_ticks)){
    if (index > left_limit && index < (length(ordered_axis_ticks)-right_limit+1)){
      res <- c(res, ordered_axis_ticks[index])
    }
    index <- index +1
  }
  print(res)
  return (res)
}



sortAxisTicks <- function(axis_ticks, dimension_tags) {
  swap_performed <- T
  while (swap_performed) {
    swap_performed <- F
    for (i in 1:(length(axis_ticks) - 1)) {
      left <- match(axis_ticks[i],dimension_tags)
      right <- match(axis_ticks[i+1],dimension_tags)
      if (left > right) {
        tmp <- axis_ticks[i]
        axis_ticks[i] <- axis_ticks[i + 1]
        axis_ticks[i + 1] <- tmp
        swap_performed <- T
      }
    }
  }
  return(axis_ticks)
}

  
  


buildOverlappingGraph <- function(dymension_overlapping_data,
                                  title = "",
                                  dymension_label = "",
                                  dymension_font_size = 8,
                                  tricluster_label = "",
                                  tricluster_font_size = 8,
                                  cell_lines = T,
                                  cell_line_size = 0.5,
                                  cell_line_color = "black",
                                  yes_color = "black",
                                  no_color = "white",
                                  dymension_in_x_axis = F,
                                  frame_line_size = 2,
                                  font_family = "Courier",
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
  
  tiles = NULL
  if (cell_lines==T){
    tiles = geom_tile(colour = cell_line_color, linewidth = cell_line_size)
  }
  else{
    tiles = geom_tile()
  }
  
  gr <- ggplot(dymension_overlapping_data, aes(x_axis, y_axis, fill = Values)) +
    tiles +
    scale_fill_manual(values = c("0" = no_color, "1" = yes_color)) +
    ggtitle(title) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme(
      axis.text.y = element_text(size = y_font_size, family = font_family),
      axis.text.x = element_text(
        size = x_font_size,
        family = font_family,
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill=NA, linewidth=frame_line_size)
    )
  
  if (!is.null(reduced_instance_ticks)) {
    if (dymension_in_x_axis == F) {
      gr <- gr + scale_y_discrete(breaks = reduced_instance_ticks,
                                  labels = reduced_instance_ticks)
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


buildDymensionOvelappingGraph <- function(dymension, experiment, 
                                          title = "",
                                          dymension_label = "",
                                          dymension_font_size = 8,
                                          tricluster_label = "",
                                          tricluster_font_size = 8,
                                          cell_lines = T,
                                          cell_line_size = 0.5,
                                          cell_line_color = "black",
                                          yes_color = "black",
                                          no_color = "white",
                                          dymension_in_x_axis = F,
                                          frame_line_size = 2,
                                          font_family = "Courier",
                                          reduced_instance_ticks = F,
                                          trim_instace_ticks = F,
                                          left_limit = 1,
                                          right_limit = 1){
  
  dymensionExperimentCoordinates <- getExperimentDymensionCoordinates(dymension,experiment)
  
  dymensionDatasetTags <- getDymensionDatasetTags(experiment, dymension)
  
  dymensionOverlappingData <- getOverlappingGraphData(dymensionExperimentCoordinates,
                                                      dymensionDatasetTags)
  reducedDymensionTicks <- NULL
  if (reduced_instance_ticks ==T){
    reducedDymensionTicks <- getReducedAxisTicks(dymensionExperimentCoordinates,
                                                dymensionDatasetTags)
    
    if (trim_instace_ticks == T){
      reducedDymensionTicks <-  
        trimReducedAxisTicks(reducedDymensionTicks,dymensionDatasetTags,left_limit,right_limit)
    }
   
    
  }
  
  dymensionOverlappingGraph <- buildOverlappingGraph(dymensionOverlappingData,
                                         title,
                                         dymension_label,
                                         dymension_font_size,
                                         tricluster_label,
                                         tricluster_font_size,
                                         cell_lines,
                                         cell_line_size,
                                         cell_line_color,
                                         yes_color,
                                         no_color,
                                         dymension_in_x_axis,
                                         frame_line_size,
                                         font_family,
                                         reduced_instance_ticks=reducedDymensionTicks)
  
  return(dymensionOverlappingGraph)
}

