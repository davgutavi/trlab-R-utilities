require(lattice)


getLegend <- function(legend_tag_list){
  legend_tag_list_length <- length(legend_tag_list)
  cols <- 0
  if (legend_tag_list_length<=4){
    cols <- legend_tag_list_length
  }
  else{
    cols <- ceiling(legend_tag_list_length/2)
  }
  
  return (list(text=legend_tag_list, space = "top",
       columns=cols))
}

getAtLabels <-
  function(solution_instances,
           instance_tags,
           visible_ticks = 5) {
    f_sol_intances <- factor(solution_instances)
    # total_ticks <- length(levels(f_sol_intances))-2
    total_ticks <- length(levels(f_sol_intances))
    
    if (visible_ticks == "all"){
      visible_ticks = total_ticks
    }
    
    div <- ceiling(total_ticks / visible_ticks)
    
    # at <- c(2)
    at <- c(1)
    i <- div
    while (i <= total_ticks - 1) {
      at <- c(at, i)
      i <- i + div
    }
    # at <- c(at, total_ticks - 1)
    at <- c(at, total_ticks)
    
    labels <- c()
    for (tck in at) {
      instance <- as.numeric(levels(f_sol_intances)[tck])
      labels <- c(labels, instance_tags[instance])
    }
    return(list(at = at, labels = labels))
  }

#' buildSolutionPatternGraph
#'
#' Builds a solution pattern graph.
#' @param solution A solution list.
#' @param tag_list The tag list of the dataset.
#' @return A list with the pattern graphs.
buildSolutionPatternGraph <- function(solution,
                                      tag_list,
                                      w = 20,
                                      h = 10,
                                      fsizeXaxis = 0.7,
                                      fsizeYaxis = 0.7,
                                      fsizeBoxes = 1.0,
                                      color = TRUE,
                                      value_tag = "values",
                                      layer_tag = "layers",
                                      instance_tag = "instances",
                                      ial_title = "Attributes for each Layer",
                                      ila_title = "Layers for each Attribute",
                                      lia_title = "Instances for each Attribute",
                                      visible_ticks = 5) {
  axis <- c(fsizeYaxis, fsizeXaxis)
  boxes <- list (cex = fsizeBoxes)
  
  inst  <- solution$g
  attr  <- solution$s
  layer  <- solution$t
  val <- solution$el
  
  f_inst <- factor(inst)
  f_attr <- factor(attr)
  f_layer <- factor(layer)
  
  at_labels <-
    getAtLabels(solution$g, tag_list$instance_tags, visible_ticks)
  
  instances <- c()
  for (ii in levels(f_inst)) {
    instances <-
      append(instances, as.character(tag_list$instance_tags[as.numeric(ii) + 1]))
  }
  
  attributes <- c()
  for (ai in levels(f_attr)) {
    attributes <-
      append(attributes, as.character(tag_list$attribute_tags[as.numeric(ai) + 1]))
  }
  
  layers <- c()
  for (li in levels(f_layer)) {
    layers <-
      append(layers, as.character(tag_list$slide_tags[as.numeric(li) + 1]))
  }
  
  # IAL
  # Attributes for each Layer
  # x = instances, o = attributes, p = layers
  ial_graph <- xyplot(
    val ~ f_inst | f_layer,
    solution,
    main = ial_title,
    xlab = instance_tag,
    ylab = value_tag,
    groups = attr,
    type = "a",
    font = "mono",
    scales = list(
      x = list(at = at_labels$at, labels = at_labels$labels, rot=90),
      cex = axis
    ),
    layout = c(1, nlevels(f_layer)),
    strip = strip.custom(factor.levels = layers, par.strip.text = boxes),
    auto.key =getLegend(attributes)
  )
  
  # ILA
  # Layers for each Attribute
  # x = instances, o = layers p = attributes
  ila_graph <- xyplot(
    val ~ f_inst | f_attr,
    solution,
    main = ila_title,
    xlab = instance_tag,
    ylab = value_tag,
    groups = layer,
    type = "a",
    font = "mono",
    scales = list(
      x = list(at = at_labels$at, labels = at_labels$labels, rot=90),
      cex = axis
    ),
    layout = c(1, nlevels(f_attr)),
    strip = strip.custom(factor.levels = attributes, par.strip.text = boxes),
    auto.key = getLegend(layers)
  )
  
  # LIA
  # Instances for each Attribute
  # x = layers, o = instances, p = attributes
  lia_graph <- xyplot(
    val ~ f_layer | f_attr,
    solution,
    main = lia_title,
    xlab = layer_tag,
    ylab = value_tag,
    groups = inst,
    type = "a",
    font = "mono",
    layout = c(1, nlevels(f_attr)),
    scales = list(x = list(labels = layers), cex = axis),
    strip = strip.custom(factor.levels = attributes, par.strip.text = boxes)
  )
  
  return(list(ial = ial_graph, ila = ila_graph, lia = lia_graph))
  
}



#' buildExperimentPatternGraphs
#'
#' Builds the solution combo without printing on any device
#' @param experiment A experiment list
#' @return A list with rows, columns and slide tags as a vector.
buildExperimentPatternGraphs <- function(experiment,
                                         w = 20,
                                         h = 10,
                                         fsizeXaxis = 0.7,
                                         fsizeYaxis = 0.7,
                                         fsizeBoxes = 1.0,
                                         color = TRUE,
                                         value_tag = "values",
                                         layer_tag = "layers",
                                         instance_tag = "instances",
                                         ial_title = "Attributes for each Layer",
                                         ila_title = "Layers for each Attribute",
                                         lia_title = "Instances for each Attribute",
                                         visible_ticks = 5, 
                                         index_graphs = TRUE) {
  solutions <- experiment$solutions
  gr_pattern_list <- list()
  i <- 1
  
  ial_title_base <- ial_title
  ila_title_base <- ila_title
  lia_title_base <- lia_title
  
  for (sol in solutions) {
    
    if(index_graphs){
      ial_title <- paste0("[Solution ",i,"] ",ial_title_base)
      ila_title <- paste0("[Solution ",i,"] ",ila_title_base)
      lia_title <- paste0("[Solution ",i,"] ",lia_title_base)
    }
    
    gr_pattern_list[[i]] <- buildSolutionPatternGraph(
      sol,
      experiment$dataset_tags,
      w,
      h,
      fsizeXaxis,
      fsizeYaxis,
      fsizeBoxes,
      color,
      value_tag,
      layer_tag,
      instance_tag,
      ial_title,
      ila_title,
      lia_title,
      visible_ticks
    )
    i <- i+1
  }
  return(gr_pattern_list)
  
}



























