source("load_environment.R")
source("utils/general_graphs.R")
source("development/experiment_management_dev.R")

dataset <- load_dataset_by_name("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration_2/it2_004/it2_004.sol"
experiment <- load_experiment_with_loaded_dataset(solPath,dataset)

getSolutionTags <- function(experimentTagList, solution){
  
  f_inst <- factor(solution$g)
  f_attr <- factor(solution$s)
  f_layer <- factor(solution$t)
  
  instances <- c()
  for (ii in levels(f_inst)) {
    instances <-
      append(instances, as.character(experimentTagList$instance_tags[as.numeric(ii) + 1]))
  }
  
  attributes <- c()
  for (ai in levels(f_attr)) {
    attributes <-
      append(attributes, as.character(experimentTagList$attribute_tags[as.numeric(ai) + 1]))
  }
  
  layers <- c()
  for (li in levels(f_layer)) {
    layers <-
      append(layers, as.character(experimentTagList$slide_tags[as.numeric(li) + 1]))
  }
  
  return (list(instances=instances,attributes=attributes,layers=layers))
  
}

getValueStatistics <- function(solutionValues){
  max <- max(solutionValues)
  min <- min(solutionValues)
  mean <- mean(solutionValues)
  median <- median(solutionValues)
  mode <- mfv(solutionValues)
  stdv <- sd(solutionValues)
  return(list(max=max,min=min,mean=mean,median=median,mode=mode,stdv=stdv))
}


tag_list <- getGraphicalTags(experiment)
solution <- experiment$solutions[[1]]
res <- getSolutionTags(tag_list,solution)

library(modeest)

mean(solution$el)
median(solution$el)
mfv(solution$el)
sd(solution$el)
max(solution$el)
min(solution$el)


distrMode(solution$g)


