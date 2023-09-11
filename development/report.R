source("system/load_environment.R")
require(modeest)
require(ggplot2)

dataset <- loadDatasetByTrLabName("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration_2/it2_004/it2_004.sol"
experiment <- loadExperimentFromLoadedDataset(solPath,dataset)


getValueStatistics <- function(solutionValues){
  max <- max(solutionValues)
  min <- min(solutionValues)
  mean <- mean(solutionValues)
  median <- median(solutionValues)
  mode <- mfv(solutionValues)
  stdv <- sd(solutionValues)
  return(list(max=max,min=min,mean=mean,median=median,mode=mode,stdv=stdv))
}

getDymensioLabels<-function(solution_points,list_of_tags){
  labels <- c()
  for (ii in levels(factor(solution_points))) {
    labels <-
      append(labels, as.character(list_of_tags[as.numeric(ii) + 1]))
  }
  return(labels)
}

getValueStatistics(experiment$solutions[[1]]$el)
lbl <- getDymensioLabels(experiment$solutions[[1]]$g,experiment$dataset_tags$instance_tags)



df <- data.frame(Tricluster="tricluster 1",Dymension=levels(factor(experiment$solutions[[1]]$g)))


ggplot( df, aes(x=Dymension, y=Tricluster)) + 
  geom_line() +
  scale_x_discrete(labels=experiment$dataset_tags$instance_tags)
  
