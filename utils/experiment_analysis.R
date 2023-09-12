require(modeest)

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