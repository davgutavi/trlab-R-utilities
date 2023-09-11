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