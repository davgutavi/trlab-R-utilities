library(rgl)
source("load_environment.R")

dataset <- load_dataset_by_name("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration1/exp004/exp004.sol"

experiment <- loadExperiment(solPath)
experiment <- load_experiment_with_loaded_dataset(solPath,dataset)
solutions <- experiment[[1]]

years <- list(as.numeric(levels(factor(solutions[[1]]$t))))
for (i in c(2:length(solutions)) ) {
  years[[i]] <- as.numeric(levels(factor(solutions[[i]]$t)))
}


freq_list <- list()

i <- 1

y <- years[[i]]
