library(rgl)
source("load_environment.R")

dataset <- load_dataset_by_name("france-monthly")

solPath <- "/Users/davgutavi/triclustering_france/exp004/exp004.sol"

# experiment <- loadExperiment(input)
experiment <- load_experiment_with_loaded_dataset(solPath,dataset)
solutions <- experiment[[1]]
sol1 <- solutions[1]
data1 <- sol1[[1]]

open3d()



lines3d(data1$el ~ data1$t | data1$s)
  
  
  x=data1$t,y=data1$s,z=data1$g)




