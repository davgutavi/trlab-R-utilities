source("system/load_environment.R")

dataset <- loadDatasetByTrLabName("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration_2/it2_004/it2_004.sol"
# solPath <- "/Users/davgutavi/triclustering_france/iteration_1/it1_002/it1_002.sol"

# experiment <- loadExperiment(input)
experiment <- loadExperimentFromLoadedDataset(solPath,dataset)

# plotPatterns(input)
source("development/experiment_management_dev.R")
patternGraphs <- buildExperimentPatternGraphs(experiment,
                                              value_tag = "consumption",
                                              layer_tag = "year",
                                              instance_tag = "time slot",
                                              ial_title = "Monthly consumption series for each year",
                                              ila_title = "Yearly consumption series for each month",
                                              lia_title = "Annual consumption for each month",
                                              visible_ticks = "all")

patternGraphs[[1]]$ial
patternGraphs[[1]]$ila
