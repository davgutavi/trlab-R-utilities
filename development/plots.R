source("load_environment.R")
source("utils/general_graphs.R")


dataset <- load_dataset_by_name("france-monthly")

solPath <- "/Users/davgutavi/triclustering_france/iteration_2/it2_004/it2_004.sol"

solPath <- "/Users/davgutavi/triclustering_france/iteration_1/it1_002/it1_002.sol"

# experiment <- loadExperiment(input)
experiment <- load_experiment_with_loaded_dataset(solPath,dataset)


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



factor(experiment$solutions[[1]]$g)


solution_path <- "/Users/davgutavi/triclustering_france/experiment_01/experiment_01.sol"
parent_path <- dirname(solution_path)
sol_name <- basename(solution_path)
experiment_name <- unlist(strsplit(sol_name, "\\."))[1]
filename <- paste(experiment_name,"_triq.csv",sep="")

filepath <- paste(parent_path,filename,sep="/")
csv <- read.csv(filepath, sep = ";" , header = TRUE )
