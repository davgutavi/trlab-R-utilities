source("system/load_environment.R")
source("utils/experiment_management.R")

dataset <- load_dataset_by_name("france-monthly")

solPath <- "/Users/davgutavi/triclustering_dev/dev_010/dev_010.sol"

solPath <- "/Users/davgutavi/triclustering_france/it1_001/it1_001.sol"
# solPath <- "/Users/davgutavi/triclustering_france/france/france.sol"
# solPath <- "/Users/davgutavi/triclustering_france/exp001/exp001.sol"

# experiment <- loadExperiment(input)
experiment <- load_experiment_with_loaded_dataset(solPath,dataset)

# plotPatterns(input)
patternGraphs <- buildExperimentPatternGraphs(experiment,
                                              value_tag = "consumption",
                                              layer_tag = "year",
                                              instance_tag = "time slot",
                                              ial_title = "Monthly consumption series for each year",
                                              ila_title = "Yearly consumption series for each month",
                                              lia_title = "Annual consumption for each month")

patternGraphs[[1]]$ial



aux1 <- paste0(unlist(strsplit(solPath, "/")))
aux2 <- aux1[-length(aux1)]
aux3 <-paste0(aux2,collapse="/")
outPath <- paste0(aux3, "/graphs/")
dir.create(outPath,showWarnings = FALSE)

i <- 1
trellis.device(device="pdf", color=TRUE)
for (sol_gr in patternGraphs){
  out <- paste0(outPath,"graph_tri_",i,".pdf")
  pdf(file = out, colormodel="rgb",paper = "a4",width=20, height=20)
  print(sol_gr$ial)
  print(sol_gr$ila)
  print(sol_gr$lia)
  dev.off() 
  i <- i+1
}

for (sol_gr in patternGraphs){
  print(sol_gr$ial)
  print(sol_gr$ila)
  print(sol_gr$lia)  
  i <- i+1
}

print(patternGraphs[[1]]$ial)


i <- 1
for (sol_gr in patternGraphs){
  out <- paste0(outPath,"graph_tri_",i,".eps")
  trellis.device(device="postscript", color=TRUE)
  postscript(file = out, colormodel="rgb",onefile=FALSE, horizontal=FALSE,paper = "special",width=20, height=20)
  print(sol_gr$ial,split=c(1,1,3,1),more = TRUE)
  print(sol_gr$ila,split=c(2,1,3,1),more = TRUE)
  print(sol_gr$lia,split=c(3,1,3,1),more = FALSE)
  dev.off() 
  i <- i+1
}






