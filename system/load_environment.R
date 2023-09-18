# General variables and libraries ----
require(properties)
require(XML)
require(modeest)
TrLabPath <- "/Users/davgutavi/TrLab4.0"
rsFolderPath <- paste0(TrLabPath,"/resources")
rsFilePath <- paste0(TrLabPath,"/resources/resources.xml")
rsData <- xmlParse(rsFilePath)
rsList <<- xmlToList(rsData)

# Global functions ----
getResourcesFolderPath <- function(){
  return(rsFolderPath)
}

closeXmlResources <- function() {
  rsList <<- NULL
}

# Load DATASET: ----
# by TrLab name ----
loadDatasetByTrLabName <- function(dataset_name){
  datasetInfo <- getDataset(dataset_name)
  datasetValues <- getDatasetValues(datasetInfo)
  datasetTags <- getGraphicalTags(datasetInfo)
  return(list(dataset_info=datasetInfo,
              dataset_values=datasetValues,
              dataset_tags=datasetTags))
}

# Load EXPERIMENT:----
# solutions, dataset info, values, tags and triq analysis ----
# From csv and a previously loaded dataset ----
loadExperimentFromLoadedDataset <- function(solution_path, loaded_dataset){
  props <- read.properties(solution_path)
  solutions <- getTriclusters(props,loaded_dataset$dataset_values)
  triqAnalysis <- load_triq_analysis(solution_path, loaded_dataset$dataset_info$.attrs[14])
  return(list(solutions=solutions,
              dataset_info= loaded_dataset$dataset_info,
              dataset_values=loaded_dataset$dataset_values,
              dataset_tags=loaded_dataset$dataset_tags,
              triq_analysis=triqAnalysis))
}

# From csv ----
loadExperimentFromPath <- function (solution_path){
  props <- read.properties(solution_path)
  loaded_dataset <- loadDatasetByTrLabName(props$dataset)
  experiment <- loadExperimentFromLoadedDataset(solution_path, loaded_dataset)
  return (experiment)
}



# LOW LEVEL FUNCTIONS: ---- 
# Tricluster ---- 
getTriclusters<-function(solProperties,datasetValues){
  
  aux <- unlist(strsplit(solProperties$solutions,"@"))
  triclusters <- list()
  print(paste0("Loading ",length(aux)," solutions"))
  
  i <- 1
  for(sol in aux){
    parts <- unlist(strsplit(sol,";"))
    gv <- as.numeric(unlist(strsplit(parts[1],",")))
    sv <- as.numeric(unlist(strsplit(parts[2],",")))
    tv <- as.numeric(unlist(strsplit(parts[3],",")))
    tri <- data.frame(g = numeric(0), s=numeric(0), t=numeric(0), el=numeric(0))
    print(paste0("Loading solution ",i," [",length(gv),",",length(sv),",",length(tv),"]"))
    
    for (g in gv){
      for (s in sv){
        for (t in tv){
          timeSlice = datasetValues[[t+1]]
          el = timeSlice[g+1,s+1]
          tri[nrow(tri)+1,]<-c(g,s,t,el)
        }
      }
    }
    
    triclusters[[length(triclusters)+1]]<-tri
    i<-i+1
  }
  
  return(triclusters)
  
}

# Dataset ---- 
getDataset <- function(datasetname) {
  dataset <- NULL
  enc <- FALSE
  i <- 1
  
  while (i <= length(rsList) & !enc) {
    dt <- rsList[i]
    dtname <- dt$dataset$.attrs["name"]
    i <- i + 1
    
    if (dtname == datasetname) {
      dataset <- dt
      enc <- TRUE
    }
    
  }
  
  return (dataset$dataset)
  
}

getDatasetValues <- function(datasetInfo) {
  valuePaths <- datasetInfo$"resources"
  rootPath <- paste0(getResourcesFolderPath(), "/", datasetInfo$.attr["id"])
  sep <- valuePaths$.attrs
  values <- list()
  
  for (i in c(1:(length(valuePaths) - 1))) {
    path <- paste0(rootPath, "/", valuePaths[i])
    show(paste0("Procesing path: ", path))
    t <- read.csv(path, sep = sep, header = FALSE)
    values[[i]] <- t
  }
  
  v000 <- (values[[1]])[1, 1]
  v0C0 <- (values[[1]])[1, ncol(values[[1]])]
  vG00 <- (values[[1]])[nrow(values[[1]]), 1]
  vGC0 <- (values[[1]])[nrow(values[[1]]), ncol(values[[1]])]
  v00T <- (values[[length(values)]])[1, 1]
  v0CT <- (values[[length(values)]])[1, ncol(values[[1]])]
  vG0T <- (values[[length(values)]])[nrow(values[[1]]), 1]
  vGCT <- (values[[length(values)]])[nrow(values[[1]]), ncol(values[[1]])]
  
  show(paste0("[", 0, ",", 0, ",", 0, "] - [", 1, ",", 1, ",", 1, "] = ", v000))
  show(paste0("[",0,",",ncol(values[[1]]) - 1,",",0,"] - [",1,",",ncol(values[[1]]) - 1,",",1,"] = ",v0C0 ))
  show(paste0("[",nrow(values[[1]]) - 1,",",0,",",0,"] - [",nrow(values[[1]]) - 1,",",1,",",1,"] = ", vG00))
  show(paste0("[",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",0,"] - [",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",1,"] = ",vGC0))
  show(paste0("[",0,",",0,",",length(values) - 1,"] - [",1,",",1,",",length(values) - 1,"] = ",v00T))
  show(paste0("[",0,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] - [", 1,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] = ",v0CT))
  show(paste0("[",nrow(values[[1]]) - 1,",",0,",",length(values) - 1,"] - [",nrow(values[[1]]) - 1,",",1,",",length(values) - 1,"] = ",vG0T))
  show(paste0("[",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] - [",nrow(values[[1]]) - 1,",",ncol(values[[1]]) - 1,",",length(values) - 1,"] = ",vGCT))
  
  return(values)
  
}

# Experiment tags for graphs ---- 
#' getGraphicalTags
#'
#' From an experiment object get the resources to make graphs
#' @param experiment The dataset info
#' @return A list with rows, columns and slide tags as a vector.
getGraphicalTags <- function(datasetInfo) {
  # Getting the rows, columns and slide tags file paths
  rootPath <- paste0(getResourcesFolderPath(), "/", datasetInfo$.attr["id"])
  intancePath <- paste0(rootPath, "/", datasetInfo$genes)
  attributePath <- paste0(rootPath, "/", datasetInfo$samples)
  layerPath <- paste0(rootPath, "/", datasetInfo$times)
  
  # Getting the tags from files
  in_tags <- as.vector(read.table(intancePath, sep = "\n")$V1)
  at_tags <- as.vector(read.table(attributePath, sep = "\n")$V1)
  sl_tags <- as.vector(read.table(layerPath, sep = "\n")$V1)
  
  return(list(
    instance_tags = in_tags,
    attribute_tags = at_tags,
    slide_tags = sl_tags
  ))
  
}

# TRIQ analysis ----
load_triq_csv <- function(solution_path, dataset_type) {
  parent_path <- dirname(solution_path)
  sol_name <- basename(solution_path)
  experiment_name <- unlist(strsplit(sol_name, "\\."))[1]
  filename <- paste(experiment_name, "_triq.csv", sep = "")
  filepath <- paste(parent_path, filename, sep = "/")
  
  selectet_colums <- c(1:10)
  if (dataset_type == 'e') {
    selectet_colums <- c(1:6)
  }
  triq_csv <-
    read.csv(filepath, sep = ";" , header = TRUE)[selectet_colums]
  triq_csv[is.na(triq_csv)] <- ""
  
  return(triq_csv)
  
}

load_triq_solution_table <- function(triq_csv, dataset_type) {
  triq_sol <- list()
  for (i in c(4:nrow(triq_csv))) {
    id <- strsplit(strsplit(triq_csv[i, 1], "\\{")[[1]], "\\}")[[2]]
    triq <- as.numeric(triq_csv[i, 2])
    if (dataset_type == 'b') {
      bioq <- as.numeric(triq_csv[i, 3])
      triqn <- as.numeric(triq_csv[i, 4])
      bioqn <- as.numeric(triq_csv[i, 5])
      grq <- as.numeric(triq_csv[i, 6])
      peq <- as.numeric(triq_csv[i, 7])
      spq <- as.numeric(triq_csv[i, 8])
      aux <-
        list(
          triq = triq,
          bioq = bioq,
          grq = grq,
          peq = peq,
          spq = spq,
          triqn = triqn,
          bioqn = bioqn
        )
    }
    else{
      grq <- as.numeric(triq_csv[i, 3])
      peq <- as.numeric(triq_csv[i, 4])
      spq <- as.numeric(triq_csv[i, 5])
      aux <- list(
        triq = triq,
        grq = grq,
        peq = peq,
        spq = spq
      )
    }
    triq_sol[[id]] = aux
  }
  return(triq_sol)
}

load_triq_analysis <- function(solution_path, dataset_type) {
  triq_csv <- load_triq_csv(solution_path, dataset_type)
  
  best_tricluster_id <-
    as.numeric(strsplit(strsplit(triq_csv[1, 3], "\\{")[[1]], "\\}")[[2]])
  best_triq_value <- as.numeric(triq_csv[1, 4])
  triq_mean <-  as.numeric(triq_csv[1, 5])
  triq_stdev <-  as.numeric(triq_csv[1, 6])
  
  triq_analysis <-
    list(
      best_tricluster_id = best_tricluster_id,
      best_triq_value = best_triq_value,
      triq_mean = triq_mean,
      triq_stdev = triq_stdev
    )
  
  if (dataset_type == 'b') {
    best_tricluster_id_n <-
      as.numeric(strsplit(strsplit(triq_csv[1, 7], "\\{")[[1]], "\\}")[[2]])
    best_triq_value_n <- as.numeric(triq_csv[1, 8])
    triq_mean_n <-  as.numeric(triq_csv[1, 9])
    triq_stdev_n <-  as.numeric(triq_csv[1, 10])
    triq_analysis[["best_tricluster_id_n"]] = best_tricluster_id_n
    triq_analysis[["best_triq_value_n"]] = best_triq_value_n
    triq_analysis[["triq_mean_n"]] = triq_mean_n
    triq_analysis[["triq_stdev_n"]] = triq_stdev_n
  }
  
  triq_analysis[["triq_solutions"]] <-
    load_triq_solution_table(triq_csv, dataset_type)
  
  return(triq_analysis)
  
}

# ACCESS functions ----


getSolutionDymensionCoordinates <- function(solution_index, dymension, experiment){
  coordinates <- switch(  
    dymension,  
    "i"= as.numeric(levels(factor(experiment$solutions[[solution_index]]$g))),  
    "a"= as.numeric(levels(factor(experiment$solutions[[solution_index]]$s))),  
    "l"= as.numeric(levels(factor(experiment$solutions[[solution_index]]$t))),  
    "v"= experiment$solutions[[solution_index]]$el
  )  
  return (coordinates)
}  


getSolutionCoordinates <- function(solution_index, experiment){
  instance_coordinates <- getSolutionDymensionCoordinates(solution_index,"i",experiment)
  attribute_coordinates <- getSolutionDymensionCoordinates(solution_index,"a",experiment)
  layer_coordinates <- getSolutionDymensionCoordinates(solution_index,"l",experiment)
  value_coordinates <- getSolutionDymensionCoordinates(solution_index,"v",experiment)
  return(list(instances=instance_coordinates,attributes=attribute_coordinates,
              layers=layer_coordinates,values=value_coordinates))
}


getExperimentDymensionCoordinates <- function(dymension, experiment){
  coordinates <- list()
  solution_index <- 1
  while(solution_index <= length(experiment$solutions)){
    coordinates[[solution_index]] <- getSolutionDymensionCoordinates(solution_index,dymension,experiment)
    solution_index <- solution_index + 1
  }
  return(coordinates)
}



getExperimentCoordinates <- function(experiment){
  coordinates <- list()
  solution_index <- 1
  while(solution_index <= length(experiment$solutions)){
    coordinates[[solution_index]] <- getSolutionCoordinates(solution_index,experiment)
    solution_index <- solution_index + 1
  }
  return(coordinates)
}

getDymensionLabels<-function(solution_dymension_points,list_of_tags){
  labels <- c()
  for (i in levels(factor(solution_dymension_points))) {
    labels <-
      append(labels, as.character(list_of_tags[as.numeric(i) + 1]))
  }
  return(labels)
}

getDymensionDatasetTags <- function(experiment, dymension){
  dymension_label_list <- switch(  
    dymension,  
    "i"= experiment$dataset_tags$instance_tags,  
    "a"= experiment$dataset_tags$attribute_tags,  
    "l"= experiment$dataset_tags$layer_tags
  )  
  return (dymension_label_list)
  
}


getSolutionSummary <- function(solution_index, experiment) {
  coordinates <- getSolutionCoordinates(solution_index, experiment)
  instance_size <- length(coordinates$instances)
  attribute_size <- length(coordinates$attributes)
  layer_size <- length(layers <- coordinates$layers)
  volume <- instance_size*attribute_size*layer_size
  max_value <- max(coordinates$values)
  min_value <- min(coordinates$values)
  mean_value <- mean(coordinates$values)
  median_value <- median(coordinates$values)
  mode_value <- mfv(coordinates$values)
  stdv_value <- sd(coordinates$values)
  return(list(
    volume = volume,
    instance_size = instance_size,
    attribute_size = attribute_size,
    layer_size = layer_size,
    max_value = max_value,
    min_value = min_value,
    mean_value = mean_value,
    stdv_value = stdv_value,
    median_value = median_value,
    mode_value = mode_value
  ))
}

getExperimentSummary <- function(experiment){
  experiment_summary <- data.frame()
  for (solution_index in c(1:length(experiment$solutions))){
    solution_summary = getSolutionSummary(solution_index,experiment)
    solution_summary_row <- data.frame(
      Tricluster = paste0("#",solution_index),
      Vol = solution_summary$volume,
      I = solution_summary$instance_size,
      A = solution_summary$attribute_size,
      L = solution_summary$layer_size,
      Max = solution_summary$max_value,
      Min = solution_summary$min_value,
      Mean = solution_summary$mean_value,
      Stdv = solution_summary$stdv_value,
      Median = solution_summary$median_value,
      Mode = paste(solution_summary$mode_value, collapse = ",")
    )
    experiment_summary <- rbind(experiment_summary,solution_summary_row)
  }
  return(experiment_summary)
}


