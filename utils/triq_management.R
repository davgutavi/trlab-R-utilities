require(dplyr)

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


load_triq_values <- function(solution_path, dataset_type) {
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

get_triq_table <- function(triq_solution_table, row_tag = "Solution") {
  triq_table <- t(as_tibble(triq_solution_table))
  rnames <- c()
  for (name in names(triq_solution_table)) {
    rnames <- c(rnames, paste0(row_tag,"-", name))
  }
  rownames(triq_table) <- rnames
  colnames(triq_table) <- toupper(names(triq_solution_table[[1]]))
  return (triq_table)
}

round_triq_table <- function(triq_table, digits=3){
  rounded_table <- triq_table
  for (i in c(1:nrow(rounded_table))){
    for (j in c(1:ncol(rounded_table))){
      rounded_table[[i,j]] <- round(rounded_table[[i,j]],digits)
    }
  }
  return(rounded_table)
}

round_triq_data <- function(triq_data, digits=3){
  rounded_data <- triq_data
  for (i in c(2:4)){
    rounded_data[[i]] <- round(rounded_data[[i]],digits)
  }
 
  for (i in c(1:length(rounded_data$triq_solutions))){
    for (j in c(1:length(rounded_data$triq_solutions[[i]]))){
      rounded_data$triq_solutions[[i]][[j]] <- round(rounded_data$triq_solutions[[i]][[j]],digits)
    }
  }
  
  return(rounded_data)
}




