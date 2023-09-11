require(dplyr)

get_triq_table <- function(triq_analysis, row_tag = "Solution") {
  triq_table <- t(as_tibble(triq_analysis))
  rnames <- c()
  for (name in names(triq_analysis)) {
    rnames <- c(rnames, paste0(row_tag,"-", name))
  }
  rownames(triq_table) <- rnames
  colnames(triq_table) <- toupper(names(triq_analysis[[1]]))
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

round_triq_analysis <- function(triq_data, digits=3){
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




