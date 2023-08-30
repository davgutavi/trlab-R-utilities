library("ggplot2")
library("reshape")

# Function to get the solution files names
get_solutions <- function( solution_path ){
  
  coordinates_path <- list.files(path = solution_path , 
                                 full.names = TRUE , 
                                 pattern = "coordinates")
  solutions <- list.files( path = coordinates_path , 
                           full.names = TRUE )
  return( solutions )
}

# Function to create the graphs
graph_creator <- function( df , sol_num ) {
  graph <- ggplot(df, aes(x = x, y = value, color = variable)) +
    geom_line() +
    scale_color_discrete(labels = gsub("X","Condición ",unique(df$variable))) +
    ggtitle(label = paste("Tricluster: ",sol_num)) +
    xlab(label = "Genes del tricluster") +
    ylab(label = "Valores de Expresión") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  return( graph )
}


# Function to create the data frames of the ggplot function
get_plots <- function( solution , fgroup , sgroup , sol_num ){ 
  # solution is a csv with the data of a solution
  group <- unique(solution[[fgroup]])
  plots <- vector('list', length(group))
  for(i in 1:length(group)){
    aux <- solution[solution[[fgroup]] == group[i],]
    elements <- aux$el
    con_group <- aux[[sgroup]]
    conditions <- unique(con_group)
    df <- data.frame()
    for(j in 1:length(conditions)) {
      if(length(df)==0){
        df <- data.frame(elements[con_group==conditions[j]])
      } else {
        df <- data.frame(df,elements[con_group==conditions[j]])
      }
    }
    names(df) <- conditions
    df <- data.frame(x = seq_along(df[, 1]),df)
    df <- melt(df, id.vars = "x")
    # We need to generate the graphs right here
    graph <- graph_creator( df = df , sol_num =  sol_num )
    plots[[i]] <- graph
  }
  return(plots)
}

# Functions that get the TriQ parameters of a CSV
get_triq <- function( solution_path ) {
  
  filename <- list.files( solution_path, pattern = "_triq.csv" , full.names = TRUE )
  csv <- read.csv( filename , sep = ";" , header = TRUE )
  return(csv)
  
}

get_triq_params <- function(csv) {
  
  return(csv[c(-1,-2,-3),1:7])
  
}

# Function to get the number of rows for a grid.arrange for the report.Rmd
get_nrow <- function(lista) {
  if(length(lista)%%2==0){
    nrow <- length(lista)/2
  } else {
    nrow <- (length(lista)+1)/2
  }
  return(nrow)
}


