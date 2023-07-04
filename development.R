
solution_path <- "/Users/davgutavi/triclustering_france/experiment_01/experiment_01.sol"
parent_path <- dirname(solution_path)
sol_name <- basename(solution_path)
experiment_name <- unlist(strsplit(sol_name, "\\."))[1]
filename <- paste(experiment_name,"_triq.csv",sep="")

filepath <- paste(parent_path,filename,sep="/")
csv <- read.csv(filepath, sep = ";" , header = TRUE )

