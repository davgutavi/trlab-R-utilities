source("load_environment.R")
source("utils/experiment_management.R")
library(ggplot2)
dataset <- load_dataset_by_name("france-monthly")
solPath <- "/Users/davgutavi/triclustering_france/iteration1/exp004/exp004.sol"

#experiment <- loadExperiment(solPath)
experiment <- load_experiment_with_loaded_dataset(solPath,dataset)
long <- as.numeric(dataset$dataset_info$.attrs[[13]])
tag_list <- getGraphicalTags(experiment)
solutions <- experiment[[1]]


build_count_dym_dframe <- function(solutions, dym, dym_tags){
  dym_length <- length(dym_tags)
  dym_count_df <- data.frame(matrix(ncol = dym_length, nrow = length(solutions)))
  colnames(dym_count_df) <- dym_tags
  dym_count_df[] <- 0
  for (i in c(1:length(solutions)) ) {
    dyms <- as.numeric(levels(factor(solutions[[i]][[dym]])))
    print(dyms)
    for (e in dyms){
      dym_count_df[i,e+1] <- 1
    }
  }
  return(dym_count_df)
}

build_freq_dym_dframe <- function(dym_count_df, dym_tag){
  dym_freq_df <- count <- data.frame(matrix(ncol = 3, nrow = length(dym_count_df)))
  colnames(dym_freq_df) <- c(dym_tag, "Count", "Percentage")
  dym_freq_df[] <- 0
  for(i in c(1:length(dym_count_df))){
    dym_freq_df[i,1] <- i
    summatory <- sum(dym_count_df[,i]) 
    dym_freq_df[i,2] <- summatory
    dym_freq_df[i,3] <- paste0(as.character(summatory/length(solutions)*100),"%")
  }
  return(dym_freq_df)
}

build_freq_graph<- function(dym_freq_df,dym_tag, dym_tags){
  gr<- ggplot(dym_freq_df, aes(.data[[dym_tag]],Count)) +
    geom_col() +
    scale_x_continuous(breaks = dym_freq_df[[dym_tag]], labels = dym_tags) +
    geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25)
  return(gr)
}

month_count <- build_count_dym_dframe(solutions,"s",tag_list$attribute_tags)
year_count <- build_count_dym_dframe(solutions,"t",tag_list$slide_tags)
slot_count <- build_count_dym_dframe(solutions,"g",tag_list$instance_tags)

month_freq <- build_freq_dym_dframe(month_count,"Month")
year_freq <- build_freq_dym_dframe(year_count,"Year")
slot_freq <- build_freq_dym_dframe(slot_count,"Time")

build_freq_graph(month_freq,"Month",tag_list$attribute_tags)
build_freq_graph(year_freq,"Year",tag_list$slide_tags)
build_freq_graph(slot_freq,"Time",tag_list$instance_tags)

ggplot(slot_freq, aes(Time, Count)) +
  geom_col() +
  scale_x_continuous(breaks = slot_freq[["Time"]], labels = tag_list$instance_tags) 

p<-ggplot(slot_freq, aes(Time, Count)) +
  geom_col() +
  coord_flip() +
  scale_x_continuous(breaks = slot_freq[["Time"]], labels = tag_list$instance_tags) +
  geom_text(aes(label=Percentage), position=position_dodge(width=0.9), vjust=-0.25)

ggsave("/Users/davgutavi/Desktop/times.pdf",plot=p,device="pdf",
       width = 20, height = 50, units = "cm",scale=10,limitsize = FALSE)


