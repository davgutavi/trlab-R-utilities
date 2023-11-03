source("system/load_environment.R")
source("utils/experiment_analysis.R")
source("utils/tricluster_plots.R")
source("utils/triq_report.R")


solPath <- "/Users/davgutavi/triclustering_rat/preliminar/ex1/ex1.sol"

experiment <- loadExperimentFromPath(solPath)
summary_table <- getExperimentSummary(experiment)

#NO lee correctamente el fichero
in_tags <- as.vector(read.table("/Users/davgutavi/TrLab4.0/resources/0022/gse8988d_genes.txt", sep = "\n",comment.char = "")$V1)

aux<- read.delim("/Users/davgutavi/TrLab4.0/resources/0022/gse8988d_genes.txt", header = F,sep = "\n")


in_tags <- as.vector(read.table("/Users/davgutavi/TrLab4.0/resources/0022/gse8988d_genes.txt", sep = "\n",comment.char = "")$V1)

instanceExperimentCoordinates <- getExperimentDymensionCoordinates("i",experiment)
instanceExperimentCoordinates[[1]]
instanceExperimentCoordinates[[2]]

getReducedAxisTicks(instanceExperimentCoordinates,experiment$dataset_tags$instance_tags)
experiment_dymension_coordinates <- instanceExperimentCoordinates
dymension_tag_list <- experiment$dataset_tags$instance_tags

reduced_axis_labels <- c(dymension_tag_list[[1]])
solution_index <- 1
while (solution_index <= length(experiment_dymension_coordinates)) {
  dymension_index_list <-
    experiment_dymension_coordinates[[solution_index]]
  
  left_label <- dymension_tag_list[[dymension_index_list[[1]]+1]]
  right_label <- dymension_tag_list[[dymension_index_list[[length(dymension_index_list)]]]]
  
  if (!left_label %in% reduced_axis_labels){
    reduced_axis_labels <- c(reduced_axis_labels, left_label)
  }
  if (!right_label %in% reduced_axis_labels){
    reduced_axis_labels <- c(reduced_axis_labels, right_label)
  }
  solution_index <- solution_index + 1
}
reduced_axis_labels <-
  c(reduced_axis_labels, dymension_tag_list[[length(dymension_tag_list)]])



















gse8988_dataset <- loadDatasetByTrLabName("gse8988d")


solution_path <- "/Users/davgutavi/triclustering_rat/preliminar/ex1/ex1.sol"
dataset_type <- gse8988_dataset$dataset_info$.attrs[14]


props <- read.properties(solution_path)
solutions <- getTriclusters(props,gse8988_dataset$dataset_values)
triqAnalysis <- load_triq_analysis(solution_path, gse8988_dataset$dataset_info$.attrs[14])





triq_csv <- load_triq_csv(solution_path, dataset_type)

filepath<-"/Users/davgutavi/triclustering_rat/preliminar/ex1/ex1_triq.csv"

filepath<-"/Users/davgutavi/triclustering_france/iteration_1/it1_001/it1_001_triq.csv"
selectet_colums <- c(1:10)
triq_csv_a <- read.csv(filepath, sep = ";", header = T)[selectet_colums]
triq_csv_a <- read.csv(filepath, sep = ";" , header = T, row.names = 1)[selectet_colums]
triq_csv_b <- read.csv(filepath, sep = ";" , header = F,row.names = NULL)[selectet_colums]
triq_csv[is.na(triq_csv)] <- ""







exp4 <- loadExperimentFromPath("/Users/davgutavi/ex1/ex1.sol")
exp7 <- loadExperimentFromPath("/Users/davgutavi/triclustering_france/iteration_2/it2_007/it2_007.sol")

# LATEX TABLES ----
library(xtable)
exp4_rounded_triq <- round_triq_analysis(exp4$triq_analysis)
exp4_triq_table <- get_triq_table(exp4_rounded_triq$triq_solutions)
exp4_latex_table <- xtable(exp4_triq_table)
print(exp4_latex_table)

exp7_rounded_triq <- round_triq_analysis(exp7$triq_analysis)
exp7_triq_table <- get_triq_table(exp7_rounded_triq$triq_solutions)
exp7_latex_table <- xtable(exp7_triq_table)
print(exp7_latex_table)

# INSTANCE COVERAGE ----
insGrExt4 <- buildDymensionOvelappingGraph("i",exp4,
  dymension_font_size = 7,
  tricluster_font_size = 7,
  yes_color = "red",
  cell_lines = F,
  frame_line_size = 0.5,
  dymension_in_x_axis = T,
  reduced_instance_ticks = T,
  trim_instace_ticks = T,
  left_limit = 1,
  right_limit = 1
)

insGrExt4
ggsave("exp4_inst.pdf",insGrExt4,"pdf","/Users/davgutavi/Desktop",width = 17.5,height = 5.5,units = "cm")

insGrExt7 <- buildDymensionOvelappingGraph("i",exp7,
  dymension_font_size = 7,
  tricluster_font_size = 7,
  yes_color = "red",
  cell_lines = F,
  frame_line_size = 0.5,
  dymension_in_x_axis = T,
  reduced_instance_ticks = T,
  trim_instace_ticks = T,
  left_limit = 2,
  right_limit = 2
)
insGrExt7
ggsave("exp7_inst.pdf",insGrExt7,"pdf","/Users/davgutavi/Desktop",width = 17.5,height = 5.5,units = "cm")

# ATTRIBUTE COVERAGE ----
attrGrExt4 <- buildDymensionOvelappingGraph("a",exp4,
  dymension_font_size = 7,
  tricluster_font_size = 7,
  yes_color = "green",
  dymension_in_x_axis = T
)

print(attrGrExt4)
ggsave("exp4_attr.pdf",attrGrExt4,"pdf","/Users/davgutavi/Desktop",width = 5.5,height = 5.5,units = "cm")

attrGrExt7 <- buildDymensionOvelappingGraph("a",exp7,
                                            dymension_font_size = 7,
                                            tricluster_font_size = 7,
                                            yes_color = "green",
                                            dymension_in_x_axis = T
)

print(attrGrExt7)
ggsave("exp7_attr.pdf",attrGrExt7,"pdf","/Users/davgutavi/Desktop",width = 5.5,height = 5.5,units = "cm")

# LAYER COVERAGE ----
layerGrExt4 <- buildDymensionOvelappingGraph("l",exp4,
                                            dymension_font_size = 7,
                                            tricluster_font_size = 7,
                                            yes_color = "blue",
                                            dymension_in_x_axis = T
)

print(layerGrExt4)
ggsave("exp4_layer.pdf",layerGrExt4,"pdf","/Users/davgutavi/Desktop",width = 5.5,height = 5.5,units = "cm")

layerGrExt7 <- buildDymensionOvelappingGraph("l",exp7,
                                            dymension_font_size = 7,
                                            tricluster_font_size = 7,
                                            yes_color = "blue",
                                            dymension_in_x_axis = T
)

print(layerGrExt7)
ggsave("exp7_layer.pdf",layerGrExt7,"pdf","/Users/davgutavi/Desktop",width=5.5,height=5.5,units="cm")

# PATTERNS ----
exp4_patt <- buildSolutionPatternGraph(exp4$solutions[[4]],exp4$dataset_tags,
                                       # w = 20,
                                       # h = 10,
                                       fsizeXaxis = 0.5,
                                       fsizeYaxis = 0.5,
                                       # fsizeBoxes = 1.0,
                                       # color = TRUE,
                                       tags_size = 8,
                                       value_tag = "Consumption (TWh)",
                                       layer_tag = "Year",
                                       instance_tag = "Time slot",
                                       ial_title = "",
                                       ila_title = "",
                                       lia_title = "",
                                       leyend_font_size = 0.5,
                                       visible_ticks = 15)

print(exp4_patt$ial)
print(exp4_patt$ila)
# print(exp4_patt$lia)

pdf("/Users/davgutavi/Desktop/exp4_patt_months.pdf",width=5,height=5)
print(exp4_patt$ial)
dev.off()

pdf("/Users/davgutavi/Desktop/exp4_patt_years.pdf",width=5,height=5)
print(exp4_patt$ila)
dev.off()

exp7_patt <- buildSolutionPatternGraph(exp7$solutions[[6]],exp7$dataset_tags,
                                       # w = 20,
                                       # h = 10,
                                       fsizeXaxis = 0.5,
                                       fsizeYaxis = 0.5,
                                       # fsizeBoxes = 1.0,
                                       # color = TRUE,
                                       tags_size = 8,
                                       value_tag = "Consumption (TWh)",
                                       layer_tag = "Year",
                                       instance_tag = "Time slot",
                                       ial_title = "",
                                       ila_title = "",
                                       lia_title = "",
                                       leyend_font_size = 0.5,
                                       visible_ticks = 15)


print(exp7_patt$ial)
print(exp7_patt$ila)
# print(exp7_patt$lia)

pdf("/Users/davgutavi/Desktop/exp7_patt_months.pdf",width=5,height=5)
print(exp7_patt$ial)
dev.off()

pdf("/Users/davgutavi/Desktop/exp7_patt_years.pdf",width=5,height=5)
print(exp7_patt$ila)
dev.off()





