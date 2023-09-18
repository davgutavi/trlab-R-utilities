source("system/load_environment.R")
source("utils/experiment_analysis.R")

exp4 <-
  loadExperimentFromPath("/Users/davgutavi/triclustering_france/iteration_2/it2_004/it2_004.sol")
exp7 <-
  loadExperimentFromPath("/Users/davgutavi/triclustering_france/iteration_2/it2_007/it2_007.sol")


# INSTANCE COVERAGE

insGrExt4 <- buildDymensionOvelappingGraph(
  "i",
  exp4,
  dymension_font_size = 7,
  tricluster_font_size = 7,
  yes_color = "red",
  cell_lines = F,
  frame_line_size = 0.5,
  dymension_in_x_axis = T,
  reduced_instance_ticks = T
)

ggsave(
  "exp4.pdf",
  insGrExt4,
  "pdf",
  "/Users/davgutavi/triclustering_france/iteration_2",
  width = 17.5,
  height = 5.5,
  units = "cm"
)

insGrExt7 <- buildDymensionOvelappingGraph(
  "i",
  exp7,
  dymension_font_size = 7,
  tricluster_font_size = 7,
  yes_color = "red",
  cell_lines = F,
  frame_line_size = 0.5,
  dymension_in_x_axis = T,
  reduced_instance_ticks = T
)

ggsave(
  "exp7.pdf",
  insGrExt7,
  "pdf",
  "/Users/davgutavi/triclustering_france/iteration_2",
  width = 17.5,
  height = 5.5,
  units = "cm"
)


