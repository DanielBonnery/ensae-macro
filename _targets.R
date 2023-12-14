# Load packages required to define the pipeline:
library(targets)
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with our custom functions:
"R"|>list.files(full.names = TRUE)|>sapply(FUN = source)


# Targets list:
list(
  #question 1. Generate X
  tar_target(
    name = data_set,
    command = get_data()))
