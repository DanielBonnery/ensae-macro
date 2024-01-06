# Load packages required to define the pipeline:
library(targets)
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with our custom functions:
"R"|>list.files(full.names = TRUE)|>sapply(FUN = source)

# Targets list:
list(
  #question 1. Generate X
  tar_target(
    name = raw_data,
    command = get_data_safe()),
  #question 1. Generate X
  tar_target(variables_y,c("gs1" ,"logsp500","us_rgdp","us_gdpdef","ebpnew")),
  tar_target(variables_m,c("ff4_hf","sp500_hf")),
  tar_target(variables_t,c("year","month")),
  tar_target(study_period,zoo::as.yearmon(c("1984-02","2016-12"))),
  tar_target(
    name = data_ymt,
    command = select_data(raw_data,variables_y,variables_m,variables_t,study_period))
  )
