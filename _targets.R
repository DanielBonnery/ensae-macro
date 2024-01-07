# Load packages required to define the pipeline:
library(targets)

options(clustermq.scheduler = "multicore")

#List all the packages needed and install them
renv::dependencies("./R")$Package->required_packages
required_packages|>writeLines("requirements.txt")
setdiff(required_packages,installed.packages()[,"Package"])|>sapply(install.packages)


#Load some libraries
library(ggplot2)

# Run the R scripts in the R/ folder with our custom functions:
"R"|>list.files(full.names = TRUE)|>sapply(FUN = source)


# Targets list:
list(
  #Download data
  tar_target(name = raw_data,command = get_data_safe()),
  #select data
  tar_target(variables_y,c("gs1" ,"logsp500","us_rgdp","us_gdpdef","ebpnew")),
  tar_target(variables_y_non_st,c("logsp500","us_rgdp","us_gdpdef")),
  tar_target(variables_m,c("ff4_hf","sp500_hf")),
  tar_target(variables_t,c("year","month")),
  tar_target(study_period,zoo::as.yearmon(c("1984-02","2016-12"))),
  tar_target(name = data_ymt,command = select_data(raw_data,variables_y,variables_m,variables_t,study_period)),
  #Model fixed parameters
  tar_target(tightness,.2),
  tar_target(decay,1),
  tar_target(p,12),
  tar_target(exog_std,100000),
  tar_target(empirical_hyper,
             empirical_hyper_f(
               data_ymt=data_ymt,
               variables_m=variables_m,
               variables_y=variables_y,
               p=p,
               tightness=tightness,
               decay=decay,
               exog_std=exog_std)),
  #Model draw values for mcm chain
  tar_target(mcmc_settings,list(nchains=0,chains_size=4000,burning=1000,thining=4)),
  #Model draw values for mcm chain
  tar_target(starting_points,(function(mcmc_settings){})()),
  tar_target(mcmc_chains,
             mcmc_run(data_ymt=data_ymt,
                      variables_m=variables_m,
                      variables_y=variables_y,
                      p=p,
                      hyper_parameter=hyper_parameter,
                      initial_condition_generator=initial_condition_generator)),
  tar_target(irfs_draws,
             draw_irfs(mcmc_chains=mcmc_chains,variables_m=variables_m,variables_y=variables_y)),
  #Outputs for the report
  tar_target(the_plot_0,plot_0(data_ymt,variables_m,variables_y,raw_data$dictionnary,,"outputs/the_plot_0.png")),
  tar_target(the_plot_00,plot_00(data_ymt,variables_m,variables_y,
                                 raw_data$dictionnary,
                                 path_out="outputs/the_plot_00.pdf")),
  tar_target(the_plot_1,plot_1(data_ymt,"outputs/the_plot_1.pdf")),
tar_target(the_plot_irfs,
           plot_irfs_draws(
             variables_m=variables_m,
             variables_y=variables_y,
             irfs_draws=irfs_draws,
             dictionnary=raw_data$dictionnary,
             path_out="outputs/the_plot_irfs.png")))
