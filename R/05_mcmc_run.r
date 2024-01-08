mcmc_observations_f<-function(data_ymt,p,variables_m,variables_y){
  varlags_mat(dat_ymt,variables_m = variables_m,variables_y = variables_y,p=p)
  
  
  
}



mcmc_run<-function(data_ymt,variables_m,variables_y,p,hyper_parameter,mcmc_settings){
  attach(empirical_hyper)
  mcmc_initial_values<-mcmc_initial_values_f(data_ymt,variables_m,variables_y,empirical_hyper)
  mcmc_observations<-mcmc_observations_f(data_ymt,p,variables_m,variables_y)
  
  chain_length<-mcmc_settings$chains_size*mcmc_settings$thining+mcmc_settings$burning
  
  
  b=mcmc_initial_values$b
  sigma=mcmc_initial_values$b
  
  b_sample<-array(NA,c(dim(b),chain_length))
  sigma_sample<-array(NA,c(dim(sigma),chain_length))
  
  for(i in 1:chain_length){
    mymis<-sample_ymis(sigma,b,mcmc_observations,empirical_hyper)
    my<-impute(mcmcobservation,nymis)
    sigma<-sample_sigma(b,y,mcmc_observations,empirical_hyper)
    b<-sample_b(sigma,my,mcmc_observations,empirical_hyper)
    
    sigma_sample[,,,i]=sigma
        b_sample[,,,i]=b
  }
  
}
