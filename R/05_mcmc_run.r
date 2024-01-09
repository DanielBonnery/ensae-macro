impute<-function(data_ym_mat,missingpositions,mymis){
  data_ym_mat[missingpositions]<-mymis
  data_ym_mat
}

mcmc_observations_f<-function(data_ymt,p,variables_m,variables_y){
  data_ym_mat=data_ymt|>dplyr::arrange(date)|>(`[`)(c(variables_m,variables_y))|>as.matrix()
  data_ym_mat[1:p,][is.na(data_ym_mat[1:p,])]<-0
  mismat=data_ym_mat|>is.na()
  missingpositions=cbind(row(mismat)[c(mismat)],col(mismat)[c(mismat)])
 list(data_ym_mat=data_ym_mat,missingpositions=missingpositions) 
}


mcmc_run<-function(data_ymt,variables_m,variables_y,p,empirical_hyper,mcmc_settings){
  empirical_hyper|>attach()|>suppressMessages()
  mcmc_initial_values_f(data_ymt,variables_m,variables_y,empirical_hyper)|>attach()|>suppressMessages()
  mcmc_observations<-mcmc_observations_f(data_ymt,p,variables_m,variables_y)
  mcmc_observations|>attach()|>suppressMessages()
  mcmc_settings|>attach()|>suppressMessages()
  
  chain_length<-chains_size*thining+burning
  

  b_sample<-NULL
  sigma_sample<-NULL

  pb = txtProgressBar(min = 2, max = chain_length, initial = 2) 
  
  
  for(i in 2:chain_length){
    mymis<-sample_mymis(sigma,b,mcmc_observations,empirical_hyper)
    data_ym_mat_i<-impute(data_ym_mat,missingpositions,mymis)
    my_mat_i<-varlags_mat(data_ymt_mat=data_ym_mat_i,
                          variables_m = variables_m,
                          variables_y = variables_y,p=p)
    sigma<-sample_sigma(b,my_mat_i,mcmc_observations,empirical_hyper)
    try(b<-sample_b(sigma = sigma,my_mat_i = my_mat_i,
                    empirical_hyper = empirical_hyper),silent = TRUE)
    if (i>mcmc_settings$burning& i%%mcmc_settings$thining==0){
      sigma_sample= abind::abind(sigma_sample,sigma,along=3)
      b_sample= abind::abind(b_sample,b,along=3)}
    setTxtProgressBar(pb,i)
  }
  close(pb)
  mcmc_chain=list(b_sample=b_sample,
       sigma_sample=sigma_sample)
  #Save result
  "outputs/mcmc_chain.rda"|>load()
  mcmc_chain
}

#'To save time, just load already computed
#mcmc_run=function(data_ymt,variables_m,variables_y,p,empirical_hyper,mcmc_settings){"outputs/mcmc_chain.rda"|>load()|>get()}
