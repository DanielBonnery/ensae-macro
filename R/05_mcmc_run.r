mcmc_observations<-function(data_ymt,p){
  varlags_mat(dat_ymt,)
  
  
  
}



mcmc_run<-function(data_ymt,variables_m,variables_y,p,hyper_parameter,initial_condition_generator){
  if(FALSE){
  attach(empirical_hyper)
  mcmc_initial_values<-mcmc_initial_values_f(data_ymt,variables_m,variables_y,empirical_hyper)
  
  library(rjags)
   model.text<-"model{
   Sigmainv~dwish(s_bar_under,v_bar_under)
   
      }"
  
  jags.var<- 
    jags(model.file=textConnection(model.text),
         data=c(empirical_hyper,
                list()),
         inits=list(mcmc_initial_values),
         n.chains=1,
         parameters.to.save=c('beta0','beta1','sigma'),
         n.burnin = 1000, n.iter=20000,DIC=TRUE)  
  
  jags.dugong$BUGSoutput$summary["beta1",
                                 c("2.5%","97.5%")]
  jags.dugong$BUGSoutput[c("DIC","pD")]
  beta1.1<-jags.dugong$BUGSoutput$sims.list$beta1
  
  
  }
  NULL
}
