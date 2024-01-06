
mcmc_valeurs_initiales<-
  function(data_ymt,variables_m,variables_y,empirical_hyper){
    with( empirical_hyper,
    BB = cbind(
      matrix(0,K,Nm),
      matrix(c(b_bar_under) + 
      1e-2*t(chol(qinv_bar_under))%*%rnorm( K* Ny,1), K, Ny);
      
    Sigma = s_bar_under/(v_bar_under - N - 1);
    
    list(BB=BB,Sigma=sigma)

    
  }


run_mcmc<-function(data_ymt,p){}