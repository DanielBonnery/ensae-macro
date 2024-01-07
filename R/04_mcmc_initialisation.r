
mcmc_initial_values_f<-
  function(data_ymt,variables_m,variables_y,empirical_hyper){
    with( empirical_hyper,
          ({
    BB = cbind(
      matrix(0,K,Nm),
      matrix(c(b_bar_under) + 
      1e-2*t(chol(qinv_bar_under))%*%rnorm( K* Ny,1), K, Ny))
      
    Sigma = s_bar_under/(v_bar_under - N - 1);
    nmis=data_ymt[c(variables_m,variables_y)]|>as.matrix()|>is.na()|>sum()
    list(BB=BB,Sigma=sigma,ymis=rep(0,nmis))}))

  }
