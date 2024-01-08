
mcmc_initial_values_f<-
  function(data_ymt,variables_m,variables_y,empirical_hyper){
    with( empirical_hyper,
          ({
    b = cbind(
      matrix(0,K,Nm),
      matrix(c(b_bar_under) + 
      1e-2*t(chol(qinv_bar_under))%*%rnorm( K* Ny,1), K, Ny))
      
    sigma = s_bar_under/(v_bar_under - N - 1);
    mismat=data_ymt[c(variables_m,variables_y)]|>as.matrix()|>is.na()
    missingpositions=cbind(row(mismat)[c(mismat)],col(mismat)[c(mismat)])
    list(b=b,sigma=sigma,missingpositions=missingpositions)}))
  }
