
#'@examples
#'  data_ym_mat=data_ymt[c(variables_m,variables_y)]|>as.matrix()
#'  mismat=data_ym_mat|>is.na()
#'  missingpositions=cbind(row(mismat)[c(mismat)],col(mismat)[c(mismat)])
#'  nmis=sum(mismat)
#'  mymis=rep(0,nmis)
#'  list(b=b,sigma=sigma,ymis=rep(0,nmis),missingpositions=missingpositions)}))



mcmc_initial_values_f<-
  function(data_ymt,variables_m,variables_y,empirical_hyper){
    with( empirical_hyper,
          ({
    b = cbind(
      matrix(0,K,Nm),
      matrix(c(b_bar_under) + 
      1e-2*t(chol(qinv_bar_under))%*%rnorm( K* Ny,1), K, Ny))
      
    sigma = s_bar_under/(v_bar_under - N - 1);
    nmis=data_ymt[c(variables_m,variables_y)]|>as.matrix()|>is.na()|>sum()
    list(b=b,sigma=sigma,mymis=rep(0,nmis))}))
  }

