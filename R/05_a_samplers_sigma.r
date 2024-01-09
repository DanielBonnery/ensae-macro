sample_sigma<-function(b,my_mat_i,mcmc_observations,empirical_hyper){
  with(c(empirical_hyper,my_mat_i),{
  
  
  u = y - cbind(x,1)%*%b;
  Spost = t(u)%*%u + s_bar_under;
  Spost_chol = t(chol(Spost));
  temp = matrix(rnorm(v_bar_under* N),v_bar_under,N);
  Spost_chol%*%solve(t(temp)%*%temp)%*%t(Spost_chol)})
}

