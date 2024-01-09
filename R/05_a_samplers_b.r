sample_b<-function(sigma,my_mat_i,empirical_hyper){
  with(c(empirical_hyper,my_mat_i),{
    x=cbind(x,1)
    n_y=which(is.element(c(variables_m,variables_y),variables_y))
    n_m=which(is.element(c(variables_m,variables_y),variables_m))
    Csig = chol(t(sigma));
    SigmaYY1inv = proj(proj(t(Csig[n_y,n_y]),Csig[n_y,n_y]),diag(Ny));
    A = qinv_bar_under + kronecker(SigmaYY1inv, t(x)%*%x);
    yst = y[,n_y] - y[,n_m]%*%solve(t(Csig[n_m,n_m]))%*%t(Csig[n_y,n_m]);
    a = qinvb_bar_under + t(x)%*%yst%*%SigmaYY1inv;
    C = chol(A);
    B = proj(C, proj(t(C),c(a) + rnorm(K*Ny))); 
    BB = cbind(matrix(0,K,Nm) matrix(B, K, Ny));})
  
}
