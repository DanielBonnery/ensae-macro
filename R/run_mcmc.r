
varlags_vec <- function(y,p){
  nn=length(y)
  plyr::aaply(0:p,1,function(pp){
    y[(p+1-pp):(nn-pp)]
  })|>t()
}


#'@examples
#'p=12
#'tar_load(data_ymt);tar_load(variables_y);tar_load(variables_m);
#' # matlab gives prior.minnesota.sigma=  0.0517    0.4839    0.1970    3.4035    0.5266    0.0996    0.2351
#' #R    ff4_hf   sp500_hf        gs1   logsp500    us_rgdp  us_gdpdef     ebpnew 
#       0.05214272 0.49491960 0.20017274 3.45821141 0.53511742 0.10122370 0.23883334 
#because they divide by n-1 and not by n-p also they replace missings by 0s...
# with n-1:
#ff4_hf   sp500_hf        gs1   logsp500    us_rgdp  us_gdpdef     ebpnew 
#0.05112355 0.48481814 0.19700358 3.40346054 0.52664537 0.09962111 0.23505210 


sigma_i_f<-
  function(data_ymt,variables_y,variables_m,p){
    data_ymt|>
      dplyr::select(one_of(variables_m,variables_y))|>
      plyr::llply(function(y){
        ym<-varlags_vec(y,p)
        #        lm(ym[,1]~ym[,-1])|>summary()|>(`[[`)("sigma")
        lm(ym[,1]~ym[,-1])$residuals|>sd()
        #lm(ym[,1]~ym[,-1])|>summary()|>(`[[`)("sigma")
      })|>unlist()}


s_bar_under_f<-function(sigma_i){diag(sigma_i^2)}

#'@examples
#'decay=decay_default
#'p=p_default
#'tightness=tightness_default
#'exog_std=exog_std_default
#'sigma_i=sigma_i_f(data_ymt,variables_y,variables_m,p=p)
#'qinv_bar_under_F(data_ymt,variables_y,variables_m,p,decay,sigma_i,tightness,exog_std)
qinv_bar_under_f<-
  function(data_ymt,variables_y,variables_m,p,decay,sigma_i,tightness,exog_std){
    N=length(c(variables_y,variables_m))
    Ny=length(variables_y)
temp1 <- kronecker((t(t(1:p)^(-decay))), matrix(1,N,N))# p^-d
temp2 <- matlab::repmat(sigma_i,p*N,1)# sigma_i
temp3 <- matlab::repmat(t(t(sigma_i^(-1))),p,N)# /sigma_j
Q0 <- tightness*temp1*temp2*temp3
Q0 <- rbind(Q0, matlab::repmat(exog_std,1,N))# this assumes there is only constant term in w
Q0 <- Q0^2
colnames(Q0)<-names(sigma_i)
Q0<-Q0[,variables_y] #<- []# drop the equations for m
K=nrow(Q0)
#Q <- diag(c(Q0))
Qinv <- diag(c(Q0^(-1)))
}
#'@examples
#'data_ymt
#'p=p_default
#'decay=decay_default
#'tightness=tightness_default
#'exog_std=exog_std_default
#'sigma_i=sigma_i_f(data_ymt,variables_y,variables_m,p=p)
#'qinv_bar_under(data_ymt,variables_y,variables_m,p,decay,sigma_i,tightness,exog_std)
empirical_hyper<-
  function(
    data_ymt,
    variables_y,
    variables_m,
    p,
    tightness,
    decay,
    exog_std){
    sigma_i=  sigma_i_f(data_ymt=data_ymt,variables_y=variables_y,variables_m=variables_m,p=p)
    qinv_bar_under=qinv_bar_under_f(data_ymt=data_ymt,variables_y=variables_y,variables_m=variables_m,p=p,
                        decay=decay,sigma_i=sigma_i,tightness=tightness,
                        exog_std=exog_std)
    
    s_bar_under<-s_bar_under_f(sigma_i)
    b_bar_under=NULL
    
    list(p=p,
         v_bar_under=length(variables_y)+2,
         sigma_i=sigma_i,
         decay=decay,
         tightness=tightness,
         Nm=length(variables_m),
         b_bar_under=b_bar_under,
         qinv_bar_under=qinv_bar_under,
         s_bar_under=NULL)
}



mcmc_valeurs_initiales<-
  function(data_ymt,variables_y,variables_m,p=12){
    
    sigma_i=sigma_i_f(data_ymt,variables_y,variables_m,p=12)

}


run_mcmc<-function(data_ymt,p){}