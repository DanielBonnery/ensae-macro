#'@examples
#'tar_load(data_ymt)
#'y=data_ymt$ff4_hf
#' p=p_default
#' varlags_vec(y,p)
#' varlags_vec(y,p,"toto")
varlags_vec <- function(y,p,colname=NULL){
  nn=length(y)
  x<-plyr::aaply(1:p,1,function(pp){
    y[(p+1-pp):(nn-pp)]
  })|>t()
  if(is.null(colname)){x}else{x|>(`colnames<-`)(paste0(colname,1:p))}
}

#'@examples
#'tar_load(data_ymt)
#'data_ymt_mat=as.matrix(data_ymt)
#' p=p_default
#' varlags_mat(data_ymt_mat,p,variables_m,variables_y)|>dim()
varlags_mat <- function(data_ymt_mat,p,variables_m,variables_y){

  x<-plyr::alply(c(variables_m,variables_y),1,function(y){
    varlags_vec(data_ymt_mat[,y],p=p,colname=y)})|>
  do.call(what=cbind)
  y=data_ymt_mat[-(1:p),]
  list(x=x,y=y)
  }



#'@examples
#'p=p_default
#'tar_load(data_ymt);
#'tar_load(variables_y);
#'tar_load(variables_m);
#'sigma_i_f(data_ymt,variables_m,variables_y,p)
#'# matlab gives prior.minnesota.sigma=  0.0517    0.4839    0.1970    3.4035    0.5266    0.0996    0.2351
#' #R    ff4_hf   sp500_hf        gs1   logsp500    us_rgdp  us_gdpdef     ebpnew 
# #      0.05214272 0.49491960 0.20017274 3.45821141 0.53511742 0.10122370 0.23883334 
#'#because they divide by n-1 and not by n-p also they replace missings by 0s...
#' #with n-1:
#'#ff4_hf   sp500_hf        gs1   logsp500    us_rgdp  us_gdpdef     ebpnew 
#'#0.05112355 0.48481814 0.19700358 3.40346054 0.52664537 0.09962111 0.23505210 


sigma_i_f<-
  function(data_ymt,variables_m,variables_y,p){
    data_ymt|>
      dplyr::select(one_of(variables_m,variables_y))|>
      plyr::llply(function(y){
        ym<-varlags_vec(y,p)
        #        lm(ym[,1]~ym[,-1])|>summary()|>(`[[`)("sigma")
        lm(ym[,1]~ym[,-1])$residuals|>sd()
        #lm(ym[,1]~ym[,-1])|>summary()|>(`[[`)("sigma")
      })|>unlist()|>
      (`[`)(c(variables_m,variables_y))}


s_bar_under_f<-function(sigma_i){diag(sigma_i^2)|>
    (`dimnames<-`)(list(names(sigma_i),names(sigma_i)))}

#'@examples
#'decay=decay_default
#'p=p_default
#'tightness=tightness_default
#'exog_std=exog_std_default
#'sigma_i=sigma_i_f(data_ymt,variables_m,variables_y,p=p)
#'qqinv_bar_under=qqinv_bar_under_f(data_ymt,variables_m,variables_y,p,decay,sigma_i,tightness,exog_std)
qqinv_bar_under_f<-
  function(data_ymt,variables_m,variables_y,p,decay,sigma_i,tightness,exog_std){
    variables_my<-c(variables_m,variables_y)
    sigma_i<-sigma_i[variables_my]
    N=length(variables_my)
    Ny=length(variables_y)
    temp1 <- kronecker((t(t(1:p)^(-decay))), matrix(1,N,N))# p^-d
    temp2 <- matlab::repmat(sigma_i,p*N,1)# sigma_i
    temp3 <- matlab::repmat(t(t(sigma_i^(-1))),p,N)# /sigma_j
    Q0 <- tightness*temp1*temp2*temp3
    Q0 <- rbind(Q0, rep(exog_std,N))# this assumes there is only constant term in w
    Q0 <- Q0^2
    colnames(Q0)<-variables_my
    Q0<-Q0[,variables_y] #<- []# drop the equations for m
    #K=nrow(Q0)
    list(q=diag(c(Q0)),qinv = diag(c(Q0)^(-1)))
  }

#'@examples
#'variables_y_non_st=variables_y_non_st_default
#'variables_y=variables_y_default
#'variables_m=variables_m_default
#'decay=decay_default
#'p=p_default
#'tightness=tightness_default
#'exog_std=exog_std_default
#'sigma_i=sigma_i_f(data_ymt,variables_m,variables_y,p=p)
#'qinv_bar_under=qinv_bar_under_f(data_ymt,variables_m,variables_y,p,decay,sigma_i,tightness,exog_std)
#'b_bar_under_f(variables_m,variables_y,p,variables_y_non_st,qinv_bar_under)

qinvb_b_bar_under_f<-function(variables_m,variables_y,p,variables_y_non_st,qinv_bar_under){
  variables_my<-c(variables_m,variables_y)
  N=length(variables_my)
  K=p*N+1
  B = matrix(0,K,N);
  mvector<-sapply(variables_my,is.element,variables_y_non_st_default)
  
  B[1:N,1:N] = diag(mvector)
  colnames(B)<-variables_my
  B<-B[,variables_y]
  list(b=B,qinvb=matrix(qinv_bar_under%*%c(B), nrow=K, ncol=length(variables_y)))
}
v_bar_under_f<-function(variables_m,variables_y){length(c(variables_m,variables_y))+2}
#'@examples
#'tar_load(data_ymt)
#'p=p_default
#'decay=decay_default
#'tightness=tightness_default
#'exog_std=exog_std_default
#'sigma_i=sigma_i_f(data_ymt,variables_m,variables_y,p=p)
#'qqinv_bar_under=qqinv_bar_under_f(data_ymt,variables_m,variables_y,p,decay,sigma_i,tightness,exog_std)
#'variables_y_non_st=variables_y_non_st,
#'p=p,tightness=tightness,decay=decay,exog_std=exog_std)
#'empirical_hyper=empirical_hyper_f(data_ymt,variables_m,variables_y,variables_y_non_st,p,tightness,decay,exog_std)
empirical_hyper_f<-
  function(
    data_ymt,
    variables_m,
    variables_y,
    variables_y_non_st,
    p,
    tightness,
    decay,
    exog_std){
    N=length(c(variables_m,variables_y))
    K=N*p+1
    sigma_i=  sigma_i_f(data_ymt=data_ymt,variables_m=variables_m,variables_y=variables_y,p=p)
    qqinv_bar_under=qqinv_bar_under_f(data_ymt=data_ymt,variables_y=variables_y,variables_m=variables_m,p=p,
                        decay=decay,sigma_i=sigma_i,tightness=tightness,
                        exog_std=exog_std)
    qinvb_b_bar_under=qinvb_b_bar_under_f(variables_m=variables_m,
                              variables_y=variables_y,
                              p=p,
                              variables_y_non_st=variables_y_non_st,
                              qinv_bar_under=qqinv_bar_under$qinv)
    v_bar_under=v_bar_under_f(variables_m,variables_y)
    s_bar_under<-s_bar_under_f(sigma_i)

    list(p=p,
         sigma_i=sigma_i,
         decay=decay,
         tightness=tightness,
         v_bar_under=v_bar_under,
         b_bar_under=qinvb_b_bar_under$b,
         qinvb_bar_under=qinvb_b_bar_under$qinvb,
         q_bar_under=qqinv_bar_under$q,
         qinv_bar_under=qqinv_bar_under$qinv,
         s_bar_under=s_bar_under,
         Nm=length(variables_m),
         Ny=length(variables_y),
         N=N,
         K=K)
}


