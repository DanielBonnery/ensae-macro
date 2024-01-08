#'@examples
#'variables_m=variables_m_default
#'variables_y=variables_y_default
#'i=1
#'mcmc_chain=get_mcmc(variables_m = variables_m,variables_y = variables_y)
#'N=7
#'p=12
#'B=mcmc_chain$b_sample[1:(N*p),,i]|>t()|>array(c(N,N,p))
#'cholsigma=chol(mcmc_chain$sigma_sample[,,i])
#'nstep=30

impulsdtrf <- function(B,cholsigma,nstep){
  #[neq,nvar,nlag] <- size(B)
  nlag<-dim(B)[3]
  response <- array(0,c(dim(B)[1:2],nstep))
  response[,,1] <- t(cholsigma)
  for (it in 2:nstep){
    for (ilag in 1:min(nlag,it-1)){
      response[,,it] <- response[,,it]+B[,,ilag]%*%response[,,it-ilag]
    }
  }
  response
}



test_restr<-function(i_mon=1,i_news=2,irfs){
  irfs[1,i_mon,1] > 0 & irfs[2,i_mon,1] < 0 & irfs[1,i_news,1] > 0 && irfs[2,inews,1] > 0;}




#'@examples
#'variables_m=variables_m_default
#'variables_y=variables_y_default
#'mcmc_chain=get_mcmc(variables_m = variables_m,variables_y = variables_y)
#'nstep=36
#'max_attempts=1000

draw_irfs<-function(mcmc_chain,variables_m,variables_y,p,nstep,max_attempts){
  dd<- dim(mcmc_chain$b_sample)
  N=dd[2]
  ndraws=dd[3]     
  
  i_mon=grep( "ff4_hf",c(variables_m,variables_y))
  i_news=grep( "sp500_hf",c(variables_m,variables_y))
  #'@examples
  #'i=1
  irfs_sample<-plyr::aaply(
    1:ndraws,
    .margins = 1,
    .progress="text",
    .fun=function(i){
      irfs<-array(NA,c(N,N,nstep))
      
      irfchol <- impulsdtrf(B=(mcmc_chain$b_sample[,,i][1:(N*p),])|>t()|>array(c(N,N,p)),
                            cholsigma=chol(mcmc_chain$sigma_sample[,,i]),
                            nstep=nstep)
      
      # apply the sign restrictions
      notfound=TRUE;a=1
      while (notfound &a<max_attempts){
        # create an orthogonal matrix
        Q <- diag(N)
        qr_rand <- qr(matrix(rnorm(4),2,2))# draw orthogonal QQ
        Q[1:2,1:2]<- qr_rand$qr
        
        toflip <- diag(irfchol[,,1]%*%Q)<0
        Q[,toflip] <- -Q[,toflip]
        
        candidate <- einsum::einsum("ijk,jl->ilk",irfchol,Q)
        if (candidate[1,i_mon,1] > 0 & 
            candidate[2,i_mon,1] < 0 & 
            candidate[1,i_news,1] > 0 &
            candidate[2,i_news,1] > 0){
          irfs <- candidate
          notfound=FALSE}
        a=a+1
      }
      irfs
    })|>aperm(c(2:4,1))
  
  variables_my=c(variables_m,variables_y)
  dimnames(irfs_samples)<-list(on=variables_my,
                             shock_of=variables_my,
                             time=0:(dim(irfs_samples)-1)[3],NULL)
  names(dimnames(irfs_samples))[1:3]<-c("on","shock_of","time")
  irfs_samples
  }