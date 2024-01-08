


proj<-function(Z,X){
  MASS::ginv((t(Z)%*%Z))%*%t(Z)%*%X
}

sample_mymis_0<-function(yy, dd, ZZ, WW, cc, TT, RR, a1, C1){
  # measure dimensions
  nobs=dim(yy)[1]
  TTT = dim(yy)[2]
  nobsshocks=dim(WW)[2];
  nstates=dim(RR)[1]
  nstatesshocks=dim(RR)[2]
  
  yplus = matrix(NA,nobs,TTT);
  aplus = matrix(NA,nstates,TTT+1);
  aplus[,1] = C1%*%rnorm(dim(C1)[2],1); # draw the first state with a1=0
  #'@examples 
  #'tt=1
  for (tt in 1:TTT){
    yplus[,tt] = ZZ%*%aplus[,tt] + WW%*%rnorm(nobsshocks,1);
    aplus[,tt+1] = TT%*%aplus[,tt] + RR%*%rnorm(nstatesshocks,1);}
  aplus<-aplus[,-nrow(aplus)];
  yyyy = yy - yplus;
  # allocate space
  vvv = matrix(NA,nobs,TTT); # one-step-ahead forecast error of yy
  FFFinv = array(NA,c(nobs,nobs,TTT)); # inverse of the variance of the one-step-ahead forecast error of yy
  KKK = array(NA,c(nstates,nobs,TTT)); # Kalman gain
  
  # Kalman filter on yyyy
  # compute frequently used matrices
  HH = WW%*%t(WW);
  RQR = RR%*%t(RR);
  # initialize Kalman filter
  at = a1; # at|I(t-1)
  Pt = C1%*%t(C1); # Pt|I(t-1)
  for (tt in 1:TTT){
    iobs = !is.na(yyyy[,tt]);
    nobst=sum(iobs)
    if(nobst==nobs){
      vt = yyyy[,tt] - dd - ZZ%*%at;
      Ftinv = proj((ZZ%*%Pt%*%t(ZZ) + HH),diag(nobs));
      Kt = TT%*%Pt%*%t(ZZ)%*%Ftinv;
      # update at,Pt; from now on their interpretation is "a(t+1),P(t+1)"
      at = cc + TT%*%at + Kt%*%vt;
      Pt = TT%*%Pt%*%t(TT - Kt%*%ZZ) + RQR;
      # store the quantities needed later for smoothing
      vvv[,tt] = vt;
      FFFinv[,,tt] = Ftinv;
      KKK[,,tt] = Kt;}
    if (nobst!=nobs&nobst>0){
      ZZt = ZZ[iobs,];
      HHt = HH[iobs,iobs];
      vt = yyyy[iobs,tt] - dd[iobs] - ZZt%*%at;
      Ftinv = proj((ZZt%*%Pt%*%t(ZZt) + HHt),diag(nobst));
      Kt = TT%*%Pt%*%t(ZZt)%*%Ftinv;
      # update at,Pt; from now on their interpretation is "a(t+1),P(t+1)"
      at = cc + TT%*%at + Kt%*%vt;
      Pt = TT%*%Pt%*%t(TT - Kt%*%ZZt) + RQR;
      # store the quantities needed later for smooting
      vvv[iobs,tt] = vt;
      FFFinv[iobs,iobs,tt] = Ftinv;
      KKK[,iobs,tt] = Kt;
      if(nobst==0&nobst!=nobs){
        # update at,Pt; from now on their interpretation is "a(t+1),P(t+1)"
        at = cc + TT%*%at;
        Pt = TT%*%Pt%*%t(TT) + RQR;}
    }}  
  # Kalman smoother
  # backwards recursion on r
  rrr = matrix(0,nstates,TTT);
  for (tt in (TTT-1):1){
    iobs = !is.na(yyyy[,tt+1]);
    nobst=sum(iobs)
    
    if (nobst==nobs){
      rrr[,tt] = t(ZZ)%*%FFFinv[,,tt+1]%*%vvv[,tt+1] + t(TT - KKK[,,tt+1]%*%ZZ)%*%rrr[,tt+1];}
    if (nobst!=nobs&nobst>0){
      Ftp1inv = FFFinv[iobs,iobs,tt+1];
      ZZtp1 = ZZ[iobs,];
      vtp1 = vvv[iobs,tt+1];
      Ktp1 = KKK[,iobs,tt+1];
      rrr[,tt] = t(ZZtp1)%*%Ftp1inv%*%vtp1 + t(TT - Ktp1%*%ZZtp1)%*%rrr[,tt+1];}
    if (nobst!=nobs&nobst==0){
      rrr[,tt] = t(TT)%*%rrr[,tt+1]; # backward recursion with no observables
    }}
  # one more iteration to get r0
  iobs = !is.na(yyyy[,1]);
  r0 = t(ZZ[iobs,])%*%FFFinv[iobs,iobs,1]%*%vvv[iobs,1] + t(TT - KKK[,iobs,1]%*%ZZ[iobs,])%*%rrr[,1];
  # allocate space for smoothed states
  aaa = matrix(NA,nstates,TTT);
  # forwards recursion to compute smoothed states from r - DK(2002),eq.8
  aaa[,1] = a1 + C1%*%t(C1)%*%r0; # initialize the forward recursion
  for (tt in 2:TTT){
    aaa[,tt] = cc + TT%*%aaa[,tt-1] + RQR%*%(rrr[,tt-1]);
  }
  
  aaa = aaa + aplus;
  
  t(aaa[1:N,])
}
  


#'@examples
#'tar_load(data_ymt)
#'tar_load(variables_m)
#'tar_load(variables_y)
#'tar_load(empirical_hyper)
#'bsigma=mcmc_initial_values_f(data_ymt,variables_m,variables_y,empirical_hyper)
#'b=bsigma$b
#'sigma=bsigma$sigma
#'mcmc_observations<-mcmc_observations_f(data_ymt,p,variables_m,variables_y)
#'sample_mymis(sigma,b,mcmc_observations,empirical_hyper)

sample_mymis<-function(sigma,b,mcmc_observations,empirical_hyper){
  mcmc_observations|>attach()|>suppressMessages()
  empirical_hyper|>attach()|>suppressMessages()
  rep(0,nrow(missingpositions))
  yy = t(data_ym_mat[(p+1):nrow(data_ym_mat),]);

  dd = matrix(0,N,1);
  ZZ = matrix(0,N,N*p); ZZ[1:N,1:N] = diag(N);
  WW = (.2*1e-15)*diag(N);
  
  cc = matrix(0,N*p,1); 
  cc[1:N,1] = t(b[nrow(b),]);
  TT = rbind(t(b[1:(nrow(b)-1),]),
             cbind(diag(N*(p-1)),
                   matrix(0,N*(p-1),N)));
  RR = rbind(t(chol(sigma)),
           matrix(0,N*(p-1),N));
  
  C1 = RR;
  a0 = c(t(data_ym_mat[p:1,]));
  a1 = cc + TT%*%a0;
  
  sample_mymis_0(yy, dd, ZZ, WW, cc, TT, RR, a1, C1)
  }
  
  
  
  
  

