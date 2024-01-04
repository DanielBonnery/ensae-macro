checkdata<-function(.data, t2datestr, ignorecols=character(0)){
  # PURPOSE: Check data structure for missing data
  # INPUTS:
  # data.y - data matrix, T x N
  # data.time - time vector, T x 1
  # data.names - variable names, cell 1 x N
  # ignorecols - indexes of columns to ignore
  
  N = ncol(.data);
  
  
  ibegend = matrix(NA,N,3);
  for (n in 1:N){
    ifirst = min(which(is.na(.data[,n])));
    ilast = max(which(is.na(.data[,n])));
    if (ifirst==Inf){ifirst = 1}
    if (ilast==-Inf){ ilast = nrow(.data)}
    nmid = sum(is.na(.data[ifirst:ilast,n]));
    ibegend[n,] = c(ifirst,ilast,nmid);
    #fprintf(1,'#-18s #s-#s   #internal NaNs: #d\n', data.names{n}, t2datestr(data.time(ifirst)), t2datestr(data.time(ilast)), nmid);
  }
  
  #disp([char(data.names) repmat(' ',N,1) t2datestr(data.time(ibegend(:,1))) repmat(' - ',N,1) t2datestr(data.time(ibegend(:,2))) repmat('   #internal NaNs: ',N,1) num2str(ibegend(:,3))])
  # info.rnames = char([{' '} data.names]);
  # info.cnames = char({'beg',' ','end',' ','#NaN'});
  # info.fmt = char({'#4d','#2d','#4d','#2d','#3d'});
  # table = [t2ym(data.time(ibegend(:,1))) t2ym(data.time(ibegend(:,2))) ibegend(:,3)];
  # mprint(table,info)
  ibegend[ignorecols,]<-NA;
  ifirst = max(ibegend[,1],na.rm=TRUE);
  ilast = min(ibegend[,2],na.rm=TRUE);
  if (ifirst>1 | ilast<nrow(.data)){
    print(' ')
    print(paste0('Truncating sample from: ', t2datestr(data.time[1]), '-', t2datestr(data.time[length(time)])))
    print(paste0('         to new sample: ', t2datestr(data.time[ifirst]), '-', t2datestr(data.time[ilast])))
  }else{
    print(' ')
    print('No need to truncate the sample')}
  
  dataout =  .data[ifirst:ilast,];
  
  if (is.element('w',names(.data))){dataout$w = .data$w[ifirst:ilast,]}
  dataout
}