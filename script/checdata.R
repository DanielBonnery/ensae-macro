checkdata<-function(.data, t2datestr, ignorecols=character(0)){
  # PURPOSE: Check data structure for missing data
  # INPUTS:
  # data.y - data matrix, T x N
  # data.time - time vector, T x 1
  # data.names - variable names, cell 1 x N
  # ignorecols - indexes of columns to ignore
  
  ibegend=.data$y[!is.element(names(.data$y),ignorecols)]|>
  plyr::ldply(function(x){
    ifirst = min(which(!is.na(x)));
    ilast = max(which(!is.na(x)));
    if (ifirst==Inf){ifirst = 1}
    if (ilast==-Inf){ ilast = nrow(.data$y)}
    nmid = sum(is.na(x[ifirst:ilast]));
    c(ifirst=ifirst,ilast=ilast,nmid=nmid);
  })|>
    dplyr::filter(!is.element(.id,ignorecols))|>
    dplyr::summarize(
  ifirst = max(ifirst,na.rm=TRUE),
  ilast = min(ilast,na.rm=TRUE))
  if (ibegend$ifirst>1 | ibegend$ilast<nrow(.data$y)){
    print(' ')
    print(paste0('Truncating sample from: ', t2datestr(.data$time[1]), '-', t2datestr(.data$time[length(time)])))
    print(paste0('         to new sample: ', t2datestr(.data$time[ibegend$ifirst]), '-', t2datestr(.data$time[ilast])))
  }else{
    print(' ')
    print('No need to truncate the sample')}
  
  dataout = .data
  dataout$y=.data$y[ibegend$ifirst:ibegend$ilast,]
  dataout$time=.data$time[ibegend$ifirst:ibegend$ilast]
  dataout$w=.data$w[ibegend$ifirst:ibegend$ilast]
  dataout$.data=.data$.data[ibegend$ifirst:ibegend$ilast,]
  dataout
}