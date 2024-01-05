checkdata<-function(.data, t2datestr, ignorecols=character(0)){
  # PURPOSE: Check data structure for missing data
  # INPUTS:
  # data.y - data matrix, T x N
  # data.time - time vector, T x 1
  # data.names - variable names, cell 1 x N
  # ignorecols - indexes of columns to ignore
  
  N = ncol(.data);
  
  plyr::ldply(.data,function(x){
    ifirst = min(which(is.na(x)));
    ilast = max(which(is.na(x)));
    if (ifirst==Inf){ifirst = 1}
    if (ilast==-Inf){ ilast = nrow(.data)}
    nmid = sum(is.na(x[ifirst:ilast]));
    c(ifirst=ifirst,ilast=ilast,nmid=nmid);
  })|>
    dplyr::filter(!is.element(.id,ignorecols))|>
    dplyr::summarize(
  ifirst = max(ifirst,na.rm=TRUE),
  ilast = min(ilast,na.rm=TRUE))
  if (ifirst>1 | ilast<nrow(.data)){
    print(' ')
    print(paste0('Truncating sample from: ', t2datestr(data.time[1]), '-', t2datestr(data.time[length(time)])))
    print(paste0('         to new sample: ', t2datestr(data.time[ifirst]), '-', t2datestr(data.time[ilast])))
  }else{
    print(' ')
    print('No need to truncate the sample')}
  
  dataout =  .data[ifirst:ilast,];
}