 <- function(y,info){
# PURPOSE: print an (nobs x nvar) matrix in formatted form
#---------------------------------------------------
# USAGE:     mprint(x,info)
# where: x         = (nobs x nvar) matrix (or vector) to be printed
#        info      = a structure containing printing options
#        info.begr = beginning row to print,    (default = 1)
#        info.endr = ending row to print,       (default = nobs)
#        info.begc = beginning column to print, (default = 1
#        info.endc = ending column to print,    (default = nvar)
#        info.cnames = an (nvar x 1) string vector of names for columns (optional)
#                      e.g. info.cnames = strvcat('col1','col2');
#                      (default = no column headings)
#        info.rnames = an (nobs+1 x 1) string vector of names for rows (optional)
#                      e.g. info.rnames = strvcat('Rows','row1','row2');
#                      (default = no row labels)
#        info.fmt    = a format string, e.g., '#12.6f' or '#12d' (default = #10.4f)
#                      or an (nvar x 1) string containing formats
#                      e.g., info.fmt=strvcat('#12.6f','#12.2f','#12d'); for nvar = 3
#        info.fid    = file-id for printing results to a file
#                      (defaults to the MATLAB command window)
#                      e.g. fid = fopen('file.out','w');
#        info.rflag  = 1 for row #'s printed, 0 for no row #'s (default = 0)
#        info.width  = # of columns before wrapping occurs (default = 80)
#---------------------------------------------------
# e.g.   in.cnames = strvcat('col1','col2');
#        in.rnames = strvcat('rowlabel','row1','row2');
#        mprint(y,in), prints entire matrix, column and row headings
#        in2.endc = 3; in2.cnames = strvcat('col1','col2','col3');
#    or: mprint(y,in2), prints 3 columns of the matrix, just column headings
#    or: mprint(y), prints entire matrix, no column headings or row labels
# NOTES: - defaults are used for info-elements not specified
#        - default wrapping occurs at 80 columns, which varies depending on the
#          format you use, e.g. #10.2f will wrap after 8 columns
#---------------------------------------------------
# SEE ALSO: tsprint, mprint_d, lprint
#---------------------------------------------------

# written by:
# James P. LeSage, Dept of Economics
# University of Toledo
# 2801 W. Bancroft St,
# Toledo, OH 43606
# jpl@jpl.econ.utoledo.edu

# setup defaults
fid <- 1; rflag <- 0; cflag <- 0; rnum <- 0; nfmts <- 1; cwidth <- 80
[nobs nvars] <- size(y)
begr <- 1; }r <- nobs; begc <- 1; }c <- nvars; fmt <- '#10.4f';
if (nargin == 1){
# rely on defaults
} else if (nargin == 2){
  if (!isstruct(info)){
    error('mprint: you must supply the options as a structure variable')
  }
fields <- fieldnames(info)
nf <- length(fields)
for (i in 1:nf){
    if (strcmp(fields{i},'fmt')){
        fmts <- info.fmt
  [nfmts junk] <- size(fmts)
  if (nfmts == nvars){
   fmt <- fmts
  } else if (nfmts == 1){
   fmt <- fmts
  } else {
   error('mprint: wrong # of formats in string -- need nvar')){
  }
    } else if (strcmp(fields{i},'fid')){
        fid <- info.fid
    } else if (strcmp(fields{i},'begc')){
        begc <- info.begc
    } else if (strcmp(fields{i},'begr')){
        begr <- info.begr
    } else if (strcmp(fields{i},'}c')){
        }c <- info.}c
    } else if (strcmp(fields{i},'}r')){
        }r <- info.}r
    } else if (strcmp(fields{i},'width')){
      cwidth <- info.width
    } else if (strcmp(fields{i},'cnames')){
        cnames <- info.cnames
        cflag <- 1
    } else if (strcmp(fields{i},'rnames')){
        rnames <- info.rnames
        rflag <- 1
    } else if (strcmp(fields{i},'rflag')){
        rnum <- info.rflag
    }
}

} else {
error('Wrong # of arguments to mprint')

}# end of if-elseif input checking


# see if the user supplied row names and set rnum
# correct her mistake if she did this
if (rflag == 1){
rnum <- 0
}

# parse formats
if (nfmts == 1){
   f1 <- strtok(fmt,'%')
   f2 <- strtok(f1,'.')
    if (strcmp(f1,f2)){
     f2 <- strtok(f2,'d')
     dflag <- 1
     fflag <- 0
    } else {
     tmp1 <- strtok(fmt,'f')
     tmp2 <- strtok(tmp1,'.')
     tmp1 <- tmp1(2:length(tmp1))
     tmp2 <- tmp2(2:length(tmp2))
     opoint <- num2str(str2num(tmp1) - str2num(tmp2))
     decimal <- opoint(1,length(opoint))
     f2 <- strtok(f2,'f')
     fflag <- 1
     dflag <- 0
    }
   f2 <- str2num(f2)
   nwide <- floor(cwidth/f2)# 80 columns divided by format
   nvar <- }c-begc+1
   nsets <- ceil(nvar/nwide)
} else {#  wrapping in this case is based on widest format in the list
nwidev <- zeros(nfmts,1)
nsetsv <- zeros(nfmts,1)
f2v <- zeros(nfmts,1)
dflagv <- zeros(nfmts,1)
fflagv <- zeros(nfmts,1)
decimalv <- zeros(nfmts,1)
   for (ii in 1:nfmts){
   f1 <- strtok(fmt(ii,:),'%')
   f2 <- strtok(f1,'.')
    if (strcmp(f1,f2)){
     f2 <- strtok(f2,'d')
     dflagv(ii,1) <- 1
     fflagv(ii,1) <- 0
    } else {
     tmp1 <- strtok(fmt(ii,:),'f')
     tmp2 <- strtok(tmp1,'.')
     tmp1 <- tmp1(2:length(tmp1))
     tmp2 <- tmp2(2:length(tmp2))
     opoint <- num2str(str2num(tmp1) - str2num(tmp2))
     decimalv(ii,1) <- opoint(1,length(opoint))
     f2 <- strtok(f2,'f')
     fflagv(ii,1) <- 1
     dflagv(ii,1) <- 0
    }
   f2v(ii,1) <- str2num(f2)
   nwidev(ii,1) <- floor(cwidth/f2v(ii,1))# cwidth columns divided by format
   nvar <- }c-begc+1
   nsetsv(ii,1) <- ceil(nvar/nwidev(ii,1))
}
nsets <- min(nsetsv)
nwide <- max(nwidev)
}

# if we have row and column labels
# adjust variable labels and column heading strings
# to match the width of the printing format

if (rnum == 1){
dstr <- 'Obs#'
}

if (cflag == 1){# we have column headings
 [vsize nsize] <- size(cnames)# error check cnames argument
 if (vsize != nvars; error('Wrong # cnames in mprint'); }){
 if (nfmts == 1){# case of only 1 format string
  nmax <- max(f2,nsize)# build format strings
                        # based on widest format
  sfmt <- ['%', num2str(nmax)]
  sfmt <- [sfmt,'s ']
  ffmt <- ['%', num2str(nmax)]
   if (dflag == 1){
   ffmt <- [ffmt,'d ']
   } else if (fflag == 1){
   ffmt <- [ffmt,'.']
   ffmt <- [ffmt,decimal]
   ffmt <- [ffmt,'f ']
   }
 } else {# we have multiple format strings, process each
 sfmtv <- []; fmtv <- []
  for (ii in 1:nfmts){# find and parse multiple formats
  nmax <- max(f2v(ii,:),nsize)# build format strings
                        # based on widest format
  sfmtv{ii} <- ['%', num2str(nmax)]
  sfmtv{ii} <- [sfmtv{ii},'s ']
  ffmtv{ii} <- ['%', num2str(nmax)]
   if (dflagv(ii,1) == 1){
   ffmtv{ii} <- [ffmtv{ii},'d ']
   } else if (fflagv(ii,1) == 1){
   ffmtv{ii} <- [ffmtv{ii},'.']
   ffmtv{ii} <- [ffmtv{ii},decimalv(ii,1)]
   ffmtv{ii} <- [ffmtv{ii},'f ']
   }
  }# end of for ii loop
 }# end of if-else
} else if (cflag == 0){# we have no column headings
 if (nfmts == 1){# case of only 1 format string
  nmax <- f2# augment format string with a space (the hard way)
  ffmt <- ['%', num2str(nmax)]
   if (dflag == 1){
   ffmt <- [ffmt,'d ']
   } else if (fflag == 1){
   ffmt <- [ffmt,'.']
   ffmt <- [ffmt,decimal]
   ffmt <- [ffmt,'f ']
   }
 } else {# we have multiple format strings, process each
 sfmtv <- []; fmtv <- []
  for (ii in 1:nfmts){# find and parse multiple formats
  nmax <- f2v(ii,:)# augment format strings with a space
  ffmtv{ii} <- ['%', num2str(nmax)]
   if (dflagv(ii,1) == 1){
   ffmtv{ii} <- [ffmtv{ii},'d ']
   } else if (fflagv(ii,1) == 1){
   ffmtv{ii} <- [ffmtv{ii},'.']
   ffmtv{ii} <- [ffmtv{ii},decimalv(ii,1)]
   ffmtv{ii} <- [ffmtv{ii},'f ']
   }
  }# end of for ii loop
 }# end of if-else
}# end of if-elseif cflag == 0,1

if (rflag == 1){# we have row labels
 [vsize nsize] <- size(rnames)# error check cnames argument
 if (vsize != nobs+1; error('Wrong # rnames in mprint'); }){
 rfmt <- ['%', num2str(nsize)]
 rfmt <- [rfmt,'s ']
}# end of if rflag == 1

if (rflag == 0 & cflag == 0){
    ffmt <- fmt
}

# print matrix
for (j in 1:nsets){
 if (nfmts == 1){# print row header and column headers
 if (rnum == 1;fprintf(fid,'){#5s ',dstr);
     } else if (rflag == 1){
  fprintf(fid,rfmt,rnames(1,:))
     }
     if (cflag == 1){
    for (i in (j-1)*nwide+begc:j*nwide+begc-1){
  if (i <= }c){
# find version #;
  #[version,junk] = version; vers = str2num(version);
   #if vers == 5.2
   fprintf(fid,sfmt,strjust(cnames(i,:),'right'))
   #else
   #fprintf(fid,sfmt,strjust(cnames(i,:)));
   #end;
  }
 }
     }
  fprintf(fid,'\n')
 } else {# we have multiple formats
 if (rnum == 1;fprintf(fid,'){#5s ',dstr);
    } else if (rflag == 1){
 fprintf(fid,rfmt,rnames(1,:))
    }
    if (cflag == 1){
   for (i in (j-1)*nwide+begc:j*nwide+begc-1){
  if (i <= }c){
# find version #;
  #[version,junk] = version; vers = str2num(version);
   #if vers == 5.2
   fprintf(fid,sfmtv{i},strjust(cnames(i,:),'right'))
   #else
   #fprintf(fid,sfmtv{i},strjust(cnames(i,:)));
   #end;
  }
   }
    }
 fprintf(fid,'\n')
 }# end of if-else nfmts
 for (k in begr:}r){# print row labels and numbers in matrix
  if (rnum == 1; fprintf(fid,'){#5d ',k);
        } else if (rflag == 1){
  fprintf(fid,rfmt,rnames(k+1,:))
        }
  for (l in (j-1)*nwide+begc:j*nwide+begc-1){
   if (l <= }c){
    if (nfmts == 1){
    fprintf(fid,ffmt,y(k,l))
    } else {
    fprintf(fid,ffmtv{l},y(k,l))
    }
   }
  }# end of for l
  fprintf(fid,'\n')
 }# end of for k
fprintf(fid,'\n')
}# end of for j
