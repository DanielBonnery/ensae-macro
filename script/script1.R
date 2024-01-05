rm(list=ls())

library(ggplot2)


c("script/checdata.R",
  "m2r/plot_y.r")|>sapply(source)->noprint
spl = cbind(c(1984,2016),c(2,12));


#spl = cbind(c(1984,2008),c(2,12));# Dec2008 ZLB reached
#spl = cbind(c(1990,2016),c(2,12));# Feb1999 surprises start
#spl = cbind(c(1979,2016),c(7,12));# GertlerKaradi2015 sample 


modname = 'us1'; #'us1','ea1','us2','ea2'
addvar = ''; #'exp_gdp_12m','exp_cpi_12m','bkeven05','gs10','sven5f5'

# IDENTIFICATION: idscheme, mnames
idscheme = 'sgnm2'; #'chol','sgnm2','sgnm2strg','supdem'

mnames = c('ff4_hf','sp500_hf'); # US baseline
#mnames = {'ff4_hf'};
#mnames = {'pmnegm_ff4sp500','pmposm_ff4sp500'}; # poor man's sign restrictions
#mnames = {'ff4_hf','sp500_hf','dbkeven02_d'}; # for supdem identification
#mnames = {'pc1ff1_hf','usstocks1_hf'}; # VAR with factors (Online Appendix C.4)
#mnames = {'pmnegm_pc1ff1usstocks1','pmposm_pc1ff1usstocks1'}; # VAR with factors, poor man's shocks (Online Appendix C.4)

#mnames = {'eureon3m_hf','stoxx50_hf'}; # euro area baseline
#mnames = {'eureon3m_hf'};
#mnames = {'pmnegm_eureon3mstoxx50','pmposm_eureon3mstoxx50'}; # poor man's sign restrictions
#mnames = {'eureon3m_hf','stoxx50_hf','deurinflswap2y_d'}; # for supdem identification

###########################################################################

# PRIOR
prior.lags = 12;
prior.minnesota.tightness = .2;
prior.minnesota.decay = 1;
prior.Nm = length(mnames);

# create the output folder based on the calling filename
path_out="path_out";path_out|>dir.create()

# functions for time operations
ym2t = function(x){ x[1]+x[2]/12 - 1/24}; # convert [year month] into time
#'functionexamples 
#'t2datestr(2005.25)
t2datestr = function(t){c(paste0(floor(t),'m', round(12*(t-floor(t)+1/24))))};
#'functionexamples 
#'t2ym(2005.25)
t2ym = function(t){paste0(floor(t), round(12*(t-floor(t)+1/24)))}; # convert time into [year month]
ymdif = function(x1,x2){(x2[1]-x1[1])*12+x2[2]-x1[2]};
findym = function(x,t){which(abs(t-ym2t(x))<1e-6)}; # find [year month] in time vector t

# Gibbs sampler settings
gssettings.ndraws = 4000;
gssettings.burnin = 4000;
gssettings.saveevery = 4;
gssettings.computemarglik = 0;

# detect poorman shocks and if yes override the identification to choleski
if (!any(grepl(pattern = 'neg',mnames[1])) &!any(grepl(pattern = 'pos',mnames[2]))){
    idscheme = 'chol'}

# nice names
mdict = c('eureon3m_'= 'surprise in 3m Eonia swaps',
              'stoxx50_'= 'surprise in  Euro Stoxx 50',
              'ff4_'= 'surprise in 3m ff futures',
              'sp500_'= 'surprise in S&P500',
              'pc1ff1_'= 'surprise in policy ind.',
              'usstocks1_'= 'surprise in 1pc of stocks');
mnames_nice = mdict[mnames];
mylimits = matrix(NA,nrow=length(mnames),ncol=2);

# define y
ny1 = 5;
ynames=
switch(modname,
       'us1'=c('gs1','logsp500','us_rgdp','us_gdpdef','ebpnew'),
       'us2'=c('gs1','logsp500','us_ip','us_cpi','ebpnew'),
       'ea1'=c('de1y_haver','stoxx50','ea_rgdp','ea_gdpdef','ea_bbb_oas_all_fred'),
       'ea2'=c('de1y_haver','stoxx50','ea_ip_excl_constr','hicp','ea_bbb_oas_all_fred'),
       stop(paste0(modname,':unknown modname')));

if (addvar!=''&!is.element(addvar,ynames)){
        ynames = c(ynames ,addvar); modname = paste0(modname,'_', addvar)}
# nice_names, yylimits, nonst
dictfname = 'inst/extdata/ydict.csv';
ydict<-read.csv(dictfname)
ydict|>(`rownames<-`)(ydict[,1])

ynames_nice = ydict[ynames,2];#ynames_nice = ynames;
yylimits = ydict[ynames,4:5];
nonst = ydict[ynames,3];

# load data
datafname = 'inst/extdata/data.csv';
data.Nm = length(mnames);
data.names = c(mnames, ynames);
.data<-dat <- d <- read.csv(datafname); 
tbeg = which(dat[[1]]==spl[1,1] & dat[[2]]==spl[1,2]); 
if (length(tbeg)==0){ tbeg=1}
tend = which(dat[[1]]==spl[2,1] & dat[[2]]==spl[2,2]); 
if (length(tend)==0){ tend=1}
ysel = intersect(data.names, names(d));
.data = dat[tbeg:tend, ysel];
.data$w = rep(1,nrow(.data));
.data$time = seq(ym2t(dat[tbeg,1:2])[[1]], 
                ym2t(dat[tend,1:2])[[1]], 
                length.out=nrow(.data))
rm( d,  tbeg, tend, ysel)

# check ydata for missing values, determine the sample




datatemp = .data
#checkdata(.data, t2datestr, 1:data.Nm);
idspl = paste0(t2datestr(datatemp$time[1]), 
               '-', 
               t2datestr(datatemp$time[length(datatemp$time)]));

# output file names
fname = file.path(path_out,paste0(modname, '_', paste0(mnames,sep='_'), '_', idspl, '_', idscheme));
#diary([fname '.txt'])
#data = checkdata(data, t2datestr, 1:data.Nm); # again, for the diary
plot_y(.data,whichplot = c(TRUE,TRUE))->plots;
plots[[1]]
plots[[2]]

# print the correlation matrix of m
.table = var(.data,use = "pairwise.complete.obs");
corrtable=.table/(sqrt(c(diag(.table))%*%t(c(diag(.table)))))
corrtable

# complete the minnesota prior
prior.minnesota.mvector = c(rep(0,data.Nm), nonst);

# replace NaNs with zeros in the initial condition
temp = .data[1:prior.lags,,]; 
temp[is.an(temp)] <- 0; 
.data[1:prior.lags,] <- temp; 

# drop the shocks before February 1994
#id = data.time<ym2t([1994 2])-1e-6; .data(id,1:data.Nm) = NaN;

# estimate the VAR
#.data(isnan(.data)) = 0; res = VAR_dummyobsprior(.data,data.w,gssettings.ndraws,prior);
res = VAR_withiid1kf(data, prior, gssettings);

savedata([fname '_data.csv'], data, t2ym)
## identification
MAlags = 36;
N = length(data.names);

switch idscheme
case 'chol'
shocknames = data.names;
irfs_draws = NaN(N,N,MAlags,gssettings.ndraws);
for i = 1:gssettings.ndraws
betadraw = res.beta_draws(1:end-size(data.w,2),:,i);
sigmadraw = res.sigma_draws(:,:,i);
response = impulsdtrf(reshape(betadraw',N,N,prior.lags), chol(sigmadraw), MAlags);
            irfs_draws(:,:,:,i) = response;
        end
        ss = 1;
        if length(mnames)>1 && ((~isempty(strfind(mnames{1},'neg')) && ~isempty(strfind(mnames{2},'pos'))) || ~isempty(strfind(mnames{2},'_signrestr'))), ss = 1:2; end
    case 'sgnm2' # baseline two sign restrictions
        shocknames = [{'mon.pol.', 'CBinfo'} mnames(2+1:end) ynames];
        dims = {[1 2]};
        imonpol = 1; inews = 2;
        test_restr = function(irfs)... ## restrictions by shock (i.e. by column):
            irfs(1,imonpol,1) > 0 && irfs(2,imonpol,1) < 0 &&... # mp
            irfs(1,inews,1) > 0 && irfs(2,inews,1) > 0; # cbi
        b_normalize = ones(1,N);
        max_try = 1000;
        disp(test_restr)
        irfs_draws = resirfssign(res, MAlags, dims, test_restr, b_normalize, max_try);
        #[irfs_draws, irfs_l_draws, irfs_u_draws] = resirfssign_robust(res, MAlags, dims, test_restr, b_normalize, max_try);
        ss = 1:2;
    case 'sgnm2strg' # strong instrument restriction
        # imposes that the THIRD variable goes up after mp shock
        shocknames = [{'mon.pol.', 'CBinfo'} ynames];
        dims = {[1 2]};
        iyld = 3;
        imonpol = 1; inews = 2;
        test_restr = function(irfs)... ## restrictions by shock (i.e. by column):
            irfs(1,imonpol,1) > 0 && irfs(2,imonpol,1) < 0 && irfs(iyld,imonpol,1)>0.01 &&... # mp
            irfs(1,inews,1) > 0 && irfs(2,inews,1) > 0; # cbi
        b_normalize = ones(1,N);
        max_try = 1000;
        disp(test_restr)
        irfs_draws = resirfssign(res, MAlags, dims, test_restr, b_normalize, max_try);
        ss = 1:2;
   case 'supdem' # disentangle CB info about supply and demand
        # requires: 1. interest rate; 2. stock price; 3. break-even inflation
        # CBinfosup shock moves stock price down but break-even inflation up
        shocknames = [{'mon.pol.', 'CBinfodem', 'CBinfosup'} mnames(3+1:end) ynames];
        dims = {[1 2 3]};
        imonpol = 1; inews = 2; isup = 3;
        test_restr = function(irfs)... ## restrictions by shock (i.e. by column):
            irfs(1,imonpol,1) > 0 && irfs(2,imonpol,1) < 0 && irfs(3,imonpol,1) < 0 &&... # mp
            irfs(1,inews,1) > 0 && irfs(2,inews,1) > 0 && irfs(3,inews,1) > 0 &&... # info demand
            irfs(1,isup,1) > -100 && irfs(2,isup,1) > 0 && irfs(3,isup,1) < 0; # info supply
        b_normalize = ones(N,1); b_normalize(3) = -1;
        max_try = 5000;
        disp(test_restr)
        irfs_draws = resirfssign(res, MAlags, dims, test_restr, b_normalize, max_try);
        ss = 1:3;
end

## reporting

# report variance decompositon
vdec_mean = table_vdecomp(irfs_draws, 1:N, ss, data.names, shocknames, 24);

# report the irfs:
qtoplot = [0.5 0.16 0.84 0.05 0.95]; # quantiles to plot
varnames = [mnames, ynames]; varnames_nice = [mnames_nice ynames_nice]; shocknames_nice = shocknames;
ylimits = [];
#ylimits = [mylimits; yylimits];
transf = nan(N,2);

# print out the impact responses
table_irf(irfs_draws, ss, 1, varnames, qtoplot(1:3));
table_irf(irfs_draws, ss, 1, varnames, qtoplot([1 4 5]));

# plot the irfs
hh = plot_irfs_draws(irfs_draws, data.Nm+1:min(N,data.Nm+ny1), ss, varnames_nice, varnames, shocknames_nice, idscheme, qtoplot, [0 0 1], '', ylimits, transf); align_Ylabels(hh); saveTightFigure(hh,[fname '_irfy1'],'pdf')
#hh = plot_irfs_draws(irfs_draws, data.Nm+ny1+1:min(N,data.Nm+2*ny1), ss, varnames_nice, varnames, shocknames_nice, idscheme, qtoplot, [0 0 1], '', ylimits); align_Ylabels(hh); saveTightFigure(hh,[fname '_irfy2'],'pdf')
#hh = plot_irfs_draws(irfs_draws, 1:N, ss, varnames_nice, varnames, shocknames_nice, idscheme, qtoplot, [0 0 1], '', ylimits); align_Ylabels(hh); saveTightFigure(hh,[fname '_irfmy'],'pdf')

if 0 # save the ylimits
    hh = plot_irfs_draws(irfs_draws, 1:N, ss, varnames_nice, varnames, shocknames_nice, idscheme, qtoplot, [0 0 1], '', ylimits, transf); align_Ylabels(hh);
    ylimits = cell2mat(get(hh.Children,'Ylim')); ylimits = ylimits(1:max(ss):N*max(ss),:); ylimits = flipud(ylimits);
    save([fname '_ylimits.mat'],'ylimits');
end

if ~isempty(addvar)
    ylimits = [mylimits; yylimits];
    varstoplot = findstrings(addvar,varnames);
    hh = plot_irfs_draws(irfs_draws, varstoplot, ss, varnames_nice, varnames, shocknames_nice, idscheme, qtoplot, [0 0 1], '', ylimits);
    saveTightFigure(hh,[fname '_addvar'],'pdf')
end

if exist('irfs_l_draws','var')
credibility = [0.68 0.9];
hh = plot_irfs_draws_robust(irfs_draws, irfs_l_draws, irfs_u_draws, data.Nm+1:min(N,data.Nm+ny1), ss, varnames_nice, shocknames_nice, credibility, [0 0 1]); align_Ylabels(hh); saveTightFigure(hh,[fname '_rirfy1'],'pdf')
hh = plot_irfs_draws_robust(irfs_draws, irfs_l_draws, irfs_u_draws, data.Nm+ny1+1:N, ss, varnames_nice, shocknames_nice, credibility, [0 0 1]); align_Ylabels(hh); saveTightFigure(hh,[fname '_rirfy2'],'pdf')
#hh = plot_irfs_draws_robust(irfs_draws, irfs_l_draws, irfs_u_draws, 5:6, ss, varnames_nice, shocknames_nice, idscheme, credibility, [1 0 1]); align_Ylabels(hh); saveTightFigure(hh,[fname '_rirfyipcpi'],'pdf')
end

diary off

