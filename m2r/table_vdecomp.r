table_vdecomp <- function(irfs_draws, vars, shocks, varnames, shocknames, HH){
# vdec_mean = table_vdecomp(irfs_draws,varnames,shocks,HH)
# PURPOSE: displays variance decomposition
#
# SUBFUNCTIONS: vdecomp
# DEPENDS: mprint
# Marek Jarocinski

disp('Variance decomposition')
[N,N2,nstep,n_draws] <- size(irfs_draws)

vdec_draws <- zeros(N,N,nstep,n_draws)
for (draw in 1:n_draws){
    vdec_draws(:,:,:,draw) <- vdecomp(irfs_draws(:,:,:,draw))
}

vdec_mean <- nanmean(vdec_draws,4)
vdec_l <- quantile(vdec_draws,0.05,4)
vdec_u <- quantile(vdec_draws,0.95,4)


# printout vdec_mean using mprint
for (n in 1:N){
    vdecn <- permute(vdec_mean(n,shocks,HH), [2 3 1])
    # printout
    disp(['variable: ' varnames{n}])
    info.cnames <- num2str(HH')
    info.fmt in '%8.3f'; for (i in 2:length(HH), info.fmt in strvcat(info.fmt,'%8.3f'); }){
    info.rnames <- strvcat('shock  \  h <- ',shocknames{shocks},'total')
    mprint([vdecn; sum(vdecn,1)],info)
}

# another printout
Ns <- length(shocks)
Nh <- length(HH)
info <- struct
temp <- {}
for (s in 1:Ns){
    temp <- [temp; strseq(shocknames{s},HH)]
}
info.cnames <- strvcat(temp)
info.rnames <- strvcat(['variable',varnames])
info.width <- 200
table <- reshape(permute(vdec_mean(:,shocks,HH), [1 3 2]),N,Ns*Nh)
mprint(table, info)

# yet another printout, with bands
for (h in HH){
    fprintf('horizon  <- #d\n',h)
    cnames <- {}
    table <- []
    for (is in 1:Ns){
        s <- shocks(is)
        cnames <- [cnames strcat(sprintf('s%d.',s),{'mean','q05','q95'})]
        table <- [table vdec_mean(:,s,h) vdec_l(:,s,h) vdec_u(:,s,h)]
    }
    info.cnames <- strvcat(cnames)
    info.rnames <- strvcat(['variable',varnames])
    mprint(table,info)
}

if (0){
    tt <- 0:nstep-1
    for (i in 1:length(shocks)){
        s <- shocks(i)
        hf <- figure(30+i)
        for (v in 1:N){
            subplot(ceil(N/2),2,v)
            toplot <- [ squeeze(mean(vdec_draws(v,s,:,:),4))...
                       squeeze(quantile(vdec_draws(v,s,:,:),0.05,4))...
                       squeeze(quantile(vdec_draws(v,s,:,:),0.95,4))...
                       ]
            plot(tt,toplot(:,1)','-k',tt,toplot(:,2)',':k',tt,toplot(:,3)',':k')
            title(['vdec:' char(varnames(s)) '->' char(varnames(v))])
            axis tight
            ylim([0 1])
        }
    }
}

}




vdecomp <- function(response){
# function vdec = vdecomp(response)
# IN: response: nvar by nshock by nstep matrix with impulse responses
cumsqrr <- cumsum(response.^2,3)
totals <- sum(cumsqrr,2)
vdec <- cumsqrr./repmat(totals,[1 size(response,1) 1])
}
