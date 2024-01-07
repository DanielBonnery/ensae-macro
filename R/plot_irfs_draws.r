#hh = plot_irfs_draws(irfs_draws, data.Nm+1:min(N,data.Nm+ny1), ss, varnames_nice, varnames, shocknames_nice, idscheme, qtoplot, [0 0 1], '', ylimits, transf); align_Ylabels(hh); saveTightFigure(hh,[fname '_irfy1'],'pdf')
#'@examples
#'tar_load(raw_data)
#'dictionnary=raw_data$dictionnary
plot_irfs_draws <- function(
    variables_m,
    variables_y,
    irfs_draws,
    dictionnary){
    variables_my<-c(variables_m,variables_y)
    irfs_draws|>
        plyr::aaply(1:3,
                    quantile,
                    c(.05,.16,.5,.84,.95))->qq
    names(dimnames(qq))[4]<-"quantile"
    
    qq|>reshape2::melt()|>reshape2::dcast(shock_of+on+time~quantile)->qqdf
    
    
    my_labels<-dictionnary$nice_name|>setNames(dictionnary$name)|>(`[`)(variables_y)|>
        c("ff4_hf"="Surprise in \n the  3-month \n fed funds \n rate ",
          "sp500_hf"="Surprise in \n the \n S&P 500")|>
        gsub(pattern="\\newline",replacement="\n",fixed=TRUE)|>
        (`[`)(variables_my)
    
    qqdf|>
        dplyr::filter(is.element(shock_of,c("ff4_hf","sp500_hf")))|>
        dplyr::mutate(shock_of2=
                          c("ff4_hf"="Monetary policy\n(negative co-movement)",
                            "sp500_hf"="CB information\n(positive co-movement)")[shock_of],
                      on2=my_labels[on]|>ordered(levels=my_labels))|>
        ggplot2::ggplot(aes(x=time,y=`50%`,group=interaction(shock_of,on)))+
        geom_ribbon(aes(ymin=`5%`,ymax=`95%`),alpha=.25,fill="orange")+
        geom_ribbon(aes(ymin=`16%`,ymax=`84%`),alpha=.5,fill="orange")+
        geom_line()+
        geom_hline(yintercept=0,size=.2)+
        facet_grid(on2~shock_of2,scales = "free_y")+
        xlab("Months")+ylab("")+
        theme_bw()
    
    
}
