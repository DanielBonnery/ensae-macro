#hh = plot_irfs_draws(irfs_draws, data.Nm+1:min(N,data.Nm+ny1), ss, varnames_nice, varnames, shocknames_nice, idscheme, qtoplot, [0 0 1], '', ylimits, transf); align_Ylabels(hh); saveTightFigure(hh,[fname '_irfy1'],'pdf')
#'@examples
#'tar_load(raw_data)
#'dictionnary=raw_data$dictionnary
plot_irfs_draws <- function(
    variables_m,
    variables_y,
    irfs_draws,
    dictionnary,
    path_out="outputs/the_plot_irfs.pdf"){
    variables_my<-c(variables_m,variables_y)
    irfs_draws|>
        plyr::aaply(1:3,
                    quantile,
                    c(.05,.16,.5,.84,.95))->qq
    names(dimnames(qq))[4]<-"quantile"
    
    qq|>reshape2::melt()->qqdf1
    qqdf1|>reshape2::dcast(shock_of+on+time~quantile)->qqdf
    
    
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
                      on2=my_labels[on]|>ordered(levels=my_labels))->qqdf
    qqdf1|>
        dplyr::filter(is.element(shock_of,c("ff4_hf","sp500_hf")))|>
        dplyr::mutate(shock_of2=
                          c("ff4_hf"="Monetary policy\n(negative co-movement)",
                            "sp500_hf"="CB information\n(positive co-movement)")[shock_of],
                      on2=my_labels[on]|>ordered(levels=my_labels),
                      sizeqline=c("5%"="5% - 95%" ,
                                  "16%"="16% - 84%", 
                                  "50%"="50%" ,"84%"="16% - 84%",
                                  "95%"="5% - 95%")[quantile]|>
                          ordered(c("5% - 95%" ,"16% - 84%", 
                                    "50%")))->qqdf1
    
    
    
       
    theplot<-
        qqdf    |>
            ggplot2::ggplot(aes(x=time,y=`50%`,group=interaction(shock_of,on)))+
            geom_ribbon(aes(ymin=`5%`,ymax=`95%`),alpha=.15,fill="orange")+
            geom_ribbon(aes(ymin=`16%`,ymax=`84%`),alpha=.5,fill="orange")+
            geom_line(data=qqdf1,aes(x=time,y=value,group=quantile,size=sizeqline),
                      colour="black")+
            geom_hline(yintercept=0,size=.2)+
            facet_grid(on2~shock_of2,scales = "free_y")+
            xlab("Months")+ylab("")+
            theme_bw()+
            scale_size_manual("",values=c("5% - 95%"=.1,"16% - 84%"=.2,"50%"=.5))+
            theme(legend.position = "bottom")
        theplot|>
        ggsave(filename=path_out,width = 19,height=25,units = "cm")
    
    
}
