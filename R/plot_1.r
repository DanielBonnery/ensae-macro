#'@examples
#'tar_load(data_ymt)
plot_1<-function(data_ymt,path_out){
  if(!dir.exists(dirname(path_out))){dir.create(dirname(path_out),recursive = TRUE)}
  data_ymt|>ggplot2::ggplot(aes(x=ff4_hf,y=sp500_hf,color=time))+
  geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)+
    geom_point(size=.7)+
    ylab("High frequency Standard and poor surprises")+
    xlab("High frequency ff4")+
    geom_text(data=data.frame(ff4_hf=c(-.3,.15,-.3,.15),sp500_hf=c(-2,-2,2,2),t=c("I","II","III","IV")),
              mapping=aes(x=ff4_hf,y=sp500_hf,label=t),colour="black")+theme_bw()+labs(colour = "Time")|>
    ggsave(filename = path_out)
}
