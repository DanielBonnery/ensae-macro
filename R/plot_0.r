plot_0<-function(data_ymt,variables_m,variables_y,dictionnary,path_out){
  
  variables_my<-c(variables_m,variables_y)
  my_labels<-dictionnary$nice_name|>setNames(dictionnary$name)|>(`[`)(variables_y)|>
    c("ff4_hf"="Surprise in \n the  3-month \n fed funds \n rate ",
      "sp500_hf"="Surprise in \n the \n S&P 500")|>
    gsub(pattern="\\newline",replacement="\n",fixed=TRUE)|>
    (`[`)(c(variables_my))
  
  x<-data_ymt|>dplyr::select(-date,-year,-month,-yearmon)
    rownames(x) <- NULL
    x <- as.data.frame((x |> is.na()))
    x$rownum <- 1:nrow(x)
    x$date=data_ymt$year+(data_ymt$month-1)/12
    (ggplot(data = reshape2::melt(x, id.vars = c("rownum","date"))|>
              dplyr::mutate(variable2=my_labels[variable]), 
           aes(x = variable2, 
                    y = date, fill = value)) + geom_tile() + 
      scale_fill_grey(name = "",
                      labels = c("Present", "Missing")) + theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
      labs(x = "Variables in Dataset", y = "Time")+ 
      scale_y_continuous(breaks=seq(1984,2016,2))+
      coord_flip()+
        theme(legend.position="bottom"))|>
      ggsave(filename = path_out)
}


plot_00<-function(data_ymt,variables_m,variables_y,
                  dictionnary,
                 path_out){
  
  variables_my<-c(variables_m,variables_y)
  
  my_labels<-dictionnary$nice_name|>setNames(dictionnary$name)|>(`[`)(variables_y)|>
    c("ff4_hf"="Surprise in \n the  3-month \n fed funds \n rate ",
      "sp500_hf"="Surprise in \n the \n S&P 500",
      "ff4_hf_s"="Sign of \n 3-m ffr\n surprise ",
      "sp500_hf_s"="Sign of \n S&P 500 surprise")|>
    gsub(pattern="\\newline",replacement="\n",fixed=TRUE)|>
    (`[`)(c(variables_my,paste0(variables_m,"_s")))
  
  (data_ymt|>
    dplyr::mutate(ff4_hf_s=sign(ff4_hf),sp500_hf_s=sign(sp500_hf))|>
    dplyr::select(date,one_of(c(variables_m,variables_y)))|>
    reshape2:::melt.data.frame(id.vars="date")|>
    dplyr::mutate(variable2=my_labels[variable])|>
    ggplot(aes(x=date,y=value))+
    geom_line()+
    facet_wrap("variable2",scales="free_y")+ylab("")+xlab("Month")+
      theme_bw())|>
    ggsave(filename = path_out)
}
