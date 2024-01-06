plot_0<-function(data_ymt,path_out="outputs/the_plot_0.png"){
  x<-data_ymt|>dplyr::select(-date,-year,-month,-yearmon)
    rownames(x) <- NULL
    x <- as.data.frame((x |> is.na()))
    x$rownum <- 1:nrow(x)
    x$date=data_ymt$year+(data_ymt$month-1)/12
    (ggplot(data = reshape2::melt(x, id.vars = c("rownum","date")), 
           aes(x = variable, 
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