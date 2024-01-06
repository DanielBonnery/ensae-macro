
"R"|>list.files(full.names = TRUE)|>sapply(FUN = source)

data_ymt<-get_data_safe()
