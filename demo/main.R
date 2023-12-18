
"R"|>list.files(full.names = TRUE)|>sapply(FUN = source)

dataset<-get_data()