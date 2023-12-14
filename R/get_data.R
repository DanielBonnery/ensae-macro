get_data<-function(){
  dest_file=tempfile(fileext="zip")
"https://www.aeaweb.org/doi/10.1257/mac.20180090.data"|>
  downloader::download(dest_file)
dest_file|>utils::unzip(exdir = tempdir())
file.path(tempdir(),"data/data_var/data.csv")|>read.csv()}
