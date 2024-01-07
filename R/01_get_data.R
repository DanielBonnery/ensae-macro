get_data<-function(){
  dest_file=tempfile(fileext="zip")
"https://www.aeaweb.org/doi/10.1257/mac.20180090.data"|>
  downloader::download(dest_file)
if(!dir.exists(tempdir())){dir.create(tempdir(),recursive = TRUE)}
dest_file|>utils::unzip(exdir = tempdir())
list(dataset=file.path(tempdir(),"data","data_var","data.csv")|>read.csv(),
     dictionnary=file.path(tempdir(),"data","data_var","ydict.csv")|>read.csv())}

get_data_safe<-function(){
  x<-try(get_data())
  if(is.element("try-error",class(x))){
    list(dataset=file.path("extdata","data.csv")|>read.csv(),
         dictionnary=file.path("extdata","ydict.csv")|>read.csv())
         
    }else{x}}
