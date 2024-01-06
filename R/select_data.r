
#'@examples
#'tar_load(raw_data)
#'variables_y=c("gs1" ,"logsp500","us_rgdp","us_gdpdef","ebpnew")
#'variables_m=c("ff4_hf","sp500_hf")
#'variables_t=c("year","month")
#'study_period=zoo::as.yearmon(c("1984-02","2016-12"))
#'data_ymt= select_data(raw_data,variables_y,variables_m,variables_time,data_range)
select_data<-function(raw_data,variables_y,variables_m,variables_t,study_period){
  raw_data|>
    dplyr::select(one_of(c(variables_t,variables_m,variables_y)))|>
    dplyr::mutate(yearmon=zoo::as.yearmon(paste0(year,"-",month)),
                  date=paste0(year,"-",month,"-01")|>as.Date("%Y-%m-%d"))|>
    dplyr::filter(yearmon>=study_period[1],
                  yearmon<=study_period[2])|>
    dplyr::mutate(weight=1)
}
