variables_y_default=c("gs1" ,"logsp500","us_rgdp","us_gdpdef","ebpnew")
variables_m_default=c("ff4_hf","sp500_hf")
variables_y_non_st_default=c("logsp500","us_rgdp","us_gdpdef")
variables_t_default=c("year","month")
study_period_default=zoo::as.yearmon(c("1984-02","2016-12"))
p_default=12
lambda_1_default=5
lambda_2_default=1
decay_default=lambda_2_default
tightness_default=1/lambda_1_default
exog_std_default=100000
#Model draw values for mcm chain
mcmc_settings_default=list(nchains=4,chains_size=1000,burning=4)
#Model draw values for mcm chain
#Outputs for the report