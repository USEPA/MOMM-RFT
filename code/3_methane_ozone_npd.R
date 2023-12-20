#######################################################################################
### Title:        3_methane_ozone_npd.R
### Purpose:      This file will read all output files from the Methane-Ozone Mortality
##                Model-Reduced Form Tool and calculate net present damages
##                and aggregate statistics across all trials. (for each model & country)
##                The calculations are done for each model and country and combined at
##                the end to help reduce the memory usage when processing all 10,000 RFF scenarios
### Written by:   US EPA, Climate Change Division (OAP)
### Date Created: 3/15/2023      
### Last Edited:  12/20/2023
##      Inputs:                                                                       ##
##        -damages_mean_NOx_X_momm_rft.parquet                                        ##
##        -Final Country Grid Col_Row Index.csv                                       ##
##     Outputs:
##        -npd_full_streams_rff_NOx_X_MODEL_COUNTRY.parquet                           ##
##        - npd_country_rff_means_NOX_MODEL_COUNTRY.parquet                           ##
##        - npd_global_rff_means_NOX_MODEL.csv                                        ##
## Units: All $ values for $2020                                                      ##
########################################################################################

rm(list = ls()); gc()
#necessary packages

packs<-c("dplyr","tidyverse","readxl","purrr","foreach","utils")
for (package in packs) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}

library(dplyr) #for general data manipulation
library(tidyverse) #for general data manipulation
library(readxl) #to read in xlsx files
library(foreach) #to do parallel processing in loop
library(purrr)  #for general data manipulation
library(utils)
library(arrow)
#setwd("~/shared/OAR/OAP/CCD/CSIB/Methane-Ozone/MOMM-RFT")

Inputs <- file.path('output',"rft")
Outputs <- file.path("output",'npd')

PULSEYEAR  = 2080
PULSE = 275e6 #CH4 pulse of 275 million metric tonnes
allRFF = 'mean' #(set to 1 if reading in all RFF scenario, otherwise read in mean)

countries <- read.csv(file.path('input',"Final Country Grid Col_Row Index.csv"))[,c(1,3:4,6,8)]
countries <- countries %>% filter(Region != "")

NOx_scalar = 1

read_momm_rft = 
  function(x,model){
    ttemp <- 
    read_parquet(x) %>%
      filter(Model == model) %>%
             #LocID == countries$COL[icountry]) %>%
      mutate(damages = annual_impacts,
             damages_wlag = annual_impacts_wlag) %>%
      group_by(ModelYear,LocID,Model,trial) %>% # sum across all countries
      summarise(damages = sum(damages),
                damages_wlag = sum(damages_wlag),
                pop = sum(pop),
                gdp = sum(gdp),
                .groups='keep') %>%
      ungroup() %>%
      select(Model,ModelYear,LocID,trial,damages,damages_wlag,gdp,pop)#%>%
      #mutate(emissions.year   = YEAR)
  }


for (MODEL in c('MMM')) { #},'CESM2','HadGEM','GISS','GFDL','MIROC')) {
  if (allRFF==1){
    # To read all files:
    damages = 
      list.files(Inputs, pattern = "damages_\\d+\\_vsl10_NOx_1*", full.names = T) %>% 
      map_df(~read_momm_rft(., MODEL))
  } else {
    # to read specific file:
    damages =
      read_momm_rft(file.path(Inputs,paste0('damages_mean_vsl10_NOx_',NOx_scalar,'_',PULSEYEAR,'_momm_rft.parquet')),MODEL)
  }
  print(MODEL)
  
  countries <- damages %>% distinct(LocID)

  #damages are calculated as a function of GDP (growth is corrected for damages)
  damages  <- damages %>%
    group_by(Model,trial,LocID) %>% #LocID
    mutate(damages.pct        = 1 - (1/(1+(damages/gdp))),     #fraction of GDP that are damages
      damages_wlag.pct        = 1 - (1/(1+(damages_wlag/gdp))),
      ypc                     = ((1-damages.pct) * gdp)/pop,   #gdp per capita, with gdp reduced by % of damages
      ypc_wlag                = ((1-damages_wlag.pct) * gdp)/pop,
      base.ypc                = case_when(ModelYear == PULSEYEAR ~ ypc, T ~ 0),
      base.ypc                = max(base.ypc),                 #base year gdp/cap ## TO-DO, clunky way to ensure that the base ypc for discrete time discount factor can be used in the discounting function, fix later
      base.ypc_wlag           = case_when(ModelYear == PULSEYEAR ~ ypc_wlag, T ~ 0),
      base.ypc_wlag           = max(base.ypc_wlag),                 #base year gdp/cap ## TO-DO, clunky way to ensure that the base ypc for discrete time discount factor can be used in the discounting function, fix later
      damages.marginal        = (gdp * damages.pct) / PULSE,   #calculate the damages as a % of GDP and divide by pulse size (for $/tonne CH4)
      damages_wlag.marginal   = (gdp * damages_wlag.pct) / PULSE,
      ) %>%  
  select(-c(contains('pct'))) %>%
  ungroup()
  
  ## discount rates
  ## Using stochastic Ramsey discount rate calculation, following Rennert et al., 2022
  rates = tibble(rate = c('1.5% Ramsey', '2.0% Ramsey', '2.5% Ramsey', '3.0% Ramsey', '2.0% CDR', '3.0% CDR'),
               rho  = c(exp(0.000091496)-1, exp(0.001972641)-1, exp(0.004618785)-1, exp(0.007702711)-1, 0.02, 0.03), ## under discrete time, need to transform the rho that was calibrated using continuous time 
               #rho = c(0.000091496, 0.001972641, 0.004618785, 0.007702711, 0.02, 0.03),
               eta  = c(1.016010261, 1.244459020, 1.421158057, 1.567899395, 0, 0))

  
  for (COUNTRY in 1:nrow(countries)) {
    
    ## object to store data
    data = tibble()
    ## object to store data
    means = tibble()
    
    
    print(COUNTRY)
    
    for (RATE in 1:length(rates$rate)){
  
    
    # ## test 
    # RATE = 5
    #print(RATE)
  
    ## get damage parameters
    rate = rates$rate[[RATE]]
    rho  = rates$rho[[RATE]]
    eta  = rates$eta[[RATE]]
  
    ## code calculates a stochastic Ramsey discount factor due to the discrete time nature of the results
    ## More info in Rennert et al., 2022
    ## get streams of discounted damages and net present damages
    data = 
      bind_rows(
        data,
        damages %>%
          filter(LocID == countries$LocID[COUNTRY]) %>%
          group_by(trial,Model,LocID) %>% #LocID
          mutate(discount.rate                   = rate,
               discount.factor                   = case_when(grepl("Ramsey", rate) ~ (base.ypc/ypc)^eta/(1+rho)^(ModelYear-PULSEYEAR),
                                                       T ~ 1/(1+rho)^(ModelYear-PULSEYEAR)),
               discount.factor_wlag              = case_when(grepl("Ramsey", rate) ~ (base.ypc_wlag/ypc_wlag)^eta/(1+rho)^(ModelYear-PULSEYEAR),
                                                       T ~ 1/(1+rho)^(ModelYear-PULSEYEAR)),
               damages.marginal.discounted       = damages.marginal * discount.factor,
               damages.marginal.discounted_wlag  = damages_wlag.marginal * discount.factor_wlag,
               npd                               = sum(damages.marginal.discounted, na.rm = F),
               npd_wlag                          = sum(damages.marginal.discounted_wlag, na.rm = F)) %>%
          mutate(certainty.eq.adjustment         = case_when(grepl("Ramsey", rate) ~ (base.ypc^-eta)/mean(base.ypc^-eta, na.rm = T),
                                                         T ~ 1)) %>%
          mutate(certainty.eq.adjustment.wlag   = case_when(grepl("Ramsey", rate) ~ (base.ypc_wlag^-eta)/mean(base.ypc_wlag^-eta, na.rm = T),
                                                          T ~ 1)) %>%
        ungroup()
    )
  

  ## export full streams
 if (allRFF ==1){
  data %>%
    write_parquet(file.path(Outputs, paste0('npd_full_streams_rff_vsl10_NOx_',NOx_scalar,'_',MODEL,'_',countries$LocID[COUNTRY],'_',PULSEYEAR,'.parquet')))
 } else {
   data %>%
     write_parquet(file.path(Outputs, paste0('npd_full_streams_vsl10_NOx_',NOx_scalar,'_',MODEL,'_',countries$LocID[COUNTRY],'_',PULSEYEAR,'.parquet')))
 }
}
  # recover summary statistics across all trials
  # Note that the stats and certainty equivalent corrections are only relevant when model is run for all trials (not the mean RFF-SP)
  means =
    bind_rows(
      means,
      data %>%
        group_by(Model,LocID,discount.rate) %>% #calculate statistics for each country and discount rate
        summarise(mean_npd         = mean(npd),
                  mean.npd.cert.eq = mean(npd * certainty.eq.adjustment, na.rm = T),
                  npd_2.5          = quantile(npd, .025, na.rm = T), #these uncertainties are socioeconomic uncertainties (not BenMAP) - but only when results are run for all RFF-SPs (not just the mean)
                  npd_97.5         = quantile(npd, .975, na.rm = T),
                  median           = median(npd, na.rm = T),
                  mean_npd_wlag    = mean(npd_wlag),
                  mean.npd.wlag.cert.eq = mean(npd_wlag * certainty.eq.adjustment.wlag, na.rm = T),
                  npd_2.5_wlag     = quantile(npd_wlag, .025, na.rm = T),
                  npd_97.5_wlag    = quantile(npd_wlag, .975, na.rm = T),
                  median_wlag      = median(npd_wlag, na.rm = T),
                .groups = 'drop')
  )


  ## export summary stats
  if (allRFF ==1){
    means %>%
        write_csv(file.path(Outputs,paste0('npd_country_rff_means_vsl10_NOx_',NOx_scalar,'_',MODEL,'_',countries$LocID[COUNTRY],'_',PULSEYEAR,'.csv')))
  } else {
    means %>%
      write_csv(file.path(Outputs,paste0('npd_country_means_vsl10_NOx_',NOx_scalar,'_',MODEL,'_',countries$LocID[COUNTRY],'_',PULSEYEAR,'.csv')))
  }

}

}

#Do global calculation

read_results = 
  function(x){
    ttemp <- 
      read_parquet(x) %>%
      filter(ModelYear == PULSEYEAR) #%>% #all npd years are the same
      #select(LocID,trial,discount.rate,npd,npd_wlag)
  }


if (allRFF ==1){
  Results_comb = 
    list.files(Outputs, pattern = paste0("full_streams_rff_vsl10_NOx_",NOx_scalar,'_MMM_\\d+\\_',PULSEYEAR),full.names = T) %>% 
    map_df(~read_results(.))
} else {
  Results_comb = 
    list.files(Outputs, pattern = paste0("full_streams_vsl10_NOx_",NOx_scalar,'_MMM_\\d+\\_',PULSEYEAR),full.names = T) %>% 
    map_df(~read_results(.))
}


#export global stats
glob_means = tibble()
# Note that the stats and certainty equivalent corrections are only relevant when model is run for all trials (not the mean RFF-SP)
  glob_means =
      bind_rows(
        glob_means,
        Results_comb %>%
        group_by(discount.rate, trial) %>% #group by rate and trial and then sum across all countries
        summarise(
          npd.cert.eq = sum(npd * certainty.eq.adjustment, na.rm = T),
          npd.wlag.cert.eq = sum(npd_wlag * certainty.eq.adjustment.wlag, na.rm = T),
          npd_wlag = sum(npd_wlag),
          npd      = sum(npd),
          .groups = 'keep') %>%
          ungroup() %>%
        group_by(discount.rate) %>%      #next, group by discount rate to calculate stats across all trials
        summarise(mean_npd      = mean(npd),
                  mean.npd.cert.eq = mean(npd.cert.eq),
                  npd_2.5       = quantile(npd, .025, na.rm = T), #these are the socioeconomic stats (not BenMAP)
                  npd_97.5      = quantile(npd, .975, na.rm = T),
                  median        = median(npd, na.rm = T),
                  mean_npd_wlag = mean(npd_wlag),
                  mean.npd.wlag.cert.eq = mean(npd.wlag.cert.eq),
                  npd_2.5_wlag  = quantile(npd_wlag, .025, na.rm = T),
                  npd_97.5_wlag = quantile(npd_wlag, .975, na.rm = T),
                  median_wlag   = median(npd_wlag, na.rm = T),
                .groups = 'keep')
  )
# #
#  #write data
  if (allRFF ==1){
    glob_means %>%
      write_csv(file.path(Outputs,paste0('npd_global_rff_means_vsl10_NOx_',NOx_scalar,'_',MODEL,'_',PULSEYEAR,'.csv')))
  } else {
    glob_means %>%
      write_csv(file.path(Outputs,paste0('npd_global_means_vsl10_NOx_',NOx_scalar,'_',MODEL,'_',PULSEYEAR,'.csv')))
  }

  #CODE END  