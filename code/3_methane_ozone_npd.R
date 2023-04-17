#####
## Title:   3_methane_ozone_npd.R
## Purpose: This file will read all output files from the Methane-Ozone Mortality
##          Model-Reduced Form Tool and calculate net present damages
##          and aggregate statistics across all trials.
##          
## Inputs:  output/damages/rffsp/damages_[scenario].parquet
## Outputs: output/damages/rffsp/damages_[scenario].parquet
## Written by: US EPA, Climate Change Division (OAP); April 2023
## Last updated: 4/12/2023 by E. McDuffie
## Notes:       
##
#####
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
setwd("~/shared/OAR/OAP/CCD/CSIB/Methane-Ozone/MOMM-RFT")

Inputs <- file.path('output',"rft")
Outputs <- file.path("output")

YEAR=2020
PULSE = 275e6 #CH4 pulse of 275 million metric tonnes

countries<-read.csv(file.path('input',"Final Country Grid Col_Row Index_EEM.csv"))[,c(1,3:4,6,8)]
countries <- countries %>% filter(Region != "")

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
#for (COUNTRY in c(41:42)) { #nrow(countries)) {
  damages = 
    list.files(Inputs, full.names = T) %>% 
    map_df(~read_momm_rft(., MODEL))
  print(MODEL)
  
  countries <- damages %>% distinct(LocID)

  #damages are calculated as a function of GDP (growth is corrected for damages)
  damages  <- damages %>%
    group_by(Model,trial,LocID) %>% #LocID
    mutate(#damages.pct        = 1 - (1/(1+(damages/gdp))),     #fraction of GDP that are damages
      damages_wlag.pct        = 1 - (1/(1+(damages_wlag/gdp))),
      #damages.pct.2_5        = 1 - (1/(1+(damages_2_5/gdp))),
      #damages.pct.97_5       = 1 - (1/(1+(damages_97_5/gdp))),
      #ypc                     = ((1-damages.pct) * gdp)/pop,   #gdp per capita, with gdp reduced by % of damages
      ypc_wlag                = ((1-damages_wlag.pct) * gdp)/pop,
      #base.ypc                = case_when(ModelYear == YEAR ~ ypc, T ~ 0),
      #base.ypc                = max(base.ypc),                 #base year gdp/cap ## TO-DO, clunky way to ensure that the base ypc for discrete time discount factor can be used in the discounting function, fix later
      base.ypc_wlag           = case_when(ModelYear == YEAR ~ ypc_wlag, T ~ 0),
      base.ypc_wlag           = max(base.ypc_wlag),                 #base year gdp/cap ## TO-DO, clunky way to ensure that the base ypc for discrete time discount factor can be used in the discounting function, fix later
      #damages.marginal        = (gdp * damages.pct) / PULSE,   #calculate the damages as a % of GDP and divide by pulse size (for $/tonne CH4)
      damages_wlag.marginal   = (gdp * damages_wlag.pct) / PULSE,
      #damages_wlag.marginal.2_5   = damages_wlag.delta.2_5 / 275e6,
      #damages.marginal.97_5  = damages.delta.97_5 / 275e6
      ) %>%  
  select(-c(contains('pct'))) %>%
  ungroup()
  print('here')
  
  ## discount rates
  rates = tibble(rate = c('1.5% Ramsey', '2.0% Ramsey', '2.5% Ramsey', '3.0% Ramsey', '2.0% CDR', '3.0% CDR'),
               rho  = c(exp(0.000091496)-1, exp(0.001972641)-1, exp(0.004618785)-1, exp(0.007702711)-1, 0.02, 0.03), ## under discrete time, need to transform the rho that was calibrated using continuous time 
               #rho = c(0.000091496, 0.001972641, 0.004618785, 0.007702711, 0.02, 0.03),
               eta  = c(1.016010261, 1.244459020, 1.421158057, 1.567899395, 0, 0))
  #rate_name = c('1pnt5Ram','2Ram','2pnt5Ram','3Ram','2CDR','3CDR')

  
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
  
    ## get streams of discounted damages and net present damages
    data = 
      bind_rows(
        data,
        damages %>%
          filter(LocID == countries$LocID[COUNTRY]) %>%
          group_by(trial,Model,LocID) %>% #LocID
          mutate(discount.rate               = rate,
               #discount.factor1             = case_when(grepl("Ramsey", rate) ~ (1/(1+rho + eta*growth)),
               #                                          T ~ 1/(1+rho)^(ModelYear-emissions.year)),
               #discount.factor               = case_when(grepl("Ramsey", rate) ~ (base.ypc/ypc)^eta/(1+rho)^(ModelYear-YEAR),
               #                                        T ~ 1/(1+rho)^(ModelYear-YEAR)),
               discount.factor_wlag          = case_when(grepl("Ramsey", rate) ~ (base.ypc_wlag/ypc_wlag)^eta/(1+rho)^(ModelYear-YEAR),
                                                       T ~ 1/(1+rho)^(ModelYear-YEAR)),
               #damages.marginal.discounted   = damages.marginal * discount.factor,
               damages.marginal.discounted_wlag  = damages_wlag.marginal * discount.factor_wlag,
               #damages.marginal.discounted  = case_when(is.na(discount.factor) ~ damages.marginal,
               #                                          TRUE ~ damages.marginal * discount.factor),
               #npd                          = sum(damages.marginal.discounted, na.rm = F),
               npd_wlag                     = sum(damages.marginal.discounted_wlag, na.rm = F)) %>%
               #damages.marginal.2_5.discounted  = case_when(is.na(discount.factor) ~ damages.marginal.2_5,
              #                                              TRUE ~ damages.marginal.2_5 * discount.factor),
              # npd.2_5                       = sum(damages.marginal.2_5.discounted, na.rm = F),
              # damages.marginal.97_5.discounted  = case_when(is.na(discount.factor) ~ damages.marginal.97_5,
               #                                              TRUE ~ damages.marginal.97_5 * discount.factor),
               #npd.97_5                       = sum(damages.marginal.97_5.discounted, na.rm = F)) %>%
        ungroup()
    )
  

  ## export full streams
 data %>%
#    #mutate(LocID = countries$COL[COUNTRY]) %>%
    write_parquet(file.path(Outputs, paste0('npd_full_streams_rff_',MODEL,'_',countries$LocID[COUNTRY],'.parquet')))
#     write_parquet(file.path(Outputs, paste0('npd_full_streams_rff_',MODEL,'.parquet')))
}
  # recover summary statistics across all trials
  means =
    bind_rows(
      means,
      data %>%
        group_by(Model,LocID,discount.rate) %>% #calculate statistics for each country and discount rate
        summarise(#mean_npd      = mean(npd),
                  #npd_2.5       = quantile(npd, .025, na.rm = T), #these uncertainties are socioeconomic uncertainties (not BenMAP)
                  #npd_97.5      = quantile(npd, .975, na.rm = T),
                  #median        = median(npd, na.rm = T),
                  mean_npd_wlag = mean(npd_wlag),
                  npd_2.5_wlag  = quantile(npd_wlag, .025, na.rm = T),
                  npd_97.5_wlag = quantile(npd_wlag, .975, na.rm = T),
                  median_wlag   = median(npd_wlag, na.rm = T),
                  #`std. err.` = sd(npd, na.rm = T),
                  #min         = min(npd, na.rm = T),
                  #max         = max(npd),
                .groups = 'drop')
  )


  ## export summary stats
  means %>%
    #mutate(LocID = countries$COL[COUNTRY]) %>%
    #write_csv(file.path(Outputs,paste0('npd_country_rff_means_',countries$COL[COUNTRY],'.csv')))
    write_csv(file.path(Outputs,paste0('npd_country_rff_means_',MODEL,'_',countries$LocID[COUNTRY],'.csv')))


  #data = unique(data) #get unique rows (filter out duplicate years)

  #  glob_means = 
  #    bind_rows(
  #      glob_means, 
  #      data %>%
  #        filter(ModelYear == 2020) %>% #npd's are the same for all years in the df
  #        group_by(Model,discount.rate,trial) %>% #goup by rate and trial and then sum across all countries
  #        summarise(npd = sum(npd),
  #                  npd_wlag = sum(npd_wlag),
  #                  .groups = 'keep') %>%
  #        ungroup() %>%
  #        group_by(Model,discount.rate) %>%      #next, group by trial to calculate stats across all trials
  #        summarise(mean_npd      = mean(npd),
  #                  npd_2.5       = quantile(npd, .025, na.rm = T), #these are the socioeconomic stats (not BenMAP)
  #                  npd_97.5      = quantile(npd, .975, na.rm = T),
  #                  median        = median(npd, na.rm = T),
  #                  mean_npd_wlag = mean(npd_wlag),
  #                  npd_2.5_wlag  = quantile(npd_wlag, .025, na.rm = T),
  #                  npd_97.5_wlag = quantile(npd_wlag, .975, na.rm = T),
  #                  median_wlag   = median(npd_wlag, na.rm = T),
  #                  #`std. err.` = sd(npd, na.rm = T),
  #                  #min         = min(npd, na.rm = T),
  #                  #max         = max(npd),
  #                .groups = 'keep') 
  #  )
  # # 
  #  #write data
  #  glob_means %>% 
  #    write_csv(file.path(Outputs,paste0('npd_global_rff_means_',MODEL,'_',rate_name[RATE],'.csv')))
}

}

#Do global calculation

#Result.files<- list.files(Outputs,pattern=paste0("npd_country_rff_MMM",countries$LocID[COUNTRY],'.parquet'),full.names = T)
#list.files(Inputs, full.names = T)



read_results = 
  function(x){
    ttemp <- 
      read_parquet(x) %>%
      filter(ModelYear == 2020) %>% #all npd years are the same
      select(LocID,trial,discount.rate,npd_wlag)
  }


  #for (COUNTRY in c(41:42)) { #nrow(countries)) {
Results_comb = 
    list.files(Outputs, pattern = "full_streams_rff_MMM",full.names = T) %>% 
    map_df(~read_results(.))

#Result.files <- file.path(Outputs,"npd_country_rff_MMM") %>%
#  Result.files <- list.files(Outputs, pattern = "full_streams_rff_MMM")  # "\\.parquet") #%>%
#  Results_comb<-lapply(paste0(Outputs,"/",Result.files[1:2]), read_parquet)

#export global stats
glob_means = tibble()

  glob_means =
      bind_rows(
        glob_means,
        Results_comb %>%
        group_by(discount.rate, trial) %>% #group by rate and trial and then sum across all countries
        summarise(
          npd_wlag = sum(npd_wlag),
          .groups = 'keep') %>%
          ungroup() %>%
        group_by(discount.rate) %>%      #next, group by discount rate to calculate stats across all trials
        summarise(#mean_npd      = mean(npd),
#                  npd_2.5       = quantile(npd, .025, na.rm = T), #these are the socioeconomic stats (not BenMAP)
#                  npd_97.5      = quantile(npd, .975, na.rm = T),
#                  median        = median(npd, na.rm = T),
                  mean_npd_wlag = mean(npd_wlag),
                  npd_2.5_wlag  = quantile(npd_wlag, .025, na.rm = T),
                  npd_97.5_wlag = quantile(npd_wlag, .975, na.rm = T),
                  median_wlag   = median(npd_wlag, na.rm = T),
#                  #`std. err.` = sd(npd, na.rm = T),
#                  #min         = min(npd, na.rm = T),
#                  #max         = max(npd),
                .groups = 'keep')
  )
# #
#  #write data
  glob_means %>%
    write_csv(file.path(Outputs,paste0('npd_global_rff_means_',MODEL,'.csv')))

#1. Read in npd_full_streams_rff_MMM_x.csv files, bind,
#then sum across countries and calculate stats