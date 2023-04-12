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
Inputs <- file.path('output',"rft")
Outputs <- file.path("output")

YEAR=2020

read_momm_rft = 
  function(x,model){
    read_parquet(x) %>%
      filter(Model == model) %>%
      mutate(damages = annual_impacts,
             damges_wlag = annual_impacts_wlag) %>%
      select(ModelYear,LocID,trial,damages, damges_wlag,gdp,pop)%>%
      mutate(emissions.year   = YEAR)
  }

for (MODEL in c('MMM','CESM2','HadGEM','GISS','GFDL','MIROC')) {
  damages = 
    list.files(Inputs, full.names = T) %>% 
    map_df(~read_momm_rft(., MODEL))


  #damages are calculated as a function of GDP (growth is corrected for damages)
  damages  <- damages %>%
    group_by(LocID,trial) %>% 
    mutate(damages.pct            = 1 - (1/(1+(damages/gdp))),
      damages_wlag.pct            = 1 - (1/(1+(damges_wlag/gdp))),
      #damages.pct.2_5        = 1 - (1/(1+(damages_2_5/gdp))),
      #damages.pct.97_5       = 1 - (1/(1+(damages_97_5/gdp))),
      ypc                    = ((1-damages.pct) * gdp)/pop,
      ypc_wlag               = ((1-damages_wlag.pct) * gdp)/pop,
      base.ypc               = case_when(ModelYear == emissions.year ~ ypc, T ~ 0),
      base.ypc               = max(base.ypc),
      gdp_percap             = ((1-damages.pct) * gdp)/pop,
      gdp_percap_wlag        = ((1-damages_wlag.pct) * gdp)/pop,
      #growth                 = (gdp_percap - lag(gdp_percap))/lag(gdp_percap), ##**This does not internalize baseline damages
      #        # ypc                   = ((1-damages.baseline.pct) * gdp)/pop,
      #         base.growth            = case_when(ModelYear == emissions.year ~ growth, T ~ 0),
      #        # base.ypc              = max(base.ypc), ## TO-DO, clunky way to ensure that the base ypc for discrete time discount factor can be used in the discounting function, fix later
      #        # damages.baseline      = gdp * damages.baseline.pct,
      damages.delta          = gdp * damages.pct,
      damages_wlag.delta          = gdp * damages_wlag.pct,
      #damages.delta.2_5      = gdp * damages.pct.2_5,
      #damages.delta.97_5     = gdp * damages.pct.97_5,
      damages.marginal        = damages.delta / 275e6,
      damages_wlag.marginal   = damages_wlag.delta / 275e6,
      #damages_wlag.marginal.2_5   = damages_wlag.delta.2_5 / 275e6,
      #damages.marginal.97_5  = damages.delta.97_5 / 275e6
      ) %>%  
  select(-c(contains("delta"),contains('pct'))) %>%
  ungroup()

  ## discount rates
  rates = tibble(rate = c('1.5% Ramsey', '2.0% Ramsey', '2.5% Ramsey', '3.0% Ramsey', '2.0% CDR', '3.0% CDR'),
               rho  = c(exp(0.000091496)-1, exp(0.001972641)-1, exp(0.004618785)-1, exp(0.007702711)-1, 0.02, 0.03), ## under discrete time, need to transform the rho that was calibrated using continuous time 
               #rho = c(0.000091496, 0.001972641, 0.004618785, 0.007702711, 0.02, 0.03),
               eta  = c(1.016010261, 1.244459020, 1.421158057, 1.567899395, 0, 0))

  ## object to store data
  data = tibble()
  ## object to store data
  means = tibble()

  for (RATE in 1:length(rates$rate)){
  
    # ## test 
    # RATE = 5
  
    ## get damage parameters
    rate = rates$rate[[RATE]]
    rho  = rates$rho[[RATE]]
    eta  = rates$eta[[RATE]]
  
    ## get streams of discounted damages and net present damages
    data = 
      bind_rows(
        data,
        damages %>%
          group_by(LocID) %>%
          mutate(discount.rate               = rate,
               #discount.factor1             = case_when(grepl("Ramsey", rate) ~ (1/(1+rho + eta*growth)),
               #                                          T ~ 1/(1+rho)^(ModelYear-emissions.year)),
               discount.factor             = case_when(grepl("Ramsey", rate) ~ (base.ypc/ypc)^eta/(1+rho)^(ModelYear-emissions.year),
                                                       T ~ 1/(1+rho)^(ModelYear-emissions.year)),
               discount.factor_wlag         = case_when(grepl("Ramsey", rate) ~ (base.ypc/ypc_wlag)^eta/(1+rho)^(ModelYear-emissions.year),
                                                       T ~ 1/(1+rho)^(ModelYear-emissions.year)),
               damages.marginal.discounted  = damages.marginal * discount.factor,
               damages.marginal.discounted_wlag  = damages_wlag.marginal * discount.factor_wlag,
               #damages.marginal.discounted  = case_when(is.na(discount.factor) ~ damages.marginal,
               #                                          TRUE ~ damages.marginal * discount.factor),
               npd                          = sum(damages.marginal.discounted, na.rm = F),
               npd_wlag                     = sum(damages.marginal.discounted_wlag, na.rm = F)) %>%
               #damages.marginal.2_5.discounted  = case_when(is.na(discount.factor) ~ damages.marginal.2_5,
              #                                              TRUE ~ damages.marginal.2_5 * discount.factor),
              # npd.2_5                       = sum(damages.marginal.2_5.discounted, na.rm = F),
              # damages.marginal.97_5.discounted  = case_when(is.na(discount.factor) ~ damages.marginal.97_5,
               #                                              TRUE ~ damages.marginal.97_5 * discount.factor),
               #npd.97_5                       = sum(damages.marginal.97_5.discounted, na.rm = F)) %>%
        ungroup()
    )
  }

  ## export full streams
  data %>% 
    write_parquet(file.path(Outputs, paste0('npd_full_streams_rff_',MODEL,'.parquet')))

  ## recover summary statistics across all trials
  means = 
    bind_rows(
      means, 
      data %>%
        group_by(LocID,discount.rate) %>%
        summarise(mean_npd  = mean(npd),
                npd_2.5      = quantile(npd, .025, na.rm = T),
                npd_97.5     = quantile(npd, .975, na.rm = T),
                mean_npd_wlag = mean(npd_wlag),
                npd_2.5_wlag  = quantile(npd_wlag, .025, na.rm = T),
                npd_97.5_wlag = quantile(npd_wlag, .975, na.rm = T),
                #median      = median(npd, na.rm = T),
                #`std. err.` = sd(npd, na.rm = T),
                #min         = min(npd, na.rm = T),
                #`0.5%`      = quantile(npd, .005, na.rm = T),
                #`1%`        = quantile(npd, .01,  na.rm = T),
                #`2.5%`      = quantile(npd, .025, na.rm = T),
                #`5%`        = quantile(npd, .05,  na.rm = T),
                #`10%`       = quantile(npd, .10,  na.rm = T),
                #`25%`       = quantile(npd, .25,  na.rm = T),
                #`75%`       = quantile(npd, .75,  na.rm = T),
                #`90%`       = quantile(npd, .90,  na.rm = T),
                #`95%`       = quantile(npd, .95,  na.rm = T),
                #`97.5%`     = quantile(npd, .975, na.rm = T),
                #`99%`       = quantile(npd, .99,  na.rm = T),
                #`99.5%`     = quantile(npd, .995, na.rm = T),
                #max         = max(npd),
                .groups = 'drop') 
  )


  ## export summary stats
  means %>% 
    write_csv(file.path(Outputs,paste0('npd_country_rff_means_',MODEL,'.csv')))

  #export global stats
  glob_means = tibble()
  #data = unique(data) #get unique rows (filter out duplicate years)

  glob_means = 
    bind_rows(
      glob_means, 
      data %>%
        filter(ModelYear == 2020) %>% #npd's are the same for all years in the df
        group_by(discount.rate,trial) %>%
        summarise_at(.vars = c('npd','npd_wlag'), sum) %>%
        ungroup() %>%
        group_by(discount.rate) %>%
        summarise(mean_npd  = mean(npd),
                  npd_2.5      = quantile(npd, .025, na.rm = T),
                  npd_97.5     = quantile(npd, .975, na.rm = T),
                  mean_npd_wlag = mean(npd_wlag),
                  npd_2.5_wlag  = quantile(npd_wlag, .025, na.rm = T),
                  npd_97.5_wlag = quantile(npd_wlag, .975, na.rm = T),
                #median      = median(npd, na.rm = T),
                #`std. err.` = sd(npd, na.rm = T),
                #min         = min(npd, na.rm = T),
                #`0.5%`      = quantile(npd, .005, na.rm = T),
                #`1%`        = quantile(npd, .01,  na.rm = T),
                #`2.5%`      = quantile(npd, .025, na.rm = T),
                #`5%`        = quantile(npd, .05,  na.rm = T),
                #`10%`       = quantile(npd, .10,  na.rm = T),
                #`25%`       = quantile(npd, .25,  na.rm = T),
                #`75%`       = quantile(npd, .75,  na.rm = T),
                #`90%`       = quantile(npd, .90,  na.rm = T),
                #`95%`       = quantile(npd, .95,  na.rm = T),
                #`97.5%`     = quantile(npd, .975, na.rm = T),
                #`99%`       = quantile(npd, .99,  na.rm = T),
                #`99.5%`     = quantile(npd, .995, na.rm = T),
                #max         = max(npd),
                .groups = 'drop') 
  )

  #write data
  glob_means %>% 
    write_csv(file.path(Outputs,paste0('npd_global_rff_means_',MODEL,'.csv')))
}
