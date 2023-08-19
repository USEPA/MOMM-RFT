##################################################################
## Title:   BenMAP_O3Mort_Valuation.R
## Purpose: This file reads in O3 Mortality results from BenMAP
##          and calculates the annual damages$ and 
##          calculates the NPD value ($ per ton CO2) using 
##          1.5%, 2%, 2.5%, and 3% Ramsey discounting and a 2% and 3% constant discount rate
##          by 2100 in 2020 USD.
##          Option to implement cessation lags or not
## Inputs:  Final Country Grid Col_Row Index.csv
##          rffsp_pop_gdp_all_trials.parquet
##          Country Results Interpolated 2020-2100 by Model.csv
## Outputs: Country Results Interpolated Mortality Annual Undiscounted Damages_w[out]Cessation.csv
##          npd_damage_transformation_full_streams_w[out]Cessation.parquet
##          npd_global_damage_transformation_w[out]Cessation.csv
## Written by: US EPA, Climate Change Division (OAP); March 2023
## Last updated: 8.17.23 by E. McDuffie
## NOTES: 
#####################################################################

##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages = c('tidyverse', 'magrittr',
                     'arrow',
                     'zoo',
                     'doParallel', 'foreach',
                     'ggplot2')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
################### constants
##########################


Inputs_mort     <- file.path('analysis','Manuscript','BenMAP','Outputs')
Inputs_socio    <- file.path('input','RFF') #need mean population that was used in BenMAP, as well as the corresponding GDP scenario
cntryxwalkpath  <- file.path('input')
Results         <- file.path('analysis','Manuscript','BenMAP','Outputs')
  
#### Constants
file_name_flag  <- 'vsl10'  #custom file name string ending
pulse           = 275e6     #original methane pulse was 275 million metric tonnes
base_vsl        = 10.05e6   # US $2020 VSL (from Rennert et al., 2022/EPA SC-GHG Report) 9.33e6    #us vsl in 2020$ 
# from USEPA 2006 value in 2010 guidance (default = 1)
Elasticity      = 1         #default = 1
# turn 20-year cessation flag on (=1) or off (=0) (default = 0)
cessation_flag  = 0         #default =0

### country name list ###
Ctry.Name<-read.csv(file.path(cntryxwalkpath,'Final Country Grid Col_Row Index.csv'))

if (cessation_flag ==1) {
  ### Define standard 20 year cessation lags ###
  #each value is the fraction of deaths from current year that will occur each year over the next 20 years
  lags = c(6, 2.5, 2.5, 2.5, 2.5, 4/15, 4/15, 4/15, 4/15,4/15, 4/15, 4/15, 4/15, 
           4/15, 4/15, 4/15, 4/15, 4/15, 4/15, 4/15)/20
}

###########
##### BenMAP MORTALITY DATA & PROCESSING #####
##########
ozone_mort <- read_csv(file.path(Inputs_mort, 'Country Results Interpolated 2020-2100 by Model.csv'),
                       col_types = cols())

#implement cessation lags
if (cessation_flag ==1) {
  ozone_mort_wide <- ozone_mort %>%
    select(c('column','Model','ModelYear','Inverse.PE'))%>%
    spread(column, Inverse.PE)
  ozone_mort_25_wide <- ozone_mort %>%
    select(c('column','Model','ModelYear','Inverse.2_5'))%>%
    spread(column, Inverse.2_5)
  ozone_mort_975_wide <- ozone_mort %>%
    select(c('column','Model','ModelYear','Inverse.97_5'))%>%
    spread(column, Inverse.97_5)
  years <- unique(ozone_mort$ModelYear)
  models = unique(ozone_mort$Model)
  countries = unique(ozone_mort$column)
  
  lags_matrix <- matrix(replicate(length(countries),lags),nrow=20) #make matrix of lag rates (countries  X 20 years)
  
  for (imodel in 1:length(models)){
    temp <- ozone_mort_wide[ozone_mort_wide$Model == models[imodel],]
    temp_cessation <- temp
    temp_cessation[,-c(1:2)] <- 0
    temp_25 <- ozone_mort_25_wide[ozone_mort_25_wide$Model == models[imodel],]
    temp_25_cessation <- temp_25
    temp_25_cessation[,-c(1:2)] <- 0
    temp_975 <- ozone_mort_975_wide[ozone_mort_975_wide$Model == models[imodel],]
    temp_975_cessation <- temp_975
    temp_975_cessation[,-c(1:2)] <- 0
    for (iyear in 1:length(years)){
      #print(years[iyear])
      v1 <- t(as.numeric(temp[temp$ModelYear == years[iyear],-c(1:2)])*t(lags_matrix))
      v1_2.5 <- t(as.numeric(temp_25[temp_25$ModelYear == years[iyear],-c(1:2)])*t(lags_matrix))
      v1_97.5 <- t(as.numeric(temp_975[temp_975$ModelYear == years[iyear],-c(1:2)])*t(lags_matrix))
      
      if (iyear+20 <= length(years)){
        temp_cessation[temp$ModelYear >= years[iyear] & temp$ModelYear < years[iyear+20],-c(1:2)]  <- 
          temp_cessation[temp$ModelYear >= years[iyear] & temp$ModelYear < years[iyear+20],-c(1:2)]+v1
        temp_25_cessation[temp_25$ModelYear >= years[iyear] & temp_25$ModelYear < years[iyear+20],-c(1:2)]  <- 
          temp_25_cessation[temp_25$ModelYear >= years[iyear] & temp_25$ModelYear < years[iyear+20],-c(1:2)]+v1_2.5
        temp_975_cessation[temp_975$ModelYear >= years[iyear] & temp_975$ModelYear < years[iyear+20],-c(1:2)]  <- 
          temp_975_cessation[temp_975$ModelYear >= years[iyear] & temp_975$ModelYear < years[iyear+20],-c(1:2)]+v1_97.5
      } else {
        temp_cessation[temp$ModelYear >= years[iyear],-c(1:2)]  <- 
          temp_cessation[temp$ModelYear >= years[iyear],-c(1:2)]+v1[1:(length(years)-iyear+1),]
        temp_25_cessation[temp_25$ModelYear >= years[iyear],-c(1:2)]  <- 
          temp_25_cessation[temp_25$ModelYear >= years[iyear],-c(1:2)]+v1[1:(length(years)-iyear+1),]
        temp_975_cessation[temp_975$ModelYear >= years[iyear],-c(1:2)]  <- 
          temp_975_cessation[temp_975$ModelYear >= years[iyear],-c(1:2)]+v1[1:(length(years)-iyear+1),]
      }
      ozone_mort_wide[ozone_mort_wide$Model == models[imodel],]  <- temp_cessation
      ozone_mort_25_wide[ozone_mort_25_wide$Model == models[imodel],]  <- temp_25_cessation
      ozone_mort_975_wide[ozone_mort_975_wide$Model == models[imodel],]  <- temp_975_cessation
      
    }
  }
  
  ozone_mort <- ozone_mort_wide %>%
    gather(column, Inverse.PE,-c(1:2))
  temp1 <- ozone_mort_25_wide %>%
    gather(column, Inverse.2_5,-c(1:2))
  temp2 <- ozone_mort_975_wide %>%
    gather(column, Inverse.97_5,-c(1:2))
  ozone_mort <- cbind(ozone_mort, temp1$Inverse.2_5, temp2$Inverse.97_5)
  colnames(ozone_mort) <- c("Model",'ModelYear','column','Inverse.PE','Inverse.2_5','Inverse.97_5')
  ozone_mort$column <- as.integer(ozone_mort$column)
}
# Combine mortality results with country names
MortResults.Ctry <-  left_join(ozone_mort,Ctry.Name[,c('COL','Region','COUNTRY','SuperRegion',"RFF_iso_code")],
                               by=c("column"="COL")) %>%
  ungroup 


##########
##### SOCIOECONOMICS & VSL CALCULATION #####
#########
#####  Read In Socioeconomic data (calculate mean GDP per capita for each country ) ###
rffsp <- read_parquet(file.path(Inputs_socio,'rft_inputs', 'rffsp_pop_gdp_all_trials.parquet'))

rffsp_mean_gdp_per_cap <- rffsp %>%
  mutate(gdp_per_cap = gdp/pop) %>%
  select(-c('dollar.year')) %>%
  group_by_at(.vars = c('Year','Country')) %>% #sum across trials for each country
  summarise_at(.vars = c('gdp_per_cap','pop','gdp'), mean) %>%
  ungroup() %>%
  #interpolate between 5 year intervals
  group_by(Country) %>%
  complete(Year=min(Year):max(Year))%>%
  mutate(gdp_per_cap = approx(x=Year,y=gdp_per_cap,xout=2020:2100)$y,
         pop = approx(x=Year,y=pop,xout=2020:2100)$y,
         gdp = approx(x=Year,y=gdp,xout=2020:2100)$y)

#Calculate country level VSL
usa_base_income <- rffsp_mean_gdp_per_cap %>%
  filter(Country == 'USA', 
         Year == 2020)

country_vsl <- rffsp_mean_gdp_per_cap %>%
  mutate(vsl = base_vsl*((gdp_per_cap/usa_base_income$gdp_per_cap)^Elasticity))

#Join country VSL data with country name  
VSLResults.Ctry<-left_join(country_vsl,Ctry.Name[,c('COL','Region','COUNTRY','SuperRegion',"RFF_iso_code")],
                           by=c("Country"="RFF_iso_code")) %>%
  ungroup %>%
  select(-c('gdp_per_cap','Country'))

#Calculate the VSL for each region
VSLResults.Reg <- VSLResults.Ctry %>%
  group_by_at(.vars = c('Region','Year')) %>%
  summarise_at(.vars= c('gdp','pop'), sum) %>%
  mutate(region_gdp_per_cap = gdp/pop) %>%
  mutate(vsl_reg = base_vsl*((region_gdp_per_cap/usa_base_income$gdp_per_cap)^Elasticity))

#combine the country and regional VSL dataframe
VSLResultsCombined <- left_join(VSLResults.Ctry, VSLResults.Reg, 
                                by=c('Region','Year')) %>%
  ungroup() %>%
  select(-c('region_gdp_per_cap'))



#join the VSL dataframe with the mortality dataframe
CombinedResults.Ctry <- left_join(MortResults.Ctry, VSLResultsCombined,
                                  by=c("column"='COL','ModelYear'='Year')) %>%
  ungroup() %>%
  mutate(vsl_calc = vsl)

#Where a country does not have a VSL (e.g., where there was no public RFF GDP data), assign the regional VSL
#figure out how to not do this in a loop later
for( i in 1:nrow(CombinedResults.Ctry) ){
  regiontemp <- CombinedResults.Ctry$Region.x[i]
  yeartemp <- CombinedResults.Ctry$ModelYear[i]
  if (is.na(CombinedResults.Ctry$vsl[i])){
    #print('here')
    CombinedResults.Ctry$vsl_calc[i] = VSLResults.Reg$vsl_reg[(VSLResults.Reg$Region == regiontemp) & (VSLResults.Reg$Year == yeartemp)]
    CombinedResults.Ctry$pop.x[i] = VSLResults.Reg$pop[(VSLResults.Reg$Region == regiontemp) & (VSLResults.Reg$Year == yeartemp)]
    CombinedResults.Ctry$gdp.x[i] = VSLResults.Reg$gdp[(VSLResults.Reg$Region == regiontemp) & (VSLResults.Reg$Year == yeartemp)]
    
  }
}

#Calculate annual damages as mortality * VSL for each country
FinalResults.Ctry <- CombinedResults.Ctry %>%
  mutate(damages      = Inverse.PE * vsl_calc,
         damages_2_5  = Inverse.2_5 * vsl_calc,
         damages_97_5 = Inverse.97_5 * vsl_calc) %>%
  select(-c('Inverse.PE','Inverse.2_5','Inverse.97_5',
            'RFF_iso_code','Region.y','SuperRegion.y','COUNTRY.y',
            'gdp.y','pop.y'))%>%
  rename(Region = Region.x,
         Country = COUNTRY.x,
         SuperRegion = SuperRegion.x,
         LocID = column,
         vsl_cntry = vsl,
         gdp = gdp.x,
         pop=pop.x)

#write data
if (cessation_flag ==1 ) {
  FinalResults.Ctry %>%
    write_csv(Results %>% file.path(paste0("Country Results Interpolated Mortality Annual Undiscounted Damages_wCessation_",file_name_flag,".csv")))
} else {
  FinalResults.Ctry %>%
    write_csv(Results %>% file.path(paste0("Country Results Interpolated Mortality Annual Undiscounted Damages_woutCessation_",file_name_flag,".csv")))
}


##########
##### DISCOUNTING ##### 
##########

global_ypc <- FinalResults.Ctry %>%
  group_by(Model,ModelYear) %>%
  summarise_at(.vars = c('damages','gdp','pop'),sum) %>%
  mutate(damages.pct = 1 - (1/(1+(damages/gdp))),
         glob_ypc         = ((1-damages.pct) * gdp)/pop,
         glob_base.ypc    = case_when(ModelYear == 2020 ~ glob_ypc, T ~ 0),
         glob_base.ypc    = max(glob_base.ypc)
  )


#damages are calculated as a function of GDP (growth is corrected for damages)
damages  <- FinalResults.Ctry %>%
  group_by(Country, Model) %>% 
  mutate(emissions.year = 2020) %>%
  mutate(damages.pct            = 1 - (1/(1+(damages/gdp))),
         damages.pct.2_5        = 1 - (1/(1+(damages_2_5/gdp))),
         damages.pct.97_5       = 1 - (1/(1+(damages_97_5/gdp))),
         ypc                    = ((1-damages.pct) * gdp)/pop,
         base.ypc               = case_when(ModelYear == emissions.year ~ ypc, T ~ 0),
         base.ypc               = max(base.ypc),
         gdp_percap             = ((1-damages.pct) * gdp)/pop,
         damages.marginal       = (gdp * damages.pct) / pulse, #convert into damages per ton CH4 (divide by pulse size)
         damages.marginal.2_5   = (gdp * damages.pct.2_5) / pulse,
         damages.marginal.97_5  = (gdp * damages.pct.97_5) / pulse) %>% 
  ungroup()

damages2 <- left_join(damages, global_ypc, by = c("Model","ModelYear"))

## discount rates
rates = tibble(rate = c('1.5% Ramsey', '2.0% Ramsey', '2.5% Ramsey', '3.0% Ramsey', '2.0% CDR', '3.0% CDR'),
               rho  = c(exp(0.000091496)-1, exp(0.001972641)-1, exp(0.004618785)-1, exp(0.007702711)-1, 0.02, 0.03), ## under discrete time (or stochastic rate), need to transform the rho that was calibrated using continuous time 
               #rho = c(0.000091496, 0.001972641, 0.004618785, 0.007702711, 0.02, 0.03),
               eta  = c(1.016010261, 1.244459020, 1.421158057, 1.567899395, 0, 0))

## object to store data
data = tibble()


for (RATE in 1:length(rates$rate)){
  
  # ## test 
  # RATE = 5
  
  ## get damage parameters
  rate = rates$rate[[RATE]]
  rho  = rates$rho[[RATE]]
  eta  = rates$eta[[RATE]]
  
  ## get streams of discounted damages and net present damages
  # Using stochastic Ramsey discount rate expression from Rennert et al., 2022
  data = 
    bind_rows(
      data,
      damages2 %>%
        group_by(Country, Model) %>%
        mutate(discount.rate                    = rate,
               discount.factor                  = case_when(grepl("Ramsey", rate) ~ (base.ypc/ypc)^eta/(1+rho)^(ModelYear-emissions.year),
                                                            T ~ 1/(1+rho)^(ModelYear-emissions.year)),
               damages.marginal.discounted      = damages.marginal * discount.factor,
               npd                              = sum(damages.marginal.discounted, na.rm = F),
               damages.marginal.2_5.discounted  = case_when(is.na(discount.factor) ~ damages.marginal.2_5,
                                                            TRUE ~ damages.marginal.2_5 * discount.factor),
               npd.2_5                          = sum(damages.marginal.2_5.discounted, na.rm = F),
               damages.marginal.97_5.discounted = case_when(is.na(discount.factor) ~ damages.marginal.97_5,
                                                            TRUE ~ damages.marginal.97_5 * discount.factor),
               npd.97_5                         = sum(damages.marginal.97_5.discounted, na.rm = F)) %>%
        ungroup()
    )
}

## export full streams
if (cessation_flag ==1 ) {
  data %>% 
    write_parquet(paste0(Results,'/npd_damage_transformation_full_streams_wCessation_',file_name_flag,'.parquet'))
} else {
  data %>% 
    write_parquet(paste0(Results,'/npd_damage_transformation_full_streams_woutCessation_',file_name_flag,'.parquet'))
}

## object to store data
means = tibble()

means = 
  bind_rows(
    means, 
    data %>%
      filter(ModelYear == 2020) %>% #same npd for each row
      group_by(Model,Country, SuperRegion,discount.rate) %>% #consolidate data for each country
      summarise(mean        = sum(npd, na.rm=T),
                `2.5%`      = sum(npd.2_5, na.rm = T),#these are BenMAP CI's
                `97.5%`     = sum(npd.97_5, na.rm = T),
                .groups = 'drop') 
  )

#write data
if (cessation_flag ==1 ) {
  means %>% 
    write_csv(paste0(Results,'/npd_country_damage_transformation_wCessation_',file_name_flag,'.csv'))
} else {
  means %>% 
    write_csv(paste0(Results,'/npd_country_damage_transformation_woutCessation_',file_name_flag,'.csv'))
}

#export global stats
glob_means = tibble()
#data = unique(data) #get unique rows (filter out duplicate years)

glob_means = 
  bind_rows(
    glob_means, 
    data %>%
      filter(ModelYear == 2020) %>% #same npd for each row
      group_by(Model, discount.rate) %>% #sum across all countries
      summarise(mean        = sum(npd, na.rm=T),
                `2.5%`      = sum(npd.2_5, na.rm = T),#these are BenMAP CI's
                `97.5%`     = sum(npd.97_5, na.rm = T),
                .groups = 'drop') 
  )

#write data
if (cessation_flag ==1 ) {
  glob_means %>% 
    write_csv(paste0(Results,'/npd_global_damage_transformation_wCessation_',file_name_flag,'.csv'))
} else {
  glob_means %>% 
    write_csv(paste0(Results,'/npd_global_damage_transformation_woutCessation_',file_name_flag,'.csv'))
}


