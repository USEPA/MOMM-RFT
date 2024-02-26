### Title: 1_build_parts_for_GIVE_rft.R                                               ##
### Purpose: Create rft inputs for GIVE model version of the rft                      ####
### Created by: E. McDuffie (EPA) 
### Date Created: 2/25/2024      
### Last Edited: 2/25/2024
##      Inputs:                                                                       ##
##        -Country Results Interpolated 2020-2100 by Model.csv                        ##
##        -All Trajectory Mortality Ratios_through 2100.rds (parts 1&2)               ##
##        -All Trajectory Population Ratios_through 2100.rds (parts 1&2)              ##
##        -sampled_pop_trajectory_numbers.csv                                         ##
##        -Final Country Grid Col_Row Index.csv                                       ##
##     Outputs:
##        - output/rft/
## Units: Code adjusts all $ values for $2020                                         ##
########################################################################################

Inputs  <- file.path('input')


## 1) Original BenMAP O3-Methane Mortality Results
CloudResults <- read.csv(file.path(Inputs,"BenMAP","Country Results Interpolated 2020-2100 by Model.csv"))
CloudResults$LocID  <- CloudResults$column #rename
# Description: Timeseries of mean and 95th percentile interpolated CH4-O3 mortality 
#  results from BenMAP for each model (from 'Summarizing Results.R')
# Extend to 2300 (keep 2100 values constant)
# Note that mortality effects will not be constant past 2100, these 2100 value will just be used to ratio
# the actual methane and ozone concentrations against later on in the rft calculations
CloudResults <- CloudResults %>% 
  group_by(LocID,Model) %>%
  complete(ModelYear=min(ModelYear):endYear) %>%
  mutate(Inverse.PE = approx(x=ModelYear,y=Inverse.PE,xout=2020:endYear, rule =2)$y,
         Inverse.2_5 = approx(x=ModelYear,y=Inverse.2_5,xout=2020:endYear, rule=2)$y,
         Inverse.97_5 = approx(x=ModelYear,y=Inverse.97_5,xout=2020:endYear, rule=2)$y)%>%
  ungroup()

#Add assocaited methane concentration data, then save
#the cloud run was only run through 2100, so hold constant after 2100
#constant variables (These are the conditions run through the BenMAP WebTool)
CloudMethPulse_Mmt  = 275   # (Mmt) original CH4 pulse size used in BenMAP simulations
CloudMethBase_ppbv  = 1834  # (ppbv) original CH4 baseline used in BenMAP simulations
methane_mmt_to_ppbv = 1/2.75 #ppbv/mt conversion (Prather et al., 2012 conversion factor used in IPCC AR5)
CloudMethPulse_ppbv = CloudMethBase_ppbv+(CloudMethPulse_Mmt*methane_mmt_to_ppbv) # methane pulse (in ppbv) used in BenMAP simulations
MethaneLifetime     = 11.8 # (years) methane perturbation lifetime  according to the AR6 assessment. 
BaseYear            = 2020 # year of CH4 pulse used in BenMAP simulations
CloudMethaneProj              <- data.frame(Years=c(2020:2100))
CloudMethaneProj$CloudMethane <- (CloudMethPulse_ppbv-CloudMethBase_ppbv)*exp((BaseYear-CloudMethaneProj$Years)/MethaneLifetime) #+CloudMethBase_ppbv
CloudMethaneProj <- CloudMethaneProj %>% 
  complete(Years=min(Years):endYear)%>%
  mutate(CloudMethane = approx(x=Years,y=CloudMethane,xout=2020:endYear, rule =2)$y)

Combined <- left_join(CloudResults,CloudMethaneProj,by=c("ModelYear"="Years"))
Combined %>% write.csv(file.path(Inputs, "GIVE_rft","BenMAP Country Mortality Results Interpolated 2020-2300 by Model.csv"))


# 4) Read in pre-process mortality ratios
mort.files <- list.files(file.path(Inputs,"RFF","lookup_tables"),pattern = "All Trajectory Resp Mortality Ratios_AllAges_through 2100_")
All.mort   <- lapply(file.path(file.path(Inputs,"RFF","lookup_tables"),mort.files), readRDS)
MortRatios <- do.call(rbind,All.mort)
names(MortRatios)[5]<-"MortRatio"  #change Ratios to MortRatio so differs from pop column name
rm(mort.files,All.mort)
# Description: Interpolated timeseries of ratios of respiratory mortality for 
# all countries in 1000 RFF scenarios relative to the average respiratory mortality (used in BenMAP run)
# These data are already corrected with Int'l Futures respiratory to all-cause mortality ratio
MortRatios %>% write.csv(file.path(Inputs, "GIVE_rft","All Teajectory Resp Mortality Ratios AllAges 2020-2100.csv"))
