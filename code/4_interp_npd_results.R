## 4. Interpret Summary NPD Results

#####
## Title:  4_interp_npd_results.R
## Purpose: This file reads in global and U.S. summary NPD results (from Step #3)
##          and interpolates between calculated emission years
##          - This code includes mean NPD's without the certainty equivalence adjustment factor
## Inputs:  output/npd/npd_global_means_vsl10_NOx_1_[year].csv
##          output/npd/npd_full_streams_vsl10_NOx_1_MMM_840_[year].parquet
## Outputs: output/npd/momm_npd_stats_global_2020_2040.csv
##          output/npd/momm_npd_stats_usa_2020_2040.csv
## Written by: US EPA, Office of Atmospheric Protection; August 2023
## Last updated: 1/2/2024 by E. McDuffie
## NOTES: 
#####

library(zoo)
library(tidyverse)
library(dplyr)


years <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080)
global <- tibble()
usa    <- tibble()

#Read in data

#1. Global NPD results
# Global Net Present Damages are calculated from the sum of NPD values
# calculated for each individual country. 
# These results contain the mean associated with the mean RFF-SP scenario (pop & GDP)
# Results also include the mean when also including the 
# O3 cessation lag, which relates the time of ozone exposure with the delayed
# time of impact (e.g., to account for delayed mortality after exposure). 
# Following the latest EPA guidance, the results presented in McDuffie et al., 2023
# do not include the cessation lag and results with the cessation lag are only
# included as a sensitivity test. 
# These data also include the valuation using multiple discount rates. 
# Note that these data also include the npd's accounting for
# the certainty equivalency adjustment, but this correction is only relevant 
# if the npd values were calculated from all RFF-SP trials, not the mean scenario
for (iyear in years ) {
  temp <- read.csv(paste0('./output/npd/npd_global_means_vsl10_NOx_1_MMM_',iyear,'.csv'))
  temp <- temp %>%
    mutate(emissions.year = iyear,
           global_mean_npd = mean_npd,
           global_mean_npd_wlag = mean_npd_wlag,
           global_mean_npd_cert_eq = mean.npd.cert.eq,
           global_mean_npd_wlag_cert_eq = mean.npd.wlag.cert.eq) %>%
    select(c('emissions.year','discount.rate','global_mean_npd',
             'global_mean_npd_wlag'))#,
             #'global_mean_npd_cert_eq','global_mean_npd_wlag_cert_eq'))
  global <- rbind(global, temp)
}

#US-specific results
# NPD values calculated for the U.S. (based on US socioeconomic, health data, and 
# ozone exposure estimates)
# These data include the US-specific NPD values with and without the cessation
# lag for multiple discount rates. Are calculated from the mean of the RFF-SPs
# Note that these data also include the ability to calculate the npd's accounting for
# the certainty equivalency adjustment, but this correction is only relevant 
# if the npd values were calculated from all RFF-SP trials, not the mean scenario
for (iyear in years ) {
  temp <- read_parquet(paste0('./output/npd/npd_full_streams_vsl10_NOx_1_MMM_840_',iyear,'.parquet'))
  temp <- temp %>%
    filter(ModelYear == iyear) %>%
    mutate(emissions.year = ModelYear,
           usa_mean_npd = npd,
           usa_mean_npd_cert_eq = npd *certainty.eq.adjustment,
           usa_mean_npd_wlag = npd_wlag,
           usa_mean_npd_wlag_cert_eq = npd_wlag * certainty.eq.adjustment.wlag) %>%
    select(c('emissions.year',"discount.rate","usa_mean_npd","usa_mean_npd_wlag"))#,
             #"usa_mean_npd_cert_eq","usa_mean_npd_wlag_cert_eq"))
  usa <- rbind(usa, temp)
}


#interpret between years
# round to the nearest 100
global_interp <- global %>% 
  group_by(discount.rate) %>% 
  complete(emissions.year = seq(first(emissions.year), last(emissions.year))) %>% 
  mutate(global_mean_npd = round((na.approx((global_mean_npd))),0), #-2
         global_mean_npd_wlag = round((na.approx((global_mean_npd_wlag))),0), #-2
         region = 'global') %>%
  select(-c('global_mean_npd_wlag'))

#round to the nearest 10
usa_interp <- usa %>% 
  group_by(discount.rate) %>% 
  complete(emissions.year = seq(first(emissions.year), last(emissions.year))) %>% 
  mutate(usa_mean_npd = round((na.approx((usa_mean_npd))),0), #-1
         usa_mean_npd_wlag = round((na.approx((usa_mean_npd_wlag))),0), #-1
         region = 'usa') %>%
  select(-c('usa_mean_npd_wlag'))




#bind all region data together and write to csv
#all_region <- bind_rows(global_interp, usa_interp)

#global_interp %>% write_csv('./output/npd/MOMM_npd_stats_global_2020_2040.csv')
#usa_interp %>% write_csv('./output/npd/MOMM_npd_stats_usa_2020_2040.csv')
