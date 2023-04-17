#####
## Title:   1_build_inputs_for_momm_rtf.R
## Purpose: This file is meant to read in raw RFF scenarios (from Zenodo) and 
##          build complete GDP (in $2020) and national population 
##          timeseries (2010-2100) for input into the Methane Ozone Mortality Model 
##          - RFF sp population is in thousands and is converted to # of people.
##          - RFF sp is in 2011$ and is converted to 2020$
## Inputs:  inputs/RFF/pop_income/rffsp_pop_income_run_x.feather
## Outputs: inputs/RFF/rft_inputs/rffsp_pop_gdp_i.feather
##          inputs/RFF/rft_inputs/rffsp_pop_gdp_all_trials.parquet
## Written by: US EPA, Climate Change Division; March 2023
## Last updated: 3/31/23 by E. McDuffie
#####


##########################
#################  library
##########################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed and, if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'readxl', 'arrow',
                      'foreach','doParallel','pbapply')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

## turn off scientific notation
options(scipen = 9999999)

##########################
##############  data paths
##########################
setwd("~/shared/OAR/OAP/CCD/CSIB/Methane-Ozone/MOMM-RFT")
inputpath = file.path('input','RFF','pop_income')
momm_rft_input_path = file.path('input','RFF','rft_inputs')

##########################
####################  data
##########################

### Constants
gdp_2011_to_2020 = 113.784/98.164 # GDP Implicit Price Deflators (https://apps.bea.gov/iTable/?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2006&last_year=2020&scale=-99&categories=survey&thetable= )
#last access: March 30, 2023

###

## read rffsps
#collect file names
##### Collect Files ####
c_iteration  <- inputpath %>% list.files(pattern = "\\.feather") %>%
  (function(x){sub("\\.feather", "", x)}) %>%
  (function(x){sub("rffsp_pop_income_run_", "", x)}) %>%
  as.numeric %>% sort; 
c_iteration %>% length
#c_iteration = c(1,2)


rffsp <- 
  pblapply(1:length(c_iteration), function(i){
    ### File name
    infile_i  <- inputpath %>%
      file.path("rffsp_pop_income_run") %>%
      paste(c_iteration[i], sep="_") %>%
      paste0(".", "feather")
    ### Read in data and return
    data_i    <- infile_i %>% read_feather
    ### Filter data for model type, national total, desired sectors, baseline scenario
    data_i    <- data_i   %>% 
      filter(Year <= 2100) %>% #filter for pre-2100
      mutate(gdp = GDP * gdp_2011_to_2020 * 1e6) %>% #RFF data is in millions of 2011$, convert to 2020$
      mutate(pop = Pop * 1e3) %>%                    #RFF population data is in 1000s, convert to individual ppl
      mutate(dollar.year = 2020) %>%
      select(-c('GDP','Pop')) %>%
      mutate(trial = i) %>%
      #interpolate for years between 5-year bins  (do interpolation in tool to save storage space)
      #group_by(Country) %>%
      #complete(Year=min(Year):max(Year))%>%
      #mutate(pop = approx(x=Year,y=pop,xout=2020:2100)$y,
      #       gdp = approx(x=Year,y=gdp,xout=2020:2100)$y)
    
    ## export scenario-specific population files to read into the RFT
    write_feather(momm_rft_input_path %>% file.path(paste0('rffsp_pop_gdp_', i, '.feather')))
  })  %>%
  
  #also bind all together
  (function(x){
    do.call(rbind, x)
  }); rffsp %>% glimpse

### Save file
  rffsp %>%
    write_parquet(momm_rft_input_path %>% file.path("rffsp_pop_gdp_all_trials.parquet"))
  


