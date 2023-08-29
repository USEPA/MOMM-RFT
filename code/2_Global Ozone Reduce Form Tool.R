### Title: 2_Global Ozone Reduce Form Tool_EEM.R                                     ##
### Purpose: To adjust the Global Ozone health effects impact generated in BenCloud  ####
### Created by: Melanie Jackson, IEc 
### Adapted by: E. McDuffie (EPA)
### Date Created: 3/15/2023      
### Last Edited: 8/29/2023
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

#### Read in necessary packages, set working paths, set constant variables  ####
  #clean up space before loop
  rm(list = ls()); gc()
  
  #necessary packages
    packs<-c("dplyr","tidyverse","readxl","purrr","foreach","utils","pbmcapply","arrow")
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
  
  #working directories
    Inputs  <- file.path('input')
    Outputs <- file.path('output','rft')
    
   
########
##### Define constants 
#######
    
  #constant variables (These are the conditions run through the BenMAP WebTool)
  CloudMethPulse_Mmt  = 275   # (Mmt) original CH4 pulse size used in BenMAP simulations
  CloudMethBase_ppbv  = 1834  # (ppbv) original CH4 baseline used in BenMAP simulations
  methane_mmt_to_ppbv = 1/2.75 #ppbv/mt conversion (Prather et al., 2012 conversion factor used in IPCC AR5)
  CloudMethPulse_ppbv = CloudMethBase_ppbv+(CloudMethPulse_Mmt*methane_mmt_to_ppbv) # methane pulse (in ppbv) used in BenMAP simulations
  MethaneLifetime     = 11.8 # (years) methane perturbation lifetime  according to the AR6 assessment. 
  BaseYear            = 2020 # year of CH4 pulse used in BenMAP simulations
  PulseYear           = 2020 # year of user-defined CH4 emissions pulse
  gdp_2011_to_2020    = 113.784/98.164 # GDP Implicit Price Deflators (https://apps.bea.gov/iTable/?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2006&last_year=2020&scale=-99&categories=survey&thetable= )
                      #last access: March 30, 2023
  base_vsl            = 10.05e6 # $2020 VSL from Rennert et al., 2022 (and EPA SC-GHG report) 9.33e6  # = USD VSL in 2006$, inflated to 2020$ (from EPA 2010)
  Elasticity          = 1  # income elasticity
  Years               <- c(2020:2100) # set simulation years
  lags                = c(6, 2.5, 2.5, 2.5, 2.5, 4/15, 4/15, 4/15, 4/15,4/15, 4/15, 4/15, 4/15, 
                          4/15, 4/15, 4/15, 4/15, 4/15, 4/15, 4/15)/20
                      # These are EPA standard 20 year cessation lags (%) for particulate matter and
                      # represent the time delay between the year of exposure and distribution of attributable
                      # deaths over the next 20 years
  
########
##### Read in Original BenMAP Simulation Data ####
########
    
    ## 1) Original BenMAP O3-Methane Mortality 
    CloudResults <- read.csv(file.path(Inputs,"BenMAP","Country Results Interpolated 2020-2100 by Model.csv"))
    CloudResults$LocID  <- CloudResults$column #rename
    # Description: Timeseries of mean and 95th percentile interpolated CH4-O3 mortality 
    #  results from BenMAP for each model (from 'Summarizing Results.R')

    ## Country Index Key ##
    countries <- read.csv(file.path(Inputs,"Final Country Grid Col_Row Index.csv"))[,c(1,3:4,6,8,9)]
    # Description: The country name and LocID crosswalk (for BenMAP 0.5x0.5 grid)
      
    

########
##### Set User-Defined Scenario Inputs
#######
    
  # User-Defined Methane Pulse Data
  Usr_MethanePulse_Mmt = 275
  Usr_MethBase_ppbv    = 1834
  Usr_MethPulse_ppbv   = Usr_MethBase_ppbv+(Usr_MethanePulse_Mmt*methane_mmt_to_ppbv)
  
  # Specify Population, Mortality, GDP data
  # The tool is currently set up to use data from the RFF-SPs
  # The user is asked to specify a specific RFF-SP trajectory Number, OR
  # specify 'ALL' to calculate the mortality estimates for all 10,000 trajectories
  RFF_TrajNumber = 'mean'   #Options: numerical value 1-10000, or 'All', or 'mean'
  
  #NOx emissions
  NOx_scalar = 1          #Options: numerical values > 0 (e.g., 50% = 0.5, 150% = 1.5)
  # this value represents the proportion of NOx emissions in each country relative
  # to NOx emission in the original CCAC simulations
  # default = 1
  
  # Implement cessation lags (1 = yes, 0 = no (default=1))
  cessation_flag = 1 #if set to 1, with calculate results with and without lag
  
  #homogeneous methane-ozone response
  homogeneous = 0
  # if set to 1, the ozone response will be set to the global (non-weighted) average (4.8 pptv/ppbv)
  #default= 0
  
##########
###### Pre-Processing Steps
#########
  
  # 1) Load in cross walk between public RFF and underlying mortality trajectories
    SampleID <- read.csv(file.path(Inputs,"sampled_pop_trajectory_numbers.csv"))
    # Description: Cross-walk between mortality and RFF trajectory & trial #s
  
  # 2) Set the trajectory numbers for the user-defined scenario
    if (RFF_TrajNumber == 'All') {
      Trajectory = SampleID$x
    } else if (RFF_TrajNumber == 'mean') {
      Trajectory =1 #will trigger mean data in main loop
    } else {
      Trajectory = SampleID$x[as.integer(RFF_TrajNumber)]
    }
  
  
  # 3) Calculate delta methane timeseries for original BenMAP and user-defined projections
  #     if the user-defined pulse year is after the cloud pulse year (i.e., 2020), set pre-pulse values to zero
    Usr_MethaneProj               <- data.frame(Years)
    Usr_MethaneProj$PulseMethane  <- case_when(PulseYear > Usr_MethaneProj$Years ~ 0, 
                                               T ~ (Usr_MethPulse_ppbv-Usr_MethBase_ppbv)*exp((PulseYear-Usr_MethaneProj$Years)/MethaneLifetime)) #+Usr_MethBase_ppbv
    CloudMethaneProj              <- data.frame(Years)
    CloudMethaneProj$CloudMethane <- (CloudMethPulse_ppbv-CloudMethBase_ppbv)*exp((BaseYear-CloudMethaneProj$Years)/MethaneLifetime) #+CloudMethBase_ppbv
  
  
  # 4) Read in pre-process mortality ratios
    mort.files <- list.files(file.path(Inputs,"RFF","lookup_tables"),pattern = "All Trajectory Resp Mortality Ratios_AllAges_through 2100_")
    All.mort   <- lapply(file.path(file.path(Inputs,"RFF","lookup_tables"),mort.files), readRDS)
    MortRatios <- do.call(rbind,All.mort)
    names(MortRatios)[5]<-"MortRatio"  #change Ratios to MortRatio so differs from pop column name
    rm(mort.files,All.mort)
    # Description: Interpolated timeseries of ratios of respiratory mortality for 
    # all countries in 1000 RFF scenarios relative to the average respiratory mortality (used in BenMAP run)
    # These data are already corrected with Int'l Futures respiratory to all-cause mortality ratio
  
  # 5) Read in pre-process RFF population ratios
    pop.files <- list.files(file.path(Inputs,"RFF","lookup_tables"),pattern = "All Trajectory Population Ratios_AllAges_through 2100_")
    All.pop   <- lapply(file.path(file.path(Inputs,"RFF","lookup_tables"),pop.files), readRDS)
    PopRatios <- do.call(rbind,All.pop)
    names(PopRatios)[4]<-"PopRatio"  #change Ratios to PopRatio so differs from mort column name
    rm(pop.files,All.pop)
    # Description: Interpolated timeseries of the ratios of population for all 
    # countries in 1000 RFF scenarios relative to the average population (used in BenMAP run).
    
    
    # 6) Read in the ozone-response NOx sensitivity data (from CCAC simulations)
    response_nox_sensitivity <- read_csv(file.path(Inputs, 'CCAC',"Country_ozone_response_nox_sensitivity.csv"),
                                         col_types = cols())
    response_nox_sensitivity <- response_nox_sensitivity %>%
      #right_join(countries,by='COL') #%>%
      right_join(CloudResults, by=c('COL'='LocID','Model'),multiple='all') %>% #expand to all years & include all countries
      mutate(CloudAvgResponse = -1000*(slope * log(NOx_Mt) + intercept)/556, #re-calculate the O3 response that corresponds to original NOx levels.  
             NOx_Mt           = NOx_Mt * NOx_scalar,                         #this could also be a timeseries of scalars
             Usr_AvgResponse  = -1000*(slope * log(NOx_Mt) + intercept)/556) %>% #pptv O3 /ppbv CH4
              # O3 change = slope * ln(NOx) + intercept -->
              # O3 change per ppbv CH4 = O3 change / CCAC CH4 pulse
      select(COL,Country,Region,SuperRegion,Model,ModelYear,CloudAvgResponse,Usr_AvgResponse)
    
    #pre-process mean pop data file (the same for each RFF scenario)
    #This was used in the BenMAP WebTool (summed over all ages for each country)
    ## Not currently used ##
#    mean_pop <- read_csv(file.path(Inputs3,'rff_pop_means.csv'),col_types = cols())
#    mean_pop<-mean_pop[(mean_pop$Year<=2100),]
#    mean_pop<-mean_pop %>%
#      group_by(LocID,Year)%>%
#      summarize(meanpop=sum(mean_pop),.groups="keep") %>%
#      ungroup %>%
#      group_by(LocID) %>%
#      complete(Year=min(Year):max(Year))%>%
#      mutate(MeanPop = approx(x=Year,y=meanpop,xout=2020:2100)$y) %>%
#      mutate(MeanPop = MeanPop *1000) %>% #convert from 1000s to counts
#      select(-c("meanpop"))
      
########
##### Begin loop through Specified SocioEconomic Scenarios ####
########    
    
    ###### Set Up Cluster ######
    ### Parallel filter and writing of feather files
    ### Detect cores and get number of cores
    parallel::detectCores()
    n.cores    <- parallel::detectCores() - 10
    n.cores    <- 1
    n.cores
    
    ### Make cluster
    my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
    
    ### Register cluster to be used by %dopar%
    ### Check if cluster is registered (optional)
    ### Check how many workers are available? (optional)
    doParallel::registerDoParallel(cl = my.cluster); foreach::getDoParRegistered(); foreach::getDoParWorkers()
    
    ###### Run RFT ######
    # Start the clock!
    ptm   <- proc.time(); time1 <- Sys.time()
    
    ## start parallel (use this to specify the start file [if simulation is disrupted])
    startFile = 1

  ####
  ##Begin Analysis ##
  ####
  #loop through the trajectories (or scenarios)
  Results <- foreach(itrial = startFile:length(Trajectory),.combine=rbind,
                  .packages=c('purrr','dplyr','utils','arrow','tidyverse')) %dopar% { #dopar takes longer to run than do
    
    
    # 1) Read in mortality, population, and gdp data for the given trial
    if (RFF_TrajNumber == 'mean') {
      pop_gdp_file <- read_parquet(file.path(Inputs,"RFF","rft_inputs", 'rffsp_pop_gdp_all_trials.parquet'))
      pop_gdp_data <- pop_gdp_file %>%
        mutate(gdp_per_cap = gdp/pop) %>%
        group_by_at(.vars = c('Year','Country')) %>% #sum across trials for each country
        summarise_at(.vars = c('gdp_per_cap','pop','gdp'), mean) %>%
        ungroup() %>%
        #interpolate between 5 year intervals
        group_by(Country) %>%
        complete(Year=min(Year):max(Year))%>%
        mutate(gdp_per_cap = approx(x=Year,y=gdp_per_cap,xout=2020:2100)$y,
               pop = approx(x=Year,y=pop,xout=2020:2100)$y,
               gdp = approx(x=Year,y=gdp,xout=2020:2100)$y)
      pop_gdp_data <- right_join(countries,pop_gdp_data, by= c("RFF_iso_code"="Country"),multiple='all') %>%
        select(COL,Year,pop,gdp,gdp_per_cap)
      
    } else {
      pop_gdp_file <- file.path(Inputs,"RFF","rft_inputs",paste0('rffsp_pop_gdp_',itrial,'.feather')) %>% read_feather #generated with "1_build_inputs_for_momm_rft.R"
      pop_gdp_data <- right_join(countries,pop_gdp_file, by= c("RFF_iso_code"="Country"),multiple='all')
      pop_gdp_data <- pop_gdp_data %>% 
        select(COL,Year,pop,gdp) %>%
        mutate(gdp_per_cap = gdp/pop) %>%
        group_by_at(.vars = c('COL')) %>% 
        #interpolate between 5 year intervals
        complete(Year=min(Year):max(Year))%>%
        mutate(gdp_per_cap = approx(x=Year,y=gdp_per_cap,xout=2020:2100)$y,
              pop = approx(x=Year,y=pop,xout=2020:2100)$y,
              gdp = approx(x=Year,y=gdp,xout=2020:2100)$y)
    
    }
                    
    # 2) Join together the original BenMAP cloud output, as well as the relevant population,
    # GDP, methane, and baseline mortality data. 
    # Because the underlying age specific background respiratory mortality data
    # were calculated elsewhere and are not published with the RFF trajectories, we
    # use the pre-processed ratios here. 
    # Background mortality and population (and GDP) are inherently linked. Therefore, 
    # this RFT can only do calculations for specific RFF scenarios (not any custom 
    # socioeconomic scalar). To make this tool more flexible in the future to be 
    # able to take in other background mortality data, we would need to calculate the 
    # mean mortality data used in the cloud here (not currently done)
   Analysis <- left_join(CloudResults,pop_gdp_data,by=c("LocID"="COL","ModelYear"="Year")) %>%
        left_join(MortRatios[MortRatios$Trajectory==Trajectory[itrial],],by=c("LocID","ModelYear"="Year")) %>%
        left_join(PopRatios[PopRatios$Trajectory==Trajectory[itrial],],by=c("LocID","ModelYear"="Year")) %>%
        select(-c("Period","Trajectory.x","Trajectory.y","point_estimate","pct_2_5","pct_97_5","PointEstimate","row"))
      
   if (RFF_TrajNumber == 'mean') {
     Analysis$MortRatio = 1
     Analysis$PopRatio  = 1 #use the same mortality and population data as used in BenMAP
    }
  
   
   # join with methane delta timeseries data 
   Analysis <- Analysis %>%
      left_join(Usr_MethaneProj, by=c("ModelYear"="Years"), multiple='all') %>%
      left_join(CloudMethaneProj, by=c("ModelYear"="Years"), multiple='all')
    

  # Combine data with the updated O3 Response data
   # the country borders and averaging between BenMAP & CCAC data are slightly 
   # different for the non-MMM models, so set the User O3 response to the BenMAP
   # response to ensure that the RFT can reproduce BenMAP results with NOx scalar = 1
   Analysis <- Analysis %>% 
     left_join(response_nox_sensitivity, by = c("LocID"='COL','Model','ModelYear')) #%>%
     #select(-c('column','SuperRegion')) 
     
   if (NOx_scalar==1 ){
      Analysis <- Analysis %>%
        mutate(Usr_AvgResponse = CloudAvgResponse) 
   }
   
   if (homogeneous ==1){
     Analysis <- Analysis %>%
       mutate(Usr_AvgResponse = 3.2) 
   }
   
    #calculate the new mortality value & 95% confidence interval (concentration response function error from BenMAP) 
      # newPE = origPE * MortRatio * PopRatio * (O3 response * methane pulse / cloud O3 response * cloud methane pulse)
      # Ratio of the methane control values captures differences in methane base, pulse, and lifetime
      #TE = resp * Pulse1
      #X  = resp * Pulse2
      #so, X = TE * Pulse2/Pulse1
    #These do not include cessation lags
    Analysis <- Analysis %>%
      mutate(scalar_PE  = MortRatio * PopRatio * ((Usr_AvgResponse * PulseMethane)/(CloudAvgResponse * CloudMethane)),
             physical_impacts = Inverse.PE * scalar_PE,
             physical_impacts_2_5 = Inverse.2_5 * scalar_PE,
             physical_impacts_97_5 = Inverse.97_5 * scalar_PE)
    
    #This spreads the annual mortality data across 20 years into the future using
    # EPA standard cessation lags
    if (cessation_flag ==1) {
      ozone_mort_wide <- Analysis %>%
        select(c('LocID','Model','ModelYear','physical_impacts'))%>%
        spread(LocID, physical_impacts)
      ozone_mort_25_wide <- Analysis %>%
        select(c('LocID','Model','ModelYear','physical_impacts_2_5'))%>%
        spread(LocID, physical_impacts_2_5)
      ozone_mort_975_wide <- Analysis %>%
        select(c('LocID','Model','ModelYear','physical_impacts_97_5'))%>%
        spread(LocID, physical_impacts_97_5)
      years <- unique(Analysis$ModelYear)
      models = unique(Analysis$Model)
      countrylocs = unique(Analysis$LocID)
      
      lags_matrix <- matrix(replicate(length(countrylocs),lags),nrow=20) #make matrix of lag rates (countries  X 20 years)
      
      for (imodel in 1:length(models)){
        #temp <- ozone_mort_wide %>%
        #  group_by(Model)
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
      
      Analysis_cessation <- ozone_mort_wide %>%
        gather(LocID, physical_impacts,-c(1:2))
      temp1 <- ozone_mort_25_wide %>%
        gather(LocID, physical_impacts_2_5,-c(1:2))
      temp2 <- ozone_mort_975_wide %>%
        gather(LocID, physical_impacts_97_5,-c(1:2))
      Analysis_cessation <- cbind(Analysis_cessation, temp1$physical_impacts_2_5, temp2$physical_impacts_97_5)
      colnames(Analysis_cessation) <- c("Model",'ModelYear','LocID','physical_impacts_wlag',
                                'physical_impacts_wlag_2_5','physical_impacts_wlag_97_5')
      Analysis_cessation$LocID <- as.integer(Analysis_cessation$LocID)
      Analysis <- left_join(Analysis, Analysis_cessation, by=c("Model","ModelYear","LocID"))
    }
      
      
  ######
  ####VALUATION STEP ###
  #####
      #Valuation of annual (non-discounted impacts)
      #1. Calculate reference vsl
      usa_base_income <- Analysis %>%
        filter(LocID == 840,           #USA 
               ModelYear == PulseYear) %>%
        select(Model, gdp_per_cap) %>%
        rename(base_income = gdp_per_cap)%>%
        filter(Model == "MMM") #is the same across all GCMs
      
      # 2. Calculate country-specific VSL
      Analysis <- Analysis %>%
        mutate(vsl = base_vsl*((gdp_per_cap/usa_base_income$base_income)^Elasticity))
      
      # 3. calculate regional VSL & assign to countries with missing vsl data
      VSLResults.Reg <- Analysis %>%
        filter(Model == 'MMM') %>% #is the same across all GCMs
        group_by_at(.vars = c('Region','ModelYear')) %>%
        summarise_at(.vars= c('gdp','pop'), sum, na.rm=TRUE) %>%
        mutate(region_gdp_per_cap = gdp/pop) %>%
        #select(-c('gdp','pop')) %>%
        mutate(vsl_reg = base_vsl*((region_gdp_per_cap/usa_base_income$base_income)^Elasticity))
      
      # figure out how to not do this in a loop later
      # (assign regional vsl when there is no country vsl available)
      for( irow in 1:nrow(Analysis) ){
        regiontemp <- Analysis$Region[irow]
        yeartemp <- Analysis$ModelYear[irow]
        if (is.na(Analysis$vsl[irow])){
          #print(irow)
          Analysis$vsl[irow] = VSLResults.Reg$vsl_reg[(VSLResults.Reg$Region == regiontemp) & (VSLResults.Reg$ModelYear == yeartemp)]
          Analysis$pop[irow] = VSLResults.Reg$pop[(VSLResults.Reg$Region == regiontemp) & (VSLResults.Reg$ModelYear == yeartemp)]
          Analysis$gdp[irow] = VSLResults.Reg$gdp[(VSLResults.Reg$Region == regiontemp) & (VSLResults.Reg$ModelYear == yeartemp)]
        }
      }
      
    
      # 4. Calculate annual monetary (undiscounted damages)
      Analysis <- Analysis %>%
        mutate(annual_impacts = physical_impacts * vsl,
               annual_impacts_2_5 = physical_impacts_2_5 * vsl,
               annual_impacts_97_5 = physical_impacts_97_5 * vsl,
               trial = as.numeric(itrial))
      
      if (cessation_flag ==1) {
        Analysis <- Analysis %>%
          mutate(annual_impacts_wlag = physical_impacts_wlag * vsl,
                 annual_impacts_wlag_2_5 = physical_impacts_wlag_2_5 * vsl,
                 annual_impacts_wlag_97_5 = physical_impacts_wlag_97_5 * vsl)
      }
      
      #cleanup/format results and column names
      
      if (cessation_flag ==1) {
        Analysis <- Analysis[,c("Model","ModelYear","Country","LocID",'Region','SuperRegion',"pop","gdp",
                                "CloudAvgResponse","Usr_AvgResponse","physical_impacts","physical_impacts_2_5","physical_impacts_97_5",
                                "physical_impacts_wlag","physical_impacts_wlag_2_5","physical_impacts_wlag_97_5",
                                "annual_impacts","annual_impacts_2_5","annual_impacts_97_5",
                                "annual_impacts_wlag","annual_impacts_wlag_2_5","annual_impacts_wlag_97_5","trial")]
      } else {
        Analysis <- Analysis[,c("Model","ModelYear","Country","LocID",'Region','SuperRegion',"pop","gdp",
                                "CloudAvgResponse","Usr_AvgResponse","physical_impacts","physical_impacts_2_5","physical_impacts_97_5",
                                "annual_impacts","annual_impacts_2_5","annual_impacts_97_5","trial")]
      }
      
      if (RFF_TrajNumber == 'mean' ) {
        if (homogeneous ==1) {
          Analysis %>% 
            write_parquet(file.path(Outputs,paste0('damages_homog_mean_vsl10_NOx_',NOx_scalar,'_',PulseYear,'_momm_rft.parquet')))
        } else {
          Analysis %>%
            write_parquet(file.path(Outputs,paste0('damages_mean_vsl10_NOx_',NOx_scalar,'_',PulseYear,'_momm_rft.parquet')))
        }
      } else {
        Analysis %>% 
          write_parquet(file.path(Outputs,paste0('damages_',itrial,'_vsl10_NOx_',NOx_scalar,'_',PulseYear,'_momm_rft.parquet')))
      }
      

}#End Scenario Loop
  
  ### stop the clock\
  time2 <- Sys.time(); time2 - time1
  proc.time() - ptm
  ###### Finish #####
  ### stop cluster
  parallel::stopCluster(cl = my.cluster)

# Discounting of annual damages done in Code file #3    
    