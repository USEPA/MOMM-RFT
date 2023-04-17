#### Purpose: To adjust the Global Ozone health effects impact generated in BenCloud####
### Created by: Melanie Jackson, IEc 
### Adapted by: E. McDuffie (EPA)
### Date Created: 3/15/2023      
### Last Edited: 4/12/2023
##      Inputs:                                                                       ##
##        -_Simulation Inputs.xlsx                                                    ##
##        -Country Results Interpolated 2020-2100 by Model.csv                        ##
##        -All Trajectory Mortality Ratios_through 2100.rds (parts 1&2)               ##
##        -All Trajectory Population Ratios_through 2100.rds (parts 1&2)              ##
##        -RFF Country GDPperCap_interpolated through 2100.rds (23 parts)             ##
##        -RFF Global GDP Per Capita_through 2100.csv                                 ##
##        -Global 2100 Population Rank Order.xlsx                                     ##
##        -sampled_pop_trajectory_numbers.csv                                         ##
##        -Discount Rates w_20yr Cessation Lags.xlsx                                  ##
##        -Country Ozone Response.csv                                                 ##
##        -BenMAP Inflation Indexed to 2015.xlsx                                      ##
##        -Discount Rates w_20yr Cessation Lags.xlsx                                  ##
##        -Final Country Grid Col_Row Index.csv                                       ##
##        -Ramsey Discounting.xlsx                                                    ##
##     Outputs:
##        - output/rft/
## Units: Code adjusts all $ values for $2020, Ramsey discounted $ is PV to 2020      ##
########################################################################################

#### Read in necessary packages, set working paths, set constant variables  ####
  #clean up space before loop
  rm(list = ls()); gc()
  #necessary packages
    packs<-c("dplyr","tidyverse","readxl","purrr","foreach","utils","pbmcapply")
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
    library(pbmcapply) #adds ETA to progress bar
    library(arrow)
  
  #working directories
    #Inputs<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Results/Reduced Form Tool/Tool Inputs/"
    #LookupTbls<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Results/Reduced Form Tool/RFF Lookup Tables/"
    #Outputs<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Results/Reduced Form Tool/Outputs/"
    #SocioEcon.GDP<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/Mortality/rffsps_v5/pop_income/"
    #EEM
    setwd("~/shared/OAR/OAP/CCD/CSIB/Methane-Ozone/MOMM-RFT")
    Inputs       <- file.path('input')
    Outputs       <- file.path('output','rft')
    
    
  #constant variables (These are the conditions run through the BenMAP WebTool)
    CloudPulse=275
    CloudMethBase=1834
    methane_mmt_to_ppbv = 1/2.75 #2.75 mT change = 1 ppb change (Prather et al., 2012 conversion factor used in IPCC AR5)
    CloudMethPulse=CloudMethBase+(CloudPulse*methane_mmt_to_ppbv) 
    MethaneLifetime=11.8 #the methane lifetime suggested is referred to as "perturbation" lifetime, the perturbation lifetime is 11.8 years according to the AR6 assessment. 
    BaseYear=2020
    gdp_2011_to_2020 = 113.784/98.164 # GDP Implicit Price Deflators (https://apps.bea.gov/iTable/?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2006&last_year=2020&scale=-99&categories=survey&thetable= )
                      #last access: March 30, 2023
    
  #ARE WE DOING DISCOUNTING IN THIS MODULE??
  #constants for Ramsey discounting
    #RamseyInput<-read_xlsx(paste0(Inputs,"Ramsey Discounting.xlsx"))
    #  names(RamseyInput)<-c("Discount Rate","1.5pct","2pct","2.5pct","3pct")
    #Rams1.5.rho<- RamseyInput$`1.5pct`[1]
    #Rams1.5.eta<-RamseyInput$`1.5pct`[2]
    #Rams2.rho<- RamseyInput$`2pct`[1] #don't need 2%
    #Rams2.eta<-RamseyInput$`2pct`[2]
    #Rams2.5.rho<- RamseyInput$`2.5pct`[1]
    #Rams2.5.eta<-RamseyInput$`2.5pct`[2]
    #Rams3.rho<- RamseyInput$`3pct`[1]
    #Rams3.eta<-RamseyInput$`3pct`[2]
    
    ## discount rates (From FrEDI NPD paper - 3/30/2023)
  #  rates = tibble(rate = c('1.5% Ramsey', '2.0% Ramsey', '2.5% Ramsey', '3.0% Ramsey', '2.0% CDR', '3.0% CDR'),
  #                 rho  = c(exp(0.000091496)-1, exp(0.001972641)-1, exp(0.004618785)-1, exp(0.007702711)-1, 0.02, 0.03), ## under discrete time, need to transform the rho that was calibrated using continuous time 
  #                 eta  = c(1.016010261, 1.244459020, 1.421158057, 1.567899395, 0, 0))
    
    
#### Read in Input Data ####
  #Lookup Tables
    
    ## O3-Methane Mortality & Responses ##
    CloudResults<-read.csv(file.path(Inputs,"BenMAP","Country Results Interpolated 2020-2100 by Model_eem.csv"))
    # Description: Timeseries of mean and 95th percentile interpolated CH4-O3 mortality 
    #  results from BenMAP for each model (from 'Summarizing Results.R')
    #OzoneResponse<-read.csv(file.path(Inputs,"Country Ozone Response.csv"))
    #  names(OzoneResponse)[1]<-"LocID"  #change cntry_cl to LocID to join tables
    OzoneResponse<-read.csv(file.path(Inputs,"BenMAP","Country Results Summary by Model & Year_eem.csv"))
      OzoneResponse <- OzoneResponse %>%
        select(c("column","COUNTRY",'Region','SuperRegion','Model','AvgResponse'))
      names(OzoneResponse)[1]<-"LocID" 
    # Description: Average O3-CH4 response (pptv O3/ppbv CH4), by country. Values 
    #  calculated in 'Summarizing Results.R', extracted & matched with country ID here
    
    ## Country Index Key ##
    countries<-read.csv(file.path(Inputs,"Final Country Grid Col_Row Index_EEM.csv"))[,c(1,3:4,6,8)]
    # Description: The row and column indices of each country (for BenMAP 0.5x0.5 grid)
      
    ## Baseline Mortality Data (for 1000 RFF scenario) ## ???
    mort.files<-list.files(file.path(Inputs,"RFF","lookup_tables"),pattern = "All Trajectory Resp Mortality Ratios_AllAges_through 2100_")
      All.mort<-lapply(file.path(file.path(Inputs,"RFF","lookup_tables"),mort.files), readRDS)
      MortRatios<-do.call(rbind,All.mort)
      names(MortRatios)[5]<-"MortRatio"  #change Ratios to MortRatio so differs from pop column name
      rm(mort.files,All.mort)
    # Description: Interpolated timeseries of ratios of respiratory mortality for 
    # all countries in 1000 RFF scenarios relative to the average respiratory mortality.
    # These data are already corrected with Int'l Futures respiratory to all-cause ratio
    #  A single file of all 1,000 scenarios for each country is too large for repo, so get a list of the
    #  input mort ratio files, read in all files, and then combine into an array
      
        
    ## Socioeconmic Data (for 1000 RFF scenarios) ###
    pop.files<-list.files(file.path(Inputs,"RFF","lookup_tables"),pattern = "All Trajectory Population Ratios_AllAges_through 2100_")
      All.pop<-lapply(file.path(file.path(Inputs,"RFF","lookup_tables"),pop.files), readRDS)
      PopRatios<-do.call(rbind,All.pop)
      names(PopRatios)[4]<-"PopRatio"  #change Ratios to PopRatio so differs from mort column name
      rm(pop.files,All.pop)
    # Description: Timeseries of the ratio of population relative to the year 2020, by country, for 
    #  1,000 RFF scenarios. A sinlge file of all 1,000 scenarios for each country is too large for repo,
    #  so get a list of the population ratio files, read in all files, and then combine into an array
    #Global2100Rank<-read.csv(file.path(Inputs,"Global 2100 Population Rank Order.csv"))
    # Description: A list of the total 2100 population in each of the 1000 RFF scenarios, and 
    #  their rank order relative to each other. This is used to calculate which of the RFF
    #  scenarios are the xth percentile, if the user wants to run the tool for a certain percentile
    SampleID<-read.csv(file.path(Inputs,"sampled_pop_trajectory_numbers.csv"))
    # Description: Cross-walk between mortality and RFF trajectory & trial #s
    
    #Add a flag for distributing deaths using cessation lag or not
    cessation_flag =1
    lags = c(6, 2.5, 2.5, 2.5, 2.5, 4/15, 4/15, 4/15, 4/15,4/15, 4/15, 4/15, 4/15, 
             4/15, 4/15, 4/15, 4/15, 4/15, 4/15, 4/15)/20
    
    #CessationLags<-read_xlsx(file.path(Inputs,"Discount Rates w_20yr Cessation Lags.xlsx"))
    #  CessationFracts<-CessationLags[1:20,1:2]
    #  CessationFracts<-as.data.frame(lapply(CessationFracts,as.numeric))
    # Description: ??
    #EEM - set as constant parameter instead
    #Inflators<-read_xlsx(paste0(Inputs,"BenMAP Inflation Indexed to 2015.xlsx"),sheet = "Data") #We should set this as a parameter instead
    #  Inf.to.2020<-Inflators$AllGoodsIndex[(Inflators$Year==2020)]/Inflators$AllGoodsIndex[(Inflators$Year==2011)]
    # GlobalGDP<-read.csv(file.path(LookupTbls,"RFF Global GDP Per Capita_through 2100.csv"))
    # # Description: ?? (GDP per capita for each GDP/Pop trajectory pair?)
    # Ctry.GDP.Files<-list.files(file.path(LookupTbls,"Split GDP Files/"),pattern = ".rds")
    #   All.Ctry.GDP<-lapply(file.path(LookupTbls,"Split GDP Files",Ctry.GDP.Files), readRDS)
    #   Ctry.GDP<-do.call(rbind,All.Ctry.GDP)
    #   rm(Ctry.GDP.Files,All.Ctry.GDP)
    #   BaseGDP<-Ctry.GDP[(Ctry.GDP$Year==BaseYear & Ctry.GDP$LocID==840),c('RunID','GDP.PerCapita')]
    # Description: Time series of GDP Per Capita for each country for each of the 1000 GDP/Pop trajectory pairs
    #   These values are used to adjust VSL in the base valuation calculations but are not
    #   used in the Ramsey discounting calculations. A single file is too large so
    #   get a list of input files, read them in, and then combine into an array
      
    #Note: we may want to change this to be able to loop through a select set of separate GDP and population scenarios
      
      
#### **Set External variables from input file ####
  #Read in Input Scenario variables and set years for run
    InputVars<-read_xlsx(file.path(Inputs,"_Simulation Inputs_eem.xlsx"),sheet="Input Variables")
    InputVars<-InputVars[!is.na(InputVars$Input),]
      
    year.var=InputVars$Value[(InputVars$Input=="Control Year")]
    #Years=ifelse(year.var=="All",list(c(2020:2100)),
    #             ifelse(year.var=="Every 5 Years",list(seq(2020,2100,5)),
    #              ifelse(year.var=="Every 10 Years",list(seq(2020,2100,10)),
    #               ifelse(year.var=="Same as Web Analysis",list(c(seq(2020,2040,5),seq(2050,2100,10))),
    #             InputVars$Value[InputVars$Input=="Control Year"]))))
    #  Years<-unlist(Years)
    Years <- c(2020:2100)
      
    Scenario=InputVars$Value[(InputVars$Input=="RFF Scenario")]
      #determine Mortality/population trajectory from the sample ID of the scenario 
        #(if a percentile is specific pull from 2100 population rank)
        #row num is run num which is scenario num
        #if running a percentile, identify percentiles and search within global 2100 rank
      if(substr(Scenario,nchar(Scenario),nchar(Scenario))=="e"){
        percentile=as.numeric(substr(Scenario,1,nchar(Scenario)-13))
        Trajectory=Global2100Rank$Trajectory[(Global2100Rank$Order==percentile*10)]
      }else{
        #if running all scenarios identify 'all' and create list of all RFF run #s (10,000)
        if(Scenario=="All"){
          Trajectory=SampleID$x
          #otherwise, for single RFF run, just search for trajectory based on runID/sampleID
        }else{
          Trajectory=SampleID$x[as.integer(Scenario)]
        }
      }
    #Trajectory <- Trajectory[1:10]
    #UsrModel = InputVars$Value[(InputVars$Input=="GCM Model")]
    
    #set up prog bar foreach loops
    num_ticks<-length(Trajectory)
    pb<-progressBar(min=0,max=length(Trajectory),style="ETA")
    
  
    #Create methane trajectory (same for all scenarios)
    
    #first calculate the methane trajectory of the custom pulse size and of the pulse that was put through BenMAP
    Pulse=as.numeric(InputVars$Value[(InputVars$Input=="Methane Emissions Pulse")])
    MethBase=as.numeric(InputVars$Value[(InputVars$Input=="Methane Baseline Concentration")])
    MethPulse=MethBase+(Pulse*methane_mmt_to_ppbv)
    MethaneProj<-data.frame(Years)
    MethaneProj$PulseMethane<-(MethPulse-MethBase)*exp((BaseYear-MethaneProj$Years)/MethaneLifetime)+MethBase
    CloudMethaneProj <-data.frame(Years)
    CloudMethaneProj$CloudMethane<-(CloudMethPulse-CloudMethBase)*exp((BaseYear-CloudMethaneProj$Years)/MethaneLifetime)+CloudMethBase
    
    #User Defined reference VSL & income elasticity
    #VSL=as.numeric(InputVars$Value[InputVars$Input=="VSL"]) #in $2011 USD, 2020 income levels
    #Elasticity=as.numeric(InputVars$Value[InputVars$Input=="Income Elasticity"])
    
    base_vsl = 9.33e6 #in 2020 $
    Elasticity = 1
    
    #do calculations for all GCMS & years
    CloudResults.yr <- CloudResults
    CloudResults.yr$LocID<-CloudResults.yr$column
    #calculate cloud standard deviation (N = 10,000 monte carlo runs; CI = 95% -> Z=1.96x2 = 3.92)
    CloudResults.yr$Std.Dev<-(CloudResults.yr$Inverse.97_5 - CloudResults.yr$Inverse.2_5)/3.92
    CloudResults.yr<-left_join(CloudResults.yr,OzoneResponse,by=c("LocID","Model"))
    
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
      
#### Begin loop for RFF Scenarios choice ####
    
    ###### Set Up Cluster ######
    ### Parallel filter and writing of feather files
    ### Detect cores and get number of cores
    parallel::detectCores()
    n.cores    <- parallel::detectCores() - 1
    #n.cores    <- 5
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
    ### Took about 3.31 hours
    
    ## start parallel
    startFile = 9703

  Results<- foreach(itrial = startFile:length(Trajectory),.combine=rbind,
                  .packages=c('purrr','dplyr','utils','pbmcapply','arrow','tidyverse')) %dopar% { #dopar takes longer to run than do
    
  ## Begin Analysis ##
    #loop through the trajectories (or scenarios)
    # Read in mortality, population, and gdp data for the given trial
    pop_gdp_file <- file.path(Inputs,"RFF","rft_inputs",paste0('rffsp_pop_gdp_',itrial,'.feather')) %>% read_feather
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
    
    #Join together the BenMAP cloud output, as well as the relevant population,
    # GDP, methane, and baseline mortality data. 
    # Because the underlying age specific background respiratory mortality data
    # were calculated elsewhere and are not published with the RFF trajectories, we
    # use the pre-processed ratios here. 
    # Bcakground mortality and population (and GDP) are inherently linked. Therefore, 
    # this RFT can only do calculations for specific RFF scenarios (not any custom 
    # socioeconomic scalar). To make this tool more flexible in the future to be 
    # able to take in other background mortality data, we would need to calculate the 
    # mean mortality data used in the cloud here (not currently done)
    Analysis <- left_join(CloudResults.yr,pop_gdp_data,by=c("LocID"="COL","ModelYear"="Year"))
    Analysis <- Analysis %>%
      left_join(MortRatios[MortRatios$Trajectory==Trajectory[itrial],],by=c("LocID","ModelYear"="Year")) %>%
      select(-c("Period","Trajectory","point_estimate","pct_2_5","pct_97_5","PointEstimate","row"))
    #Analysis$MortRatio = 1
    # There is data for more countries in the ratio files (derived from age specific data) 
    # rather than the RFF files. Therefore, use the pre-calculated population ratios
    # instead of calculating the ratio here
    Analysis <- Analysis %>%
      left_join(PopRatios[PopRatios$Trajectory==Trajectory[itrial],],by=c("LocID","ModelYear"="Year")) %>%
      select(-c('Trajectory'))
    #Analysis <- Analysis %>% 
    #  left_join(mean_pop, by=c("ModelYear"="Year","LocID")) %>%
    #  mutate(PopRatio = pop/MeanPop) %>%
    #  rename(CloudPop = MeanPop)
    #Analysis$PopRatio = 1
    #join all relevant data into dataframe for ease of calculation later
    Analysis <- Analysis %>%
      left_join(MethaneProj, by=c("ModelYear"="Years"), multiple='all') %>%
      left_join(CloudMethaneProj, by=c("ModelYear"="Years"), multiple='all')
    

    #calculate proportion Point Estimates & percentiles assuming 
      # newPE = origPE * MortRatio * PopRatio * (new Control / old Control)
      # Ratio of the methane control values captures differences in methane base, pulse, and lifetime
      #TE = resp * Pulse1
      #X  = resp * Pulse2
      #so, X = TE * Pulse2/Pulse1
    #These do not include cessation lags
    ### THIS NEEDS TO BE UPDATED TO RESP. * DELTA CH4 (NOT INCLUDING BKG)
    Analysis <- Analysis %>%
      mutate(scalar_PE  = MortRatio * PopRatio * ((AvgResponse * PulseMethane)/(AvgResponse * CloudMethane)),
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
      
      
    ### VALUATION STEP ###
      #Valuation of annual (non-discounted impacts)
      #1. Calculate reference vsl
      usa_base_income <- Analysis %>%
        filter(COUNTRY == 'United States', 
               ModelYear == 2020) %>%
        select(Model, gdp_per_cap) %>%
        rename(base_income = gdp_per_cap)%>%
        filter(Model == "MMM") #is the same across all GCMs
      
      # 2. Calculate country-specific VSL
      Analysis <- Analysis %>%
        mutate(vsl = base_vsl*((gdp_per_cap/usa_base_income$base_income)^Elasticity))
      
      # 3. calculate regional VSL & assign to countries with missing vsl data
      VSLResults.Reg <- Analysis %>%
        filter(Model == 'MMM') %>%
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
        Analysis <- Analysis[,c("Model","ModelYear","COUNTRY","LocID",'Region','SuperRegion',"pop","gdp",
                                "AvgResponse","physical_impacts","physical_impacts_2_5","physical_impacts_97_5",
                                "physical_impacts_wlag","physical_impacts_wlag_2_5","physical_impacts_wlag_97_5",
                                "annual_impacts","annual_impacts_2_5","annual_impacts_97_5",
                                "annual_impacts_wlag","annual_impacts_wlag_2_5","annual_impacts_wlag_97_5","trial")]
      } else {
        Analysis <- Analysis[,c("Model","ModelYear","COUNTRY","LocID",'Region','SuperRegion',"pop","gdp",
                                "AvgResponse","physical_impacts","physical_impacts_2_5","physical_impacts_97_5",
                                "annual_impacts","annual_impacts_2_5","annual_impacts_97_5","trial")]
      }
      
      Analysis %>% 
        write_parquet(file.path(Outputs,paste0('damages_',itrial,'_momm_rft.parquet')))
      
      
  #if(length(Trajectory)!=1){
  #  setTxtProgressBar(pb,(a-1)*length(Trajectory)+itrial)
  #}  
        
  #return(Analysis)

}#End Scenario Loop
  
  ### stop the clock\
  time2 <- Sys.time(); time2 - time1
  proc.time() - ptm
  ###### Finish #####
  ### stop cluster
  parallel::stopCluster(cl = my.cluster)

#### Post-Loop Processing ####
    
    #Do Post-Processing Elsewhere#
    
    
#  ## export results csv if running just one scenario
#   if(Scenario!="All"){
#     
#     write.csv(Results,paste0(Outputs,"Excess Respiratory Mortality_RFF ",Scenario,"_",Year,".csv"),
#               row.names=FALSE,na="")
#  }else{
# 
#  #Otherwise, process all 10,000 runs
#   ## Calculate distributional statistics include mean, 2.5th percentile, 97.5th percentile, min, and max ##
#    #summarize results by country
#     Ctry.stats<- Results %>%
#                   group_by(Column,Row,COUNTRY,`Ozone Response`,Year) %>%
#                   summarize(Mean=mean(`Excess Mortality`),
#                             Min=min(`Excess Mortality`),
#                             Max=max(`Excess Mortality`),
#                             P2_5=mean(`Pct 2_5`),
#                             P97_5=mean(`Pct 97_5`),
#                             `Valuation ($2020 Undisc)`=mean(`Valuation ($2020 Undisc)`),
#                             `Valuation ($2020;3% Disc)`=mean(`Valuation ($2020;3% Disc)`),
#                             `Valuation ($2020;7% Disc)`=mean(`Valuation ($2020;7% Disc)`),
#                             `Valuation ($2020;1.5% RamsDisc)`=mean(`Valuation ($2020;1.5% RamsDisc)`),
#                             `Valuation ($2020;2% RamsDisc)`=mean(`Valuation ($2020;2% RamsDisc)`),
#                             `Valuation ($2020;2.5% RamsDisc)`=mean(`Valuation ($2020;2.5% RamsDisc)`),
#                             `Valuation ($2020;3% RamsDisc)`=mean(`Valuation ($2020;3% RamsDisc)`),
#                             .groups="keep")
#     
#     #export
#       write.csv(Ctry.stats,paste0(Outputs,"Excess Respiratory Mortality_Summary All RFF Scenarios_Ctry_",Year,".csv"),
#                 row.names = FALSE, na="")
#       
#   #summarize results at global level
#     Global.tot<- Ctry.stats %>%
#                     group_by(Year)%>%
#                     summarize(Mean=sum(Mean),
#                               P2_5=sum(P2_5),
#                               P97_5=sum(P97_5),
#                               `Valuation ($2020 Undisc)`=sum(`Valuation ($2020 Undisc)`),
#                               `Valuation ($2020;3% Disc)`=sum(`Valuation ($2020;3% Disc)`),
#                               `Valuation ($2020;7% Disc)`=sum(`Valuation ($2020;7% Disc)`),
#                               `Valuation ($2020;1.5% RamsDisc)`=sum(`Valuation ($2020;1.5% RamsDisc)`),
#                               `Valuation ($2020;2% RamsDisc)`=sum(`Valuation ($2020;2% RamsDisc)`),
#                               `Valuation ($2020;2.5% RamsDisc)`=sum(`Valuation ($2020;2.5% RamsDisc)`),
#                               `Valuation ($2020;3% RamsDisc)`=sum(`Valuation ($2020;3% RamsDisc)`),
#                               .groups="keep")
# 
#     #export summary results
#       write.csv(Global.tot,paste0(Outputs,"Excess Respiratory Mortality_Summary All RFF Scenarios_Global_",Year,".csv"),
#               row.names = FALSE, na="")
#   }#End Post-Processing
#  print(paste0(Year," - Global and Country Summaries Exported"))
# #}#End Year Loop     
    