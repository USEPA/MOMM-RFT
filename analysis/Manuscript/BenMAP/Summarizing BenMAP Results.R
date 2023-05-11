#################################################################################
## Title: Summarizing BenMAP Results
## Purpose:  This script unzips and combines the 6 models and 11 years of ozone mortality
##    respiratory results from the BenMAP cloud application
##    Output from BenMAP are dataframes of respiratory-related mortality at the
##    country level, associated with the difference between O3 mixing ratios in
##    the baseline scenario compared to O3 in the perturbed methane scenario. 
## Inputs:
##  - BenMAP cloud run output
##  - COuntry name cross walk
##  - 55km population counts
## Ouputs:
##  - csv files (interpolated and not) of annual respiratory-related mortality data
##    associated with O3 from the marginal methane pulse, as well as the 
##    total respiratory-mortality counts, BenMAP confidence intervals, and the
##    population-weighted O3/CH4 response, globally and by country for each model
##    and the calculated MMM
## Written by: Industrial Economics, Inc., Updated by EPA OAP
## last updated: May 2023
## 
#################################################################################
rm(list = ls())

# Install necessary libraries
library(data.table)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(scales)
library(arrow)

## 1. set directories
  resultspath = file.path(getwd(),'analysis','Manuscript','BenMAP','Outputs')  
  cntryxwalkpath = file.path('input')
  aqspath = file.path(getwd(),'analysis','Manuscript','BenMAP','Inputs','Aq Surfaces', 'Intermediate')
  BenMapinputs = file.path(getwd(),'analysis','Manuscript','BenMAP','Inputs')
  #Plots <- file.path(getwd(), 'analysis','Manuscript','BenMAP','Ouptuts','Plots')

## 2. Load BenMAP output data
  # Creates a list of all zipped files in the input folder
  zips<- dir(file.path(resultspath,'Cloud_runs'), pattern =".zip")

  # Unzips all zipped files in the downloads folders
#  walk(zips, ~ unzip(zipfile = str_c(paste0(resultspath,'/Cloud_runs/'), .x), 
#                     overwrite = TRUE,
#                     exdir = str_c(paste0(resultspath,"/Results"))))
  ##DOUBLE CHECK THAT ALL FILE NAMES ARE CORRECT (ONE OF THE CESM2 FILES WAS NAMED CESM)

  # create a list of all .csv results in the unzipped folder
  Result.files<- dir(file.path(resultspath, 'Results'),pattern=".csv")

  # read in results files for all models and years
  Results<-list()
  
  for (ifile in 1:length(Result.files)){
      #get model and year from file name
      model <- ifelse(substr(Result.files[[ifile]],1,2)=="CE","CESM2",
                    ifelse(substr(Result.files[[ifile]],1,2)=="Ha","HadGEM",
                      ifelse(substr(Result.files[[ifile]],1,2)=="GI","GISS",
                         ifelse(substr(Result.files[[ifile]],1,2)=="GF","GFDL",
                           ifelse(substr(Result.files[[ifile]],1,2)=="MI","MIROC",
                            "MMM")))))
      
      year<-substr(Result.files[[ifile]],nchar(model)+2,nchar(model)+5)
      
      #read in file
      temp.result<-read.csv(file.path(resultspath,"Results",Result.files[[ifile]]))
    
      #add model column
      temp.result$Model<-model
      #add model year
      temp.result$ModelYear<-year

      
      #add the results list
      Results[[ifile]]<-temp.result
      names(Results)[[ifile]]<-paste0(model,"_",year)
    
  }

  #combine all results into single dataframe
  # Results are given by country (defined by the row and column numbers)
  Allresults<-do.call("rbind",Results)
  
  ## 3. Format the data (Create Global Mortality Sums & Calculate average across all models)

  # Summarize the point estimates of respiratory morality (and the 2.5th and 97.5th confidence intervals)
  # across all countries for each model and year. These correspond to the mean change in MDA8 max O3 concentrations
  # between ozone in the baseline case and in the marginal CH4 pulse case. 
  # In BenMAP, the deltas are calculated as baseline - pulse scenario, so the values are negative when there
  # is an increase in O3 in the pulse scenario. These are switched to positive values later
  GlobalSum <- Allresults %>%
               group_by(Model,ModelYear,endpoint,author,start_age,end_age)%>%
               summarize(PointEstimate = sum(point_estimate),
                        pct_2_5 = sum(pct_2_5),
                        pct_97_5 = sum(pct_97_5),
                        .groups="keep")
  GlobalSum$ModelYear <- as.integer(GlobalSum$ModelYear)
  
  #Summarize the population, baseline, and population-weighted MDA8 O3 delta across all countries 
  # for each model and year
  # (the socioeconomic are the same across all GCMs, so just pull results from MMM)
  InputSums <- Allresults[(Allresults$Model=="MMM"),] %>%
                  group_by(ModelYear,endpoint,author,start_age,end_age) %>%
                  summarize(Population = sum(population),
                            Baseline = sum(baseline),
                            PopWt_Delta = weighted.mean(delta_aq,population),
                            Avg.Baseline = mean(baseline),
                            .groups="keep")
  InputSums$ModelYear<-as.integer(InputSums$ModelYear)
    
  #calculate population-weighted MDA8 O3 delta (ppbv) for each model, year, and country
  Delta <- Allresults %>%
              group_by(Model,ModelYear,endpoint,author,start_age,end_age) %>%
              summarize(PopWt_Delta = weighted.mean(delta_aq,population),
                        .groups="keep")
  Delta$ModelYear <- as.integer(Delta$ModelYear)
    
  #calculate average MDA8 point estimate (mortality) across the 5 models for each 
  # year and country (used as comparison later to the MMM run through BenMAP)
  Avg.5models <- GlobalSum[(GlobalSum$Model!="MMM"),] %>%
                    group_by(ModelYear,endpoint,author,start_age,end_age)%>%
                    summarize(PointEstimate = mean(PointEstimate),
                              .groups="keep")
  Avg.5models$Model <- "MMM - Mean Results"
  
  #join calculated average with global sums
  GlobalSum <- rbind(GlobalSum, Avg.5models)
    
  #rename MMM model 
  GlobalSum$Model[(GlobalSum$Model=="MMM")]<-"MMM - Mean AQS"
    
  #calculate the negative of the point estimate
  # because deltas were calculated as baseline - pulse (which gives negative with O3 increase)
  GlobalSum$Inverse.PE      = -GlobalSum$PointEstimate
  GlobalSum$Inverse.Pct2_5  = -GlobalSum$pct_2_5
  GlobalSum$Inverse.Pct97_5 = -GlobalSum$pct_97_5

  
  ## 4. Plot Data
  #### create line chart by model and linear interpolation between modeled years ####

  #change model names to factors
  GlobalSum$Model <- factor(GlobalSum$Model,levels=c("CESM2","GFDL","GISS","HadGEM","MIROC",
                                                     "MMM - Mean AQS","MMM - Mean Results"))
  # #create graph of total attributable mortality
  # png(paste0(resultspath,"/Plots/Global Summary Results by Model.png"),height=2000,width=2400,res=300)
  # ggplot(GlobalSum,aes(x=ModelYear,y=Inverse.PE,colour=Model,group=Model,linetype=Model))+
  #       geom_point()+
  #       geom_line()+
  #       scale_linetype_manual(values=c(rep("solid",5),"solid","longdash"))+
  #       scale_colour_manual(values=c(brewer.pal(11,"Spectral")[c(1,4,9:11)],"black","gray"))+
  #       scale_size_manual(values=c(rep(1,5),1.5,1))+
  #       xlab("Year")+ylab("Annual Attributable Respiratory-Related Mortality (deaths)")+
  #       ggtitle('Annual Respiratory-Related Mortality Attributable to an Increase in \nMDA8 O3 Exposure from a Marginal Pulse of Methane in 2020')+
  #       theme_minimal()+
  #       scale_x_continuous(breaks=seq(2020,2100,10))
  # dev.off()
  # 
  # #create graph of baseline incidence 
  # png(paste0(resultspath,"/Plots/Total Baseline Mortality Incidence.png"),height=2000,width=2200,res=300)
  # ggplot(InputSums,aes(x=ModelYear,y=Baseline,group=1))+
  #       geom_point()+
  #       geom_line()+
  #       xlab("Year")+
  #       scale_y_continuous(name="Total Baseline Respiratory-Related Mortality",labels=comma,limits=c(0,20000000),
  #                          breaks=seq(0,20000000,by=4000000))+
  #       scale_x_continuous(breaks=seq(2020,2100,10))+
  #       ggtitle('Annual Global Baseline Respiratory Mortality')
  # dev.off()
  #       
  # #create graph of population (global)
  # png(paste0(resultspath,"/Plots/Total Population.png"),height=2000,width=2200,res=300)
  # ggplot(InputSums,aes(x=ModelYear,y=Population,group=1))+
  #   geom_point()+
  #   geom_line()+
  #   xlab("Year")+
  #   scale_y_continuous(name="Population (counts)",labels=comma,limits=c(0,12000000000),
  #                      breaks=seq(0,12000000000,by=2000000000))+
  #   scale_x_continuous(breaks=seq(2020,2100,10))+
  #   ggtitle('Total Global Population')+
  #   theme_minimal()
  # dev.off()
  # 
  # #create graph of average O3 delta (ppbv)
  # png(paste0(resultspath,"/Plots/Population Weighted Average Delta_Inverse.png"),height=2000,width=2200,res=300)
  # ggplot(Delta,aes(x=ModelYear,y=-PopWt_Delta,group=Model,colour=Model,linetype=Model))+
  #   geom_point()+
  #   geom_line()+
  #   xlab("Year")+ylab("Population Weighted Change in MDA8 Ozone [ppbv]")+
  #   scale_linetype_manual(values=c(rep("solid",5),"solid","longdash"))+
  #   scale_colour_manual(values=c(brewer.pal(11,"Spectral")[c(1,4,9:11)],"black","gray"))+
  #   scale_size_manual(values=c(rep(1,5),1.5,1))+
  #   scale_x_continuous(breaks=seq(2020,2100,10))+
  #   ggtitle('Annual Change in Population-Weighted MDA8 Ozone\n from a Pulse of Methane in 2020')+
  #   theme_minimal()
  # dev.off()
  
  
  ## 5. Calculate interpolated respiratory mortality for all years where mortality are not modeled in BenMAP
  AllResults2           <- Allresults[,c('column','row','Model','ModelYear','point_estimate','pct_2_5','pct_97_5')]
  AllResults2$ModelYear <- as.numeric(AllResults2$ModelYear)
  Interp.ByCtry         <- AllResults2 %>%
                            group_by(column,row,Model) %>%
                            complete(ModelYear=min(ModelYear):max(ModelYear))%>%
                            mutate(PointEstimate = approx(x=ModelYear,y=point_estimate,xout=2020:2100)$y,
                             pct_2_5 = approx(x=ModelYear,y=pct_2_5,xout=2020:2100)$y,
                             pct_97_5 = approx(x=ModelYear,y=pct_97_5,xout=2020:2100)$y)
  Interp.ByCtry$Inverse.PE   = - Interp.ByCtry$PointEstimate
  Interp.ByCtry$Inverse.2_5  = - Interp.ByCtry$pct_2_5
  Interp.ByCtry$Inverse.97_5 = - Interp.ByCtry$pct_97_5
  
  #sum interpolated results for each country across all years
  SumInterp.ByCtry <- Interp.ByCtry %>%
                              group_by(column,row,Model) %>%
                              summarise(TotalResults = sum(Inverse.PE),
                                        TotalPct2_5 = sum(Inverse.2_5),
                                        TotalPct97_5 = sum(Inverse.97_5),
                                        .groups="keep")
 
  ## Calculate interpolated respiratory mortality at Global level  
  GlobalSum$ModelYear <- as.numeric(GlobalSum$ModelYear)
  GlobalSum$pct_2_5[is.na(GlobalSum$pct_2_5)]   <- 0 #set nans to zero
  GlobalSum$pct_97_5[is.na(GlobalSum$pct_97_5)] <- 0
  InterpolatedResults <- GlobalSum %>%
                          group_by(Model) %>%
                          complete(ModelYear=min(ModelYear):max(ModelYear))%>%
                          mutate(PointEstimate = approx(x=ModelYear,y=PointEstimate,xout=2020:2100)$y,
                                 pct_2_5 = approx(x=ModelYear,y=pct_2_5,xout=2020:2100)$y,
                                 pct_97_5 = approx(x=ModelYear,y=pct_97_5,xout=2020:2100)$y)
  InterpolatedResults$Inverse.PE   = - InterpolatedResults$PointEstimate
  InterpolatedResults$Inverse.2_5  = - InterpolatedResults$pct_2_5
  InterpolatedResults$Inverse.97_5 = - InterpolatedResults$pct_97_5
  
  #sum interpolated results across all years
  SumInterpResults <- InterpolatedResults %>%
                      group_by(Model) %>%
                      summarise(TotalResults = sum(Inverse.PE),
                                TotalPct2_5 = sum(Inverse.2_5),
                                TotalPct97_5 = sum(Inverse.97_5),
                                .groups="keep")
  
  AllResults2$Inverse.PE = -AllResults2$point_estimate
  
  
  ## 6. Format final output data, including reading in and joining the population-weighted
  ##    O3/CH4 response (read from the BenMAP input) with the total and confidence interval
  ##    respiratory-related mortality counts, and the BenMAP mortality data for modeled 
  ##    time-slices.
  
  # read in all ozone map (0.5x0.5) data for scenarios #1 and #2 (that was input into BenMAP)
  # in order to get maps of the O3/CH4 response
  AQSFiles <- dir(aqspath, pattern=".parquet")
  Response        <- lapply(paste0(aqspath,"/",AQSFiles), read_parquet)
  #add name to list based on file name
  names(Response) <- substr(AQSFiles,22,nchar(AQSFiles)-8)
  
  #add model ID to each file  
  for(i in 1:length(Response)){
      Response[[i]]$Model <- names(Response)[i]
  }
  #combine all dataframes into single dataframe
  Response <- do.call("rbind",Response)
  Response <- Response[,c('Col','Row','Resp','Model')]
  
  #Calculate MMM for each grid cell
  MMM <- Response %>%
    group_by_at(.vars = c("Row",'Col')) %>%
    summarize_at(.vars = c("Resp"), mean,na.rm=T) %>%
    ungroup %>%
    mutate(Model = 'MMM')
   Response2 <- rbind(Response,MMM)
  
   
  #Read in pre-processed file with the population counts in each of the BenMAP (~55km x 55km) grid
  Pop      <- read.csv(paste0(BenMapinputs,'/Intermediate_fragments summary 1km and 55km population.csv'))
  Pop.55km <- unique(Pop[,c('X55km_Col','X55km_Row','Pop_55km')])
  sum(Pop.55km$Pop_55km)
  sum(Pop$Fragment_pop)
    
  #join O3/Ch4 response data with w/55km pop and calculate pop-weighted response by model
  Response3 <-left_join(Response2,Pop.55km,by=c("Col"="X55km_Col","Row"="X55km_Row"))
  #restrict to 55km grids w/pop
  Response3 <- Response3[!is.na(Response3$Pop_55km),]
  sum(Response3$Pop_55km[(Response3$Model=="CESM2")])
  #calculate population weighted average ozone response at the global level
  Avg.Response <- Response3 %>%
                    group_by(Model)%>%
                    summarize(AvgResponse=weighted.mean(Resp,Pop_55km),
                              .groups="keep")
  Avg.Response$Model[Avg.Response$Model=='MMM'] <- 'MMM - Mean Results'
        
  #join Response pop and calculate pop-weighted response, by model and country
  Response.Ctry <- left_join(Response2,Pop,by=c("Col"="X55km_Col","Row"="X55km_Row"), multiple='all')
  #restrict to 55km grids w/pop
  Response.Ctry <- Response.Ctry[!is.na(Response.Ctry$Pop_55km),]
  #calculate population weighted average ozone response
  Avg.Response.Ctry <- Response.Ctry %>%
                          group_by(Cntry_Col,Model)%>%
                          summarize(AvgResponse=weighted.mean(Resp,Fragment_pop),
                                        .groups="keep")

  ##Global results
  #####################
  FormattedResults <- spread(GlobalSum[,c('Model','ModelYear','Inverse.PE')],
                            key="ModelYear",value="Inverse.PE")
  #join ozone response and total interpolated results
  FormattedResults <- left_join(FormattedResults,Avg.Response,by="Model")
  FormattedResults <- left_join(FormattedResults,SumInterpResults,by="Model")

  #export summary table
  write.csv(FormattedResults,paste0(resultspath,"/Global Results Summary by Model & Year.csv"),row.names=F,na="")
    

  ## write global timeseries for confidence intervals too ###
  FormattedResults <- spread(GlobalSum[,c('Model','ModelYear','Inverse.Pct2_5')],
                             key="ModelYear",value="Inverse.Pct2_5")
  #join ozone response and total interpolated results
  FormattedResults <- left_join(FormattedResults,Avg.Response,by="Model")
  FormattedResults <- left_join(FormattedResults,SumInterpResults,by="Model")
  write.csv(FormattedResults,paste0(resultspath,"/Global Results Summary by Model & Year 2_5CI.csv"),row.names=F,na="")
    
  FormattedResults <- spread(GlobalSum[,c('Model','ModelYear','Inverse.Pct97_5')],
                             key="ModelYear",value="Inverse.Pct97_5")
  #join ozone response and total interpolated results
  FormattedResults <- left_join(FormattedResults,Avg.Response,by="Model")
  FormattedResults <- left_join(FormattedResults,SumInterpResults,by="Model")
  write.csv(FormattedResults,paste0(resultspath,"/Global Results Summary by Model & Year 97_5CI.csv"),row.names=F,na="")
    

 ##country results 
  ###################
  FormattedResults.Ctry <- spread(AllResults2[,c('column','row','Model','ModelYear','Inverse.PE')],
                                              key="ModelYear",value="Inverse.PE")
  #join ozone response and total interpolated results
  FormattedResults.Ctry <- left_join(FormattedResults.Ctry,Avg.Response.Ctry,
                                     by=c("column"="Cntry_Col","Model"="Model"))
  FormattedResults.Ctry <- left_join(FormattedResults.Ctry,SumInterp.ByCtry,
                                     by=c("column","Model"))    
  
  #read in country names
  Ctry.Name<-read.csv(file.path(cntryxwalkpath,'Final Country Grid Col_Row Index.csv'))
  FormattedResults.Ctry<-left_join(FormattedResults.Ctry,Ctry.Name[,c('COUNTRY','COL','Region','SuperRegion')],
                                     by=c("column"="COL"))
  FormattedResults.Ctry<-FormattedResults.Ctry[,c(1,20,21,22,3,15,17:19,4:14)]
    
  #export summary table
  write.csv(FormattedResults.Ctry,paste0(resultspath,"/Country Results Summary by Model & Year.csv"),row.names=F,na="")
  
  #export interpolated results by country with ozone response
  write.csv(Interp.ByCtry,paste0(resultspath,"/Country Results Interpolated 2020-2100 by Model.csv"),
                              row.names=FALSE,na="")

## Have a nice day!    
    