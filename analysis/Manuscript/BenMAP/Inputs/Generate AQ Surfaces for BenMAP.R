################################################################################
## Title: Generate AQ Surfaces for BenMAP.R
## Purpose: Process results from 5 global models (rom UNEP/CCAC Global Methane
##          Assessment) and generate maps of O3 mixing ratios for input to BenMAP
##          associated with the baseline (Sim #1) the baseline + ozone mixing
##          ratios produced from a pulse of methane emissions. Perturbed O3 
##          mixing ratios are calculated using the O3/CH4 response from the
##          difference in O3 and CH4 in UNEP/CCAC Sims #1 and #2. Results
##          are averaged across GCMs to calculate O3 from the MMM. All O3 maps
##          are output for the full grid and for the subset of grid cells where
##          there are people. Maps are calculated for methane concentrations
##          at 11 time slices. The O3-CH4 response is held constant overtime.
##    Inputs:
##    UNEP/CCAC Global Methane Assessment Simulations for:
##      -CESM2
##      -GISS
##      -GFDL
##      -HadGEM (UK)
##      -MIROC
##    Pre-processed cross-walk of grid cells with population data
##   Outputs:
##    Full Globe & Population Subset maps for O3 mixing ratios from CCAC/UNEP Sim#1
##    and the calculated ozone mixing ratio maps associated with the custom methane pulse
##  Written by: Industrial Economics, Inc. Updated by EPA OAP
##  Last Updated: May 2023
####################################################################################

#clear workspace
rm(list = ls())

#load in necessary packages
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(patchwork)
  library(arrow)

#set working directory/paths
  Inputs <- file.path(getwd(),'analysis','Manuscript', 'BenMAP','Inputs','2022 Global Methane Assessment','csv')
  Outputs <- file.path(getwd(),'analysis','Manuscript', 'BenMAP','Inputs','AQ Surfaces')
  
  #set GCM & methane pulse variables
  Pulse       = 275 #million metric tons
  Models      <- c("CESM2","GFDL","GISS","HadGEM","MIROC") #5 UNEP/CCAC GCMs
  Simulations <- c("Sim1","Sim2") #Sim1 = base; Sim2 =50% CH4 reduction
  Season      = "SUMMER"
  Meth.base   = 1834 #ppbv
  Meth.pulse  = 1834+(Pulse/27.5*10) #27.5 mT change = 10 ppb change (from Prather et al., 2012)
  Meth.decay  = 11.8 #the methane lifetime suggested is referred to as "perturbation" lifetime, the perturbation lifetime is 11.8 years according to the AR6 assessment. 
  Model.years <- c(2020,2025,2030,2035,2040,2050,2060,2070,2080,2090,2100) #BenMAP years
  Base.year   = 2020 #pulse year

#Create a list of the input UNEP/CCAC file simulations/season data we're using
  #pattern of simulations/season
  pat<-paste0("_",Simulations,"_05x05_",Season,collapse="|")
  FileNames <- list.files(Inputs,pattern = pat)

#read in all files into an array to loop over
  Files        <- lapply(paste0(Inputs,"/",FileNames), read.csv)
  names(Files) <- FileNames
    
#calculate timeseries of perturbed Methane mixing ratios for each model year
  Methane.vols <- data.frame(Model.years)
  Methane.vols$Perturbed <-(Meth.pulse-Meth.base)*exp((Base.year-Methane.vols$Model.years)/Meth.decay)+Meth.base
  Methane.vols.t <-as.data.frame(t(Methane.vols[,-1]))
  colnames(Methane.vols.t) <- Methane.vols$Model.years

# read in each model baseline to create a MMM baseline (of O3 mixing ratios)
  #pattern of simulations/season
  base.pat      <- paste0("_Sim1_05x05_",Season,collapse="|")
  BaseFileNames <- list.files(Inputs,pattern = base.pat)
      
  #read in all files into an array 
  BaseFiles        <- lapply(paste0(Inputs,"/",BaseFileNames), read.csv)
  names(BaseFiles) <- BaseFileNames
  
  #join all of the baseline dataframes together
  All.Base <- BaseFiles %>%
          reduce(left_join,by=c("Col","Row","Metric","Seasonal.Metric","Statistic"))
  
  #calculate multi model average base value (average)
  All.Base$Fin.Values <- rowMeans(All.Base[,c(6:10)],na.rm=T)
  
  #format base for BenMAP
  MMM.Base        <- All.Base[,c(1:5,11)]
  names(MMM.Base) <- c("Column","Row","Metric","Seasonal Metric","Annual Metric","Values")
  MMM.Base$Metric <- "D8HourMax"
  MMM.Base$`Annual Metric` <- "Mean"
  #export MMM baseline O3 surface data
  write.csv(MMM.Base,paste0(Outputs,"/2_AQS Grid/Full Globe/Sim1_Ozone MMM_Baseline.csv"),row.names=F,na="")
    
  #now format the baseline O3 surfaces from all models for BenMAP
  formatted.BaseFiles <- list()
  for(b in 1:length(BaseFiles)){
    temp.baseline                <- BaseFiles[[b]]
    baseline.model               <- substr(names(BaseFiles)[b],12,nchar(names(BaseFiles)[b])-27)
    names(temp.baseline)         <- c("Column","Row","Metric","Seasonal Metric","Annual Metric","Values")
    temp.baseline$Metric         <- "D8HourMax"
    temp.baseline$`Annual Metric`<- "Mean"
    #export full globe
    write.csv(temp.baseline,paste0(Outputs,"/2_AQS Grid/Full Globe/Sim1_Ozone ",
                                   baseline.model,"_Baseline.csv"),row.names=F,na="")
    formatted.BaseFiles[[b]]     <- temp.baseline
    names(formatted.BaseFiles)[b]<- baseline.model
    rm(temp.baseline,baseline.model)
  }
      
  formatted.BaseFiles[[length(formatted.BaseFiles)+1]] <- MMM.Base
  names(formatted.BaseFiles)[length(formatted.BaseFiles)] <- "MMM"
        
# Now, calculate the O3 surfaces associated with a methane emissions pulse
# First, calculate the O3/CH4 response for each grid cell/GCM from the results of UNEP/CCAC Sims #1 & 2
# Second, calculate maps of the perturbed ozone surface (control) based on this response

  #create empty array to save the outputs
  Results      <- list()
  Mean_Results <- list()

  for (i in 1:length(Models)){
    #load Sim #1 & #2 data  
    temp.model   <- Models[i]
    temp.base    <- Files[[paste0("O3_metrics_",temp.model,"_Sim1_05x05_",Season,"_MDA8.csv")]]
    temp.control <- Files[[paste0("O3_metrics_",temp.model,"_Sim2_05x05_",Season,"_MDA8.csv")]]
    
    #combine into single file from col/row number
    temp.df      <- left_join(temp.base,
                       temp.control,
                       by=c("Col","Row","Metric","Seasonal.Metric","Statistic"),
                       suffix=c(".base",".ctrl"))
    #calculate change in ozone (unit is ppb for Ozone)
    temp.df$Delta <- temp.df$Values.base - temp.df$Values.ctrl
    #convert delta to ppt (multiply by 1000) per ppbv CH4  (CH4 diff between sim #1/2 is 556 ppbv)
    temp.df$Resp  <- (temp.df$Delta*1000)/556
    
    #Calculate the timeseries of perturbed ozone values and create surfaces
    temp.df <- merge(temp.df, Methane.vols.t)
    #calculate control values as ozone produced from perturbed methane (for each GCM)
    for(a in 1:length(Model.years)){
      temp.df[paste0(Model.years[a],".ctrl")] <-
        (temp.df[grepl(paste0('^',Model.years[a],'$'), names(temp.df))] - Meth.base)*
        (temp.df$Resp/1000) + temp.df$Values.base
      }
    
    #record mean responses for plotting later
    temp.means <- temp.df %>%
      select(where(is.numeric)) %>%
      summarise(across(everything(),mean, na.rm=TRUE))
    temp.means$Model  <- Models[i]
    Mean_Results[[i]] <- temp.means
    
    #export intermediate results for analysis later
    write_parquet(temp.df,paste0(Outputs,"/Intermediate/Intermediate Results_", temp.model,".parquet"))#,
              #row.names=FALSE,na="")
        
    #subset results & format results
    subset.cols <- c("Col","Row","Metric","Seasonal.Metric","Statistic")
    subset.cols <- append(subset.cols,paste0(Model.years,".ctrl"))
    temp.df.sub <- temp.df[,names(temp.df) %in% subset.cols]
      
    #transpose so that there is one row per col/row and year
    temp.df.sub <- temp.df.sub %>%
                      gather(Year,Control,-c(Col,Row,Metric,Seasonal.Metric,Statistic))
    names(temp.df.sub)[names(temp.df.sub)=="Control"]<-paste0("Control.",temp.model)
      
    #add the final dataframe to the empty list created prior to loop (will process all control outputs at once)
    Results[[i]] <- temp.df.sub
    names(Results)[i]<-paste0("Results_",temp.model)
        
    #print when results are complete for each model
    print(paste0("COMPLETE - ",temp.model))
        
    #remove temporary objects
    rm(temp.model,temp.base,temp.control,temp.df,temp.df.sub)
      
  }

  #Process the results of perturbed+base ozone (ppbv) from all 5 GCMs and average to calculate MMM O3
  #join all of the result dataframes together
  All.results <- Results %>%
                  reduce(left_join,by=c("Col","Row","Metric","Seasonal.Metric","Statistic","Year"))
    
  #calculate average control value
  All.results$Average <- rowMeans(All.results[names(All.results) %in% paste0("Control.",Models)],na.rm=T)
                              
  #results into dataframe for each year
  MMM.results <- split(All.results,All.results$Year)
  
  #merge all global mean results from all GCMs & save output
  All.mean.results <- bind_rows(Mean_Results, .id="column_label")
  write.csv(All.mean.results,paste0(Outputs,"/Mean_Pulse_Data.csv"),row.names=F,na="")
    

# Next, create maps of O3 (ppbv) mixing ratios from both the baseline and baseline+perturbed (=control)
#  scenarios. Also subset the global maps for only grid cells that have population data
#  Save both sets of maps for each GCM
  
  #read in GIS 5x5 grid with population for subsetting ozone surface (these are the grid indices that have ppl in them)
  AQGrids <- read.csv(file.path(Outputs,'AQ Grids with Pop.csv'))

  models2 <- c(paste0("Control.",Models),"Average") #make list of 'Control.Model' names
  
  #format each dataframe for each model
  for(f in 1:length(MMM.results)){
    temp.df   <- MMM.results[[f]]
    temp.name <- names(MMM.results)[f]
      
    #subset to only necessary columns for BenMAP
    #loop over each model to create output for each model and MMM
    for(m in 1:length(models2)){
      temp.df2 <- temp.df[,c('Col','Row','Metric','Seasonal.Metric','Statistic',models2[m])]
      #assign annual metric
      temp.df2$Statistic <- "Mean"
      temp.df2$Metric    <- "D8HourMax"
      #add final column names
      names(temp.df2)    <- c("Column","Row","Metric","Seasonal Metric","Annual Metric","Values")
      temp.name2         <- ifelse(models2[m]=="Average","MMM",substr(models2[m],9,nchar(models2[m])))
      #export final dataframe
      write.csv(temp.df2,paste0(Outputs,"/2_AQS Grid/Full Globe/Sim1_Sim2_Ozone ",temp.name2,"_",
                                substr(temp.name,1,4),"_Full Globe.csv"),row.names=F,na="")
          
      #restrict to only 5x5 grids with population (by putting the AQGrids first it will restrict to only those cells)
      temp.df.sub             <- left_join(AQGrids,temp.df2,by=c("Col"="Column","Row"))
      names(temp.df.sub)[1:2] <- c("Row","Column")
      #export
      write.csv(temp.df.sub,paste0(Outputs,"/2_AQS Grid/Subset Populated Areas/Sim1_Sim2_Ozone ",temp.name2,"_",
                                   substr(temp.name,1,4),"_Subset.csv"),row.names=F,na="")
            
      rm(temp.df2,temp.df.sub,temp.name2)
      }
      #remove temporary objects
      rm(temp.df)
  }
    
  #subset baseline O3 surfaces to only include grid cells with population data
  for(b in 1:length(formatted.BaseFiles)){
    temp.base     <- formatted.BaseFiles[[b]]
    temp.model    <- names(formatted.BaseFiles)[b]
    temp.Base.sub <- left_join(AQGrids,temp.base,by=c("Col"="Column","Row"="Row"))
    names(temp.Base.sub)[1:2] <- c("Row","Column")
    write.csv(temp.Base.sub,paste0(Outputs,"/2_AQS Grid/Subset Populated Areas/Sim1_Ozone ",
                                   temp.model,"_Baseline_Subset.csv"),row.names=F,na="") 
    rm(temp.base,temp.model,temp.Base.sub)
  }


  ## Have a nice day!