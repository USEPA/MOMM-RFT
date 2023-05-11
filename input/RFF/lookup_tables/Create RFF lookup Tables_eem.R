#### Purpose: To create a look-up table of the RFF scenarios              ####
##      Look-up table will contain the ratio of mortality/population      ##
##          for each scenario to the mean mortality/population values     ##
##      Ratios will be used to scale to the mean scenario point estimates ##
############################################################################

#### Load necessary packages ####
  library(dplyr)
  library(data.table)
  library(readxl)
  library(tidyverse)
  library(zoo)
  library(arrow)
  library(progress)

#### Set working directory and read in inputs ####
  #Working Directories
    inpath<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/rff_data/new_dat/"
    outpath<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Results/Reduced Form Tool/RFF Lookup Tables/"
    input <- file.path('Code','InputData','Creating RFF Lookup Tables')
    input2 <- file.path('Code','InputData','RFF Lookup Tables')
    
  #input data (takes a few minutes to load, these files are large)
    mx<-read.csv(paste0(input,"/mx_trajectories.csv"))
    pop<-read.csv(paste0(input,"/pop_trajectories.csv"))
    #subset for years <=2100
      #must crosswalk mortality year ranges to years first
        Cross.year<-read_xlsx("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/Mortality/Crosswalk RFF Mort Year.xlsx")
        mx<-left_join(mx,Cross.year,by=c("Year"="RFF year"))
      mx<-mx[(mx$Min.Year<2100),]
      pop<-pop[(pop$Year<=2100),]
      #export files as rds so easier to load in other instances
        saveRDS(mx,file=paste0(outpath,"All Mortality Data_through 2100.rds"))
        saveRDS(pop,file=paste0(outpath,"All Population Data_through 2100.rds"))
  #IFs resp adjustments - requires crosswalking IFs country ID to RFF country ID
      IFsFolder<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/IFs_data/"
      IFsFolder <- file.path('Code',"InputData","Creating RFF Lookup Tables")
    IFsFactors<-read.csv(paste0(IFsFolder,"adjustment_factors.csv"))  
      IFsCountry<-read.csv(paste0(IFsFolder,"country_codes.csv"))
      IFsCtryCross<-read.csv(paste0(IFsFolder,"country_crosswalk.csv"))
    IFsFactors<-left_join(IFsFactors,IFsCountry,by=c("Region"="RegionNum"))
    IFsFactors<-left_join(IFsFactors,IFsCtryCross,by=c("?..RegionName"="IFsName"))
      names(IFsFactors)<-c("X","Years","Region","RFF_Age_range","final_AF","RegionName","LocID","RFFName","Notes")
      IFsFactors<-IFsFactors[,2:8]
      
#### Summarize Global 2100 Population ####
## Use the 2100 Global Population totals to order the simulation #'s by pop size ##
  #2100 global pop sums
  Global.Sum2100<- pop[(pop$Year == 2100),] %>%
                      group_by(Trajectory) %>%
                      summarize(Global_Pop = sum(Pop),
                                .groups="keep")
  #add order #'s for trajectories by total pop
    Global.Sum2100$Order<-rank(Global.Sum2100$Global_Pop)
  #identify 97.5th percentile and 2.5th percentile trajectories (1000 trajectories so it is 975th and 25th)
    Percentile975.Sum2100<-Global.Sum2100$Trajectory[(Global.Sum2100$Order==975)]
    Percentile25.Sum2100<-Global.Sum2100$Trajectory[(Global.Sum2100$Order==25)]
  
  #write rank in 2100 as csv to call upon in RFT
    write.csv(Global.Sum2100,paste0(outpath,"/Global 2100 Population Rank Order.csv"),row.names=FALSE,na="")
  
#### Calculate ratios for all-age population to mean  ####
    
    #pop<-readRDS("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Results/Reduced Form Tool/RFF Lookup Tables/All Population Data_through 2100.rds")
    pop<-readRDS(file.path(input2,"All Population Data_through 2100.rds"))
    
    
    #read in mean population
      mean.pop<-read.csv("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/rff_data/rff_pop_means.csv")
    #subset for relevant years (<2100)
      mean.pop<-mean.pop[(mean.pop$Year<=2100),]
    
    #summarize population to across all ages
      pop.noage<-pop %>%
                  group_by(LocID,Year,Trajectory)%>%
                  summarize(pop=sum(Pop),.groups="keep")
          
    #summarize mean population across all ages
      meanpop.noage<-mean.pop %>%
                      group_by(LocID,Year)%>%
                      summarize(meanpop=sum(mean_pop),.groups="keep")
      
    #interpolate for years between 5-year bins
      pop.noage<- pop.noage %>%
                    group_by(LocID,Trajectory) %>%
                    complete(Year=min(Year):max(Year))%>%
                    mutate(Population = approx(x=Year,y=pop,xout=2020:2100)$y)
      meanpop.noage<-meanpop.noage %>%
                      group_by(LocID) %>%
                      complete(Year=min(Year):max(Year))%>%
                      mutate(MeanPop = approx(x=Year,y=meanpop,xout=2020:2100)$y)
    ##checking uncertainties
      pop.qs.2075<-pop.noage[(pop.noage$Year==2075),] %>%
                      group_by(LocID)%>%
                      summarize(q.975=quantile(Population,probs=0.975),
                                q.25=quantile(Population,probs=0.025),
                                .groups="keep")
        ratio.qs.2075<-left_join(pop.qs.2075,meanpop.noage[(meanpop.noage$Year==2075),],by="LocID")
        ratio.qs.2075$ratio.975<-ratio.qs.2075$q.975/ratio.qs.2075$MeanPop
        ratio.qs.2075$ratio.25<-ratio.qs.2075$q.25/ratio.qs.2075$MeanPop

    #join pop and mean pop sums
      pop.noage<-left_join(pop.noage,meanpop.noage,by=c("LocID","Year"))
      #calculate ratio
        pop.noage$Ratio<- pop.noage$Population / pop.noage$MeanPop
        
    ##continue checking uncertainties
      pcts.qs.2075<-pop.noage[(pop.noage$Year==2075),] %>%
                      group_by(LocID)%>%
                      summarize(q.975=quantile(Ratio,probs=0.975),
                                q.25=quantile(Ratio,probs=0.025),
                                .groups="keep")
      ratio.qs.2075<-left_join(ratio.qs.2075,pcts.qs.2075,by="LocID",suffix=c("",".pcts"))
        write.csv(ratio.qs.2075,paste0(outpath,"QC/Check Pop Uncertainty Calc 2075.csv"),row.names=FALSE,na="")
   
    #calculate 97.5th and 2.5th and std dev of ratios for all years/countries across trajectories
      pop.noage.uncertainty<-pop.noage %>%
                              group_by(LocID,Year)%>%
                              summarize(mean=mean(Ratio),
                                        q.975=quantile(Ratio,probs=0.975),
                                        q.25=quantile(Ratio,probs=0.025),
                                        std.dev=sd(Ratio),
                                        .groups="keep")
      #export
      write.csv(pop.noage.uncertainty,paste0(outpath,"Uncertainty of All Population Ratios_AllAges_through 2100.csv"),
                row.names=FALSE,na="")
        
    #subset columns for final files
      pop.noage.ratio<-pop.noage[,c('LocID','Year','Trajectory','Ratio')]

    #export
      saveRDS(pop.noage.ratio,file=paste0(outpath,"/All Trajectory Population Ratios_AllAges_through 2100.rds"))
    #split up input so that it can go on github (currently 175mb)
      pop.split<-split(pop.noage.ratio,rep(1:2,length.out=nrow(pop.noage.ratio),each=ceiling(nrow(pop.noage.ratio)/2)))
      
      for(i in 1:length(pop.split)){
        
        saveRDS(pop.split[[i]],file=paste0(outpath,"All Trajectory Population Ratios_AllAges_through 2100_",i,".rds"))
        
      }
      
    #remove unnecessary datasets for mortality (keep pop and mean.pop)
      rm(pop.noage,pop.noage.ratio,pop.noage.uncertainty,pop.qs.2075,ratio.qs.2075,pop.25.2075,pcts.qs.2075,
         ratio.25.2075,ratio.975.2075,mean.pop.noage)

#### Calculate ratios for all-age Mortality to mean  ####
      
    mx<-readRDS("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Results/Reduced Form Tool/RFF Lookup Tables/All Mortality Data_through 2100.rds")
    #join mortality by age-with population
      #join with pop
        mx<-left_join(mx,pop,by=c("LocID","Min.Year"="Year","Age","Trajectory"))
      #join with IFs factors
        mx<-left_join(mx,IFsFactors,by=c("LocID","Age"="RFF_Age_range","Year"="Years"))
        #calculate respiratory rate
          mx$RespRate<-mx$mx * mx$final_AF
    
    #summarize population to across all ages
      mx.noage<-mx %>%
                 group_by(LocID,Period,Year,Min.Year,Trajectory) %>%
                 summarize(mort=weighted.mean(RespRate,Pop),.groups="keep")
    
    #summarize mean mortality/age across all ages
      #read in mean mortality
        mean.mort<-read.csv("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/rff_data/rff_mort_means.csv")
      #join with population first
        mean.mort<-left_join(mean.mort,Cross.year,by=c("Year"="RFF year"))
        mean.mort<-left_join(mean.mort,mean.pop,by=c("LocID","Min.Year"="Year","Age"))
        mean.mort<-left_join(mean.mort,IFsFactors,by=c("LocID","Year"="Years","Age"="RFF_Age_range"))
      #calculate mean resp mortality 
        mean.mort$mean_resp_rate<-mean.mort$mean_mort_rate * mean.mort$final_AF
      #subset for less year <2100
        mean.mort<-mean.mort[(mean.mort$Min.Year<2100),]
      meanmort.noage<-mean.mort %>%
                       group_by(LocID,Min.Year) %>%
                       summarize(meanmort=weighted.mean(mean_resp_rate,mean_pop),.groups="keep")
      
    #interpolate for years between 5-year bins
      mx.noage<- mx.noage %>%
                  group_by(LocID,Trajectory) %>%
                  complete(Min.Year=min(Min.Year):max(Min.Year))%>%
                  mutate(Mortality = na.approx(mort))
      meanmort.noage<-meanmort.noage %>%
                        group_by(LocID) %>%
                        complete(Min.Year=min(Min.Year):max(Min.Year))%>%
                        mutate(MeanMortality = na.approx(meanmort))
      
      ##checking uncertainties
        mort.qs.2075<-mx.noage[(mx.noage$Min.Year==2075),] %>%
                        group_by(LocID)%>%
                        summarize(q.975=quantile(Mortality,probs=0.975),
                                  q.25=quantile(Mortality,probs=0.025),
                                  .groups="keep")
        ratio.qs.2075<-left_join(mort.qs.2075,meanmort.noage[(meanmort.noage$Min.Year==2075),],by="LocID")
        ratio.qs.2075$ratio.975<-ratio.qs.2075$q.975/ratio.qs.2075$MeanMortality
        ratio.qs.2075$ratio.25<-ratio.qs.2075$q.25/ratio.qs.2075$MeanMortality
      
    #join mx and mean mort sums
      mx.noage<-left_join(mx.noage,meanmort.noage,by=c("LocID","Min.Year"))
    #calculate ratio
      mx.noage$Ratio<- mx.noage$Mortality / mx.noage$MeanMortality
    
      ##continue checking uncertainties
        pcts.qs.2075<-mx.noage[(mx.noage$Min.Year==2075),] %>%
                        group_by(LocID)%>%
                        summarize(q.975=quantile(Ratio,probs=0.975),
                                  q.25=quantile(Ratio,probs=0.025),
                                  .groups="keep")
        ratio.qs.2075<-left_join(ratio.qs.2075,pcts.qs.2075,by="LocID",suffix=c("",".pcts"))
          write.csv(ratio.qs.2075,paste0(outpath,"QC/Check Mort Uncertainty Calc 2075.csv"),row.names=FALSE,na="")
          
    #calculate 97.5th and 2.5th and std dev of ratios for all years/countries across trajectories
      mx.noage.uncertainty<-mx.noage %>%
                              group_by(LocID,Min.Year)%>%
                              summarize(mean=mean(Ratio),
                                        q.975=quantile(Ratio,probs=0.975),
                                        q.25=quantile(Ratio,probs=0.025),
                                        std.dev=sd(Ratio),
                                        .groups="keep")
      names(mx.noage.uncertainty)[2]<-"Year"
      
      #for 2095-2100 set equal to 2095 values/ratio
        mx.noage.uncertainty.2095<-mx.noage.uncertainty[(mx.noage.uncertainty$Year==2095),]
        mx.noage.uncertainty.2095<-mx.noage.uncertainty.2095 %>%
                                      group_by(LocID) %>%
                                      complete(Year=c(2095:2100))%>%
                                      fill(q.975,q.25,std.dev,.direction="down")
        #join 2095-2100 with rest of data
          mx.noage.uncertainty<-rbind(mx.noage.uncertainty,mx.noage.uncertainty.2095[(mx.noage.uncertainty.2095$Year>2095),])
      #export
        write.csv(mx.noage.uncertainty,paste0(outpath,"Uncertainty of All Mortality Ratios_AllAges_through 2100.csv"),
                  row.names=FALSE,na="")
      
    #subset columns for final file
      mx.noage.ratio<-mx.noage[,c('LocID','Period','Min.Year','Trajectory','Ratio')]
        names(mx.noage.ratio)[3]<-"Year"
        
      #for 2095-2100 set equal to 2095 values/ratio
        mx.noage.ratio.2095<-mx.noage.ratio[(mx.noage.ratio$Year==2095),]
          mx.noage.ratio.2095<-mx.noage.ratio.2095 %>%
                                group_by(LocID,Trajectory) %>%
                                complete(Year=c(2095:2100))%>%
                                fill(Ratio,.direction="down")
      #join 2095-2100 with rest of data (exclude 2095 from second table otherwise you'll have duplicates)
          mx.noage.ratio<-rbind(mx.noage.ratio,mx.noage.ratio.2095[(mx.noage.ratio.2095$Year>2095),])
            
    #export
      saveRDS(mx.noage.ratio,file=paste0(outpath,"/All Trajectory Resp Mortality Ratios_AllAges_through 2100.rds"))
      #split up input so that it can go on github (currently 175mb)
      mx.split<-split(mx.noage.ratio,rep(1:2,length.out=nrow(mx.noage.ratio),each=ceiling(nrow(mx.noage.ratio)/2)))
      
      for(i in 1:length(mx.split)){
        
        saveRDS(mx.split[[i]],file=paste0(outpath,"All Trajectory Resp Mortality Ratios_AllAges_through 2100_",i,".rds"))
        
      }
      
    #remove datasets to clear up memory for remaining analysis
      rm(mx.noage,mx.noage.ratio,mx.noage.ratio.2095,mx.noage.uncertainty,mx.noage.uncertainty.2095,mort.qs.2075,
         ratio.qs.2075,mx.pop,pcts.qs.2075,mean.mx.noage,meanmort.noage,IFsFactors,IFsCountry,IFsCtryCross,
         Cross.year)
    #no longer need mx/pop data
      rm(mx,pop,mean.mort,mean.pop)
                      
#### Create Global GDP Summary table ####
  #set path
    SocioEcon.GDP<-"J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/Mortality/rffsps_v5/pop_income/"
  
    #get list of input GDP feather files
      GDP.Files<-list.files(SocioEcon.GDP,pattern = ".feather")
    #read in all files into an array to loop over
      All.GDP<-lapply(paste0(SocioEcon.GDP,GDP.Files), read_feather)
        names(All.GDP)<-GDP.Files
    #add trajectory
      All.GDP<-Map(cbind,All.GDP,Trajectory=substr(GDP.Files,22,nchar(GDP.Files)-8))
    #create single dataframe for summary
      Combine.GDP<-do.call(rbind,All.GDP)
    #subset for relevant years (2020-2100)
      Combine.GDP<-Combine.GDP[(Combine.GDP$Year<=2100),]
    
  #read in country name crosswalk
    Country.Cross<-read_xlsx("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/World Countries Grid/rff_country_codes.xlsx",
                               sheet="rff_country_codes",skip=1)
  #join country code
    Combine.GDP<-left_join(Combine.GDP,Country.Cross,by=c("Country"="ISO Abb"))
    #add LocID for countries we added polygons (see: "J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/World Countries Grid/rff_country_codes.xlsx")
      Combine.GDP$LocID[(Combine.GDP$Country=="HKG")]<-344
      Combine.GDP$LocID[(Combine.GDP$Country=="TWN")]<-158
      Combine.GDP$LocID[(Combine.GDP$Country=="MAC")]<-446

    Combine.GDP<-Combine.GDP[,c('Country','Year','Pop','GDP','Trajectory','LocID')]
      names(Combine.GDP)[5]<-"RunID"
    #Calculate GDP per capita; GDP unit is millions of $2011, population is in 1,000s of people
      Combine.GDP$GDP.PerCapita<- (Combine.GDP$GDP*1000000) / (Combine.GDP$Pop*1000)
    #interpolate for years between 5-year bins  
      Combine.GDP<- Combine.GDP %>%
                      group_by(Country,LocID,RunID)
        
      comp.funct<-function(x){    
                    pb$tick()
                    min(x):max(x)
      }
      
      num_ticks<-n_groups(Combine.GDP)
      pb<-progress_bar$new(total=num_ticks)
      Combine.GDP<-Combine.GDP %>%
                    complete(Year=comp.funct(Year))%>%
                    mutate(GDP.PerCapita = approx(x=Year,y=GDP.PerCapita,xout=2020:2100)$y)
               
    #fill in Col/Row (Col is LocID)
      Combine.GDP$COL<-Combine.GDP$LocID
      Combine.GDP$ROW<-1
    saveRDS(Combine.GDP,file=paste0(outpath,"RFF Country GDPperCap_interpolated through 2100.rds"))
  
  ## calculate annual growth rate of GDP
    #read in existing RDS (did this after GDP file was created so skipped first part)
      Combine.GDP<-readRDS(paste0(outpath,"RFF Country GDPperCap_interpolated through 2100.rds"))
    #calculate growth rates
      Combine.GDP<- Combine.GDP %>%
                          group_by(RunID,LocID)
      
      growth.funct<-function(x){    
                      pb$tick()
                      (x-lag(x))/lag(x)
                    }
      
      num_ticks<-n_groups(Combine.GDP)
      pb<-progress_bar$new(total=num_ticks)
      
      #run calculation
      Combine.GDP<-Combine.GDP %>%
                    mutate(Growth=growth.funct(GDP.PerCapita))

    #replace NA growth in 2020 with 0
      Combine.GDP$Growth[Combine.GDP$Year==2020]<-0
    #split up GDP input so that it can go on github (currently 2.2gb)
      Combine.GDP.split<-split(Combine.GDP,rep(1:23,length.out=nrow(Combine.GDP),each=ceiling(nrow(Combine.GDP)/23)))

  saveRDS(Combine.GDP,file=paste0(outpath,"RFF Country GDPperCap_interpolated through 2100.rds"))
  for(i in 1:length(Combine.GDP.split)){
    
    saveRDS(Combine.GDP.split[[i]],file=paste0(outpath,"Split GDP Files/RFF Country GDPperCap_interpolated through 2100_",i,".rds"))
    
  }
            
  #summarize GDP/population globally by year - for countries not in online RFF download
    Global.GDP<- Combine.GDP %>%
                  group_by(Year,RunID) %>%
                  summarize(Pop = sum(Pop),
                            GDP = sum(GDP),
                            .groups="keep")
  #interpolate for years between 5-year bins  
     Global.GDP<- Global.GDP %>%
                    group_by(RunID) %>%
                    complete(Year=min(Year):max(Year))%>%
                    mutate(Pop = approx(x=Year,y=Pop,xout=2020:2100)$y,
                           GDP = approx(x=Year,y=GDP,xout=2020:2100)$y)

  #Calculate GDP per capita; GDP unit is millions of $2011, population is in 1,000s of people
    Global.GDP$GDP.PerCapita<- (Global.GDP$GDP*1000000) / (Global.GDP$Pop*1000)
  
  #join with Trajectory ID
      GDPSampleID<-read.csv("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/Mortality/rffsps_v5/sample_numbers/sampled_gdp_trajectory_numbers.csv")
        GDPSampleID$RunID<-row.names(GDPSampleID)
        GDPSampleID$RunID<-as.integer(GDPSampleID$RunID)
      PopSampleID<-read.csv("J:/share/OAQPS 2021/BenMAP FollowOn (TO 6)/Task 2.2 BenMAP Cloud Tool/2.2.4 Climate and International Analyses/Data/Mortality/rffsps_v5/sample_numbers/sampled_pop_trajectory_numbers.csv")
        PopSampleID$RunID<-row.names(PopSampleID)
        PopSampleID$RunID<-as.integer(PopSampleID$RunID)
      
    Global.GDP<-reduce(list(Global.GDP,GDPSampleID,PopSampleID),left_join,by=c("RunID"))
      names(Global.GDP)[6:7]<-c("Trajectory.GDP","Trajectory.Pop")
  #rename columns to reflect units
    names(Global.GDP)[3:4]<-c("Pop.per.1k","GDP.per.1M")
      
  #calculate growth rates
    Global.GDP<-read.csv(paste0(outpath,"RFF Global GDP Per Capita_through 2100.csv"))
    Global.GDP<-Global.GDP %>%
                  group_by(RunID)%>%
                  mutate(Growth=(GDP.PerCapita-lag(GDP.PerCapita))/lag(GDP.PerCapita))
  #replace NA growth in 2020 with 0
    Global.GDP$Growth[Global.GDP$Year==2020]<-0
    
  #export lookup global table
    write.csv(Global.GDP,paste0(outpath,"RFF Global GDP Per Capita_through 2100.csv"),
              row.names=FALSE,na="")
    