library(tidyverse)

#loop for merging summary of dives and summary of trips

SUMMAGPS<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv")

df<-SUMMAGPS #summary of TRIPS (cleaned and valid trips only) from original GPS dataframe from seabiRds with "unique_trip_ids" (dep_id + trip_id)

SUMMADIVES<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/SUMMADIVES.RDS") #summary of dives, with just dive_id, from DEPTH file from seabiRds 

dives<-SUMMADIVES 

#may be unnecesary for some datasets
# only in case  you have less devices deployed for dives than GPS
df<-df %>% 
  filter(dep_id %in% unique(SUMMADIVES$dep_id)) 


mmerSumm_dives_trips<-NULL #create blank df to keep at the end
for (i in unique(df$unique_trip)) { #loop for every unique trip per bird

  one_trip<-df[which(df$unique_trip==i),]#subset GPS df for 1 unique trip
  
  start <- one_trip$startt # get Start time of 1 unique trip 
  end <- one_trip$endt #get end time of 1 unique trip
  
  dep_ID<-one_trip$dep_id#get dep_id of 1 unique trip
  
  dives_one_depID<- dives[which(dives$dep_id == dep_ID),]#get dives with dive_IDs for corresponding times for a corresponding bird
  
  dives_one_trip <- dives_one_depID[which(dives_one_depID$startDive >= start & dives_one_depID$endDive >= start ),]#subset dives with dive_ids for the times of the unique trip 
  
  summ_dives_one_trip <- dives_one_trip %>% #get the summary metrics for that unique trip 
    summarise(
      mean_MaxDepth = mean(maxDepth), 
      n(),
      mean_time = mean(durationsecs)
    )
  
  one_trip_summs<-cbind(one_trip, summ_dives_one_trip[,2:ncol(summ_dives_one_trip)]) #bind GPS summary and new Dive summary for 1 trip
  
  mmerSumm_dives_trips<-rbind(mmerSumm_dives_trips, one_trip_summs)#incoorporate every object of the loop per unique trip
}
View(mmerSumm_dives_trips)
