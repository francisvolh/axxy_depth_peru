library(tidyverse)
library(seabiRds)
library(imputeTS)

merged.tdr.gps<-readRDS("C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/merged.tdr.gps.RDS")

###
###Start getting metrics for dives

#summary of dives FROM MERGED
SUMMADIVESmerg <- merged.tdr.gps %>% 
  filter(in_dive == 1) %>% 
  group_by(dep_id, trip_IDi, dive_id) %>% 
  summarise(
    durationsecs=n(),
    maxDepth=max(depth),
    startDive=min(time),
    endDive=max(time),
    Species = unique(species),
    unique_trip_id = unique(trip_IDi)
  ) 

unique(SUMMADIVES$dep_id)

hist(SUMMADIVES$durationsecs)
hist(SUMMADIVES$maxDepth)


hist(SUMMADIVES[which(SUMMADIVES$Species == "PEBO"),]$maxDepth)
hist(SUMMADIVES[which(SUMMADIVES$Species == "GUCO"),]$maxDepth)

#from tdrdata RDS
##summary of dive directly from TDR data alone, no trip
SUMMADIVES <- tdrdata %>% 
  mutate(
    time = lubridate::with_tz(time,tz = "America/Lima"),
    in_dive = ifelse(depth >0.5, 1, 0)
  ) %>% 
  group_by(dep_id) %>%
  mutate(
    dive_id = getSessions(in_dive, ignore = TRUE, ignoreValue = 0)) %>% 
  ungroup()  %>% 
  filter(in_dive == 1) %>% 
  group_by(dep_id, dive_id) %>% 
  summarise(
    durationsecs=n(),
    maxDepth=max(depth),
    startDive=min(time),
    endDive=max(time)
  ) 

saveRDS(SUMMADIVES,"SUMMADIVES.RDS")

#summary of dives with metric of post duration
SUMMADIVES_post <- tdrdata %>% 
  group_by(dep_id, dive_id, in_dive) %>% 
  summarise(
    durationsecs=n(),
    maxDepth=max(depth),
    startDive=min(time),
    endDive=max(time),
  ) 
####

SUMMADIVES.ind1 <- SUMMADIVES %>% 
  group_by(dep_id) %>% 
  summarise(
    x_duration = mean(durationsecs),
    max_dur = max(durationsecs),
    min_dur = min(durationsecs),
    x_max_depth = mean(maxDepth),
    max_Depth = max(maxDepth),
    min_Depth = min(maxDepth),
    total_dives = n()
  )



#recalculate Trip summaries, and check for differences in unique trip_ids with previous summaries

#summarise trips without cleaning, at 1km of colony
SUMMAGPS_merged <- merged.tdr.gps %>% 
  filter(!is.na(trip_IDi))%>% 
  group_by(dep_id, trip_IDi) %>%
  dplyr::summarise(
    steps = n(), #number of gps points that define the trip
    Year = max(year, na.rm=TRUE), #year of the trip
    Day = max(day, na.rm=TRUE), #day of the trip
    maxdist = max(coldist), #max distance to the colony
    tottime = sum(dt, na.rm = TRUE), #total time traveled (beware its a SUM of dif times between steps)
    totdist = sum(dist, na.rm = TRUE), # total distance traveled (beware distance between steps)
    startt = min(time), #start of the trip
    endt = max(time),
    samplingR= mean(dt, na.rm=TRUE),
    TimeTrip = difftime(endt, startt, units = c("hours")),#end of the trip
    minDiffTime = min(dt, na.rm=TRUE),
    maxDiffTime = max(dt, na.rm=TRUE), #largest sampling rate of trip
    MaxSpeed = max(speed, na.rm=TRUE),
    #Spec = unique(species),
    #unique_trip = unique(unique_trip),
    distCOLend = tail(coldist,1)
  ) %>% 
  mutate(
    unique_tripID = paste(dep_id, "_trip_", trip_IDi)
  )
nrow(SUMMAGPS_merged)

SUMMAGPS_merged <- SUMMAGPS_merged[which(!SUMMAGPS_merged$steps <= 5),]
SUMMAGPS_merged <- SUMMAGPS_merged[which(!SUMMAGPS_merged$TimeTrip > 10 ),]
SUMMAGPS_merged <- SUMMAGPS_merged[which(!SUMMAGPS_merged$maxdist < 5 ),]

#CHECK THIS 1 trip that does not match my Previous summary of trips => clean it or add ending point if a gappy trip 
SUMMAGPS_merged[which(SUMMAGPS_merged$TimeTrip <0.3),]

nrow(SUMMAGPS_merged)