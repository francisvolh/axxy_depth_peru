library(ggplot2)
library(seabiRds)
library(dplyr)

#library(diveMove)

##########playing with data and plots##############################################
###################################################################################
#axy2<-read.delim("02RBME20210711.csv", nrows = 4320000)

cam5<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/cam ARDs/C05PEBO_20191114_A161_S2.csv")
axxy01<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/gps/axxy tdr/A01PEBO_A31_20191113_S1.csv")
axxy06<-read.csv("C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/gps/axxy tdr/A06_PEBO_17112019_A105_S2.csv")

b<-axy2[!is.na(axy2$Depth),]

#b<-axy2[!is.na(axy2$Depth),]

b$Timestamp<-as.POSIXct(b$Timestamp, format = "%d/%m/%Y %H:%M:%S", tz="UTC")

plot(technoTDRdlw_data$time[technoTDRdlw_data$dep_id=="A06_PEBO_17112019_A105" ],
     technoTDRdlw_data$depth[technoTDRdlw_data$dep_id=="A06_PEBO_17112019_A105"], 
     type="l")

plot(technoTDRdlw_data$time[technoTDRdlw_data$day =="2019-11-11"],
     technoTDRdlw_data$depth[technoTDRdlw_data$day =="2019-11-11"], 
     type="l")

b<-technoTDRdlw_data[technoTDRdlw_data$dep_id=="A06_PEBO_17112019_A105",]
b<-b[b$day =="2019-11-14",]

plot(b$time,
     b$depth, 
     type="l")

abline(h=0.30, col = "red")

technoTDRdlw_data$diving<-ifelse(technoTDRdlw_data$depth>0.3, 1, 0)
###################################################################################
###################################################################################


#Load TDR data from axxys
#technofolder<-"E:/2019 GN/ARDs"
technofolder<-"C:/Users/franc/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys"
setwd(technofolder)
depthData<-readRDS(file ="technoTDRPeru_data_NEW.RDS")
#depthsTrials<-test_ecoOLD
#depthsTrials<-test_eco

depthData$diving<-ifelse(depthData$depth>0.5, 1, 0)

#first run only dive IDs with only DIVES, next run, with no dives to, for post-duration times
depthData <- depthData %>% 
  mutate(
    species = substring(dep_id, 4, 7),
    year = lubridate::year(time)
  )

#label dives only for diving
depthData <- depthData %>% 
  group_by(dep_id) %>% 
  mutate(
    diveID=getSessions(value=diving, ignore=T, ignoreValue = 0)
  )

#Summary of dives
Summary.dives <- depthData %>% 
  group_by(species, dep_id, diveID, diving) %>% 
  summarise(
    durationsecs=n(),
    maxDepth=max(depth),
    startDive=min(time),
    endDive=max(time),
    ) 

#second object will produce object with IDs for Dives and NO dives to be used later to calculate the Bouts after
postDurs.Depths <- depthData %>% 
  group_by(dep_id) %>% 
  mutate(
    postdiveID=getSessions(value=diving)
  ) %>% 
  group_by(dep_id, postdiveID, diving) %>% 
  summarise(
    durationsecs=n(),
    maxDepth=max(depth),
    startDive=min(time),
    endDive=max(time),
  ) %>% 
  ungroup() %>% 
  mutate(
    postDurationsecs=lead(durationsecs,1)
  ) %>% 
filter(
 diving==1
)





hist(summary.Depths$postDurationsecs, breaks = 100)
hist(summary.Depths$postDurationsecs[summary.Depths$postDurationsecs<500], breaks = 100)

max(summary.Depths$postDurationsecs, na.rm=TRUE)
summary.Depths[which(summary.Depths$postDurationsecs>70000),]

#treshold in seconds for diving bouts
bec<-300

#summary for dives and bouts, first need to run previous summary
summary.Depths %>% 
  mutate(
    inBout=ifelse(durationsecs<=bec, 1, 0),
    boutID=getSessions(value=inBout, ignore=TRUE, ignoreValue = 0)
  ) %>% 
  filter(inBout==1 & diving==1) %>% 
  group_by(dep_id, boutID) %>% 
  summarise(
    numDives=n(),
    startBout=min(startDive),
    endBout=max(endDive),
    timeBout=as.numeric(difftime(endBout,startBout, units = "sec"))+1,
    meanDiveDepth=mean(maxDepth),
    minDiveDepth=min(maxDepth),
    maxDiveDepth=max(maxDepth)
  ) %>% 
  ungroup() %>% 
  mutate(
    afterBout=as.numeric(difftime(lead(startBout),endBout, units = "sec")),
  )

View(summary.Depths)

#for 1 bird
#incorporate bout and diving IDs to raw Depth data dataframe
depthData<-depthData %>% 
  group_by(dep_id, diveID, diving) %>% 
  mutate(
    durationsecs=n(),
    inBout=ifelse(durationsecs<=bec, 1, 0)
  ) %>% 
  ungroup() %>% 
  group_by(dep_id) %>% 
  mutate(
    boutID=getSessions(value=inBout, ignore=TRUE, ignoreValue = 0)
  )


GUCOdepthData <- depthData %>% 
  subset(
    Species=GUCO
  )


ggplot(b, aes(x = Timestamp, y = Depth)) +
  geom_line() +
  geom_point(data = b[b$inBout == 1,], aes(x = Timestamp, y = Depth, col = factor(boutID))) 


###Merging depth and tracks
dp <- merge(gps.data, depthData, by = c("dep_id", "time"),  all=TRUE)