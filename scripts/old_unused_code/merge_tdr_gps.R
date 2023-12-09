library(tidyverse)
library(seabiRds)
library(imputeTS)

#load depth data from seabiRds object
tdrdata<-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/axxys/technoTDRPeru_data_NEW.RDS")

#load deployment data
dep_dataPeru<-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS")

#load cleaned final gps data
gps.data<-readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/gps_data_seabiRdsFIXED.RDS")

#load final cleaned trip summary with trip IDs, OR PRODUCE A NEW ONE THAT INCLUDES SHORT TRIPS????? or and in colony times?????
SUMMAGPS<-read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 2 - Tracks and overlap/SUMMAGPS.csv")

gpsIDs<-unique(tdrdata$dep_id)

gps.dataTDR <- gps.data %>% 
  filter(dep_id %in% gpsIDs)
#unique(gps.dataTDR$dep_id)

SUMMAGPS_subset <- SUMMAGPS %>% 
  filter(dep_id %in% gpsIDs)

#nrow(SUMMAGPS_subset) #141


#assign dive_ids to dive data only
tdrdata <- tdrdata %>%
  mutate(
    time = lubridate::with_tz(time,tz = "America/Lima"),
    in_dive = ifelse(depth>0.5, 1,0)
  ) %>% 
  group_by(dep_id) %>%
  mutate(
    dive_id = getSessions(in_dive, ignore = TRUE, ignoreValue = 0))

merged.tdr.gps <- merge(gps.dataTDR, tdrdata, by=c("dep_id","time"), all=TRUE)

merged.tdr.gps<-merged.tdr.gps %>% 
  filter(dep_id != "C02PEBO_20191112_A150_S2") # sat on a rock for 2 hours

#fill out gps location for tdr data, and recalculate all dt, speed, and dist, to fill missing values
merged.tdr.gps <- merged.tdr.gps %>%
  group_by(dep_id) %>%
  mutate(
    lon = na_interpolation(lon),
    lat = na_interpolation(lat),
    dist = getDist(lon, lat),
    coldist = na_locf(coldist),
    speed = dist/dt,
    dt = getDT(time, units = "hours"),
    behav = case_when(coldist > 1 & in_dive == 0 ~ "Flying",
                          coldist > 1 & in_dive == 1 ~ "Foraging",
                          coldist < 1 ~ "Colony"),
    species = substring(dep_id, 4, 7)
  )

merged.tdr.gps <- select(merged.tdr.gps, -c(altitude, gpsspeed, satellites, hdop, maxsignal, inrange, wetdry))

merged.tdr.gps$depth[is.na(merged.tdr.gps$depth)] <- 0

merged.tdr.gps <- merged.tdr.gps %>%
  group_by(dep_id) %>%
  arrange(dep_id, time) %>%
  mutate(
    in_tripi = ifelse(coldist > 1, 1, 0),
    trip_IDi = getSessions(in_tripi, ignore = TRUE, ignoreValue = 0)
  )

#summary(merged.tdr.gps)



###
#saveRDS(merged.tdr.gps, "C:/Users/franc/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/merged.tdr.gps.RDS")
