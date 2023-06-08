library(dplyr)
#devtools::install_github("francisvolh/seabiRds")
library(seabiRds)
library(imputeTS)
library(ggplot2)
library(momentuHMM)
library(cowplot)
library(geosphere) # some part  ofthe  code required it installed now
#library(raster) # for depth from bathymetric map

dep <- readRDS("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS")


## Get list of raw data files to process
#
setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data")
fn <- list.files(path='C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data',full.names = T, pattern = '.csv')


########################################
#split deployments in files of 4
#dep2<-dep
#dep<-dep2[4:5,]
########################################

#all axxy csv files that are available

for (i in 1:length(dep$dep_id)) { #check if this fix helped now looking at an NA at the end of loop and giving an Error
  
  dd <- dep$dep_id[i]
  idx <- grep(dd, fn)
  
  if(!identical(idx, integer(0))){
    print(paste('Start:', dd, 'at', format(Sys.time(), "%T")))
    
    #dat <- vroom(fn[idx], col_types= "c?nnncnnnnnndnnn", delim = ',') #reads fast but next steps are slower it seems
    
    dat<-read.csv(fn[1], stringsAsFactors = FALSE, sep = ",")
    
    dat <- dat %>% 
      dplyr::mutate(
        dep_id = dd,
        time = as.POSIXct(Timestamp, tz = 'UTC', format = "%d/%m/%Y %H:%M:%OS") 
      ) %>% 
      dplyr::select(dep_id, time, X, Y, Z, location.lon, location.lat) %>% 
      dplyr::rename(x = X, y = Y, z = Z, lon = location.lon, lat = location.lat) %>% 
      dplyr::filter(time > dep$time_released[dep$dep_id == dd],
                    time < dep$time_recaptured[dep$dep_id == dd])
    
    freq <- seabiRds::getFrequency(dat$time)
    
    dat <- dat %>% 
      dplyr::mutate(
        lon = imputeTS::na_interpolation(lon),
        lat = imputeTS::na_interpolation(lat),
        coldist = seabiRds::getColDist(lon = lon, lat = lat, 
                                       colonyLon = dep$dep_lon[dep$dep_id == dd],
                                       colonyLat = dep$dep_lat[dep$dep_id == dd]),
        coldist2 = getColDist2(lon = lon, lat = lat, 
                               colonyLon = dep$dep_lon[dep$dep_id == dd],
                               colonyLat = dep$dep_lat[dep$dep_id == dd])
      )# %>% slice(seq(1, nrow(dat), freq*5)) ### make average by minute instead of slicing
    
    head(dat)
    
    dat<-seabiRds::filterSpeed(dat, lon = "lon", lat="lat", time="time", threshold = 101)

    
    #### split processing and HMM into different loops
   
    
    dat$dists<-dists
    
    tp <- dat %>% 
      dplyr::select(time, coldist, coldist2, dists) %>% 
      tidyr::pivot_longer(cols = c('coldist', 'coldist2', 'dists' )) %>% 
      ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
      ggplot2::geom_line() +
      #ggplot2::geom_point(ggplot2::aes(col = HMM)) +
      ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
      ggplot2::theme(legend.position = c(0.10, 0.90),
                     legend.background = element_blank(),
                     legend.key = element_blank(),
                     text = element_text(size = 10))
    
  
    print(paste("Finished", dd, "at", format(Sys.time(), "%T")))
    
  }else{}
  
}


###################### trials of function

library(terra)

getColDist2 <- function(lon, lat, colonyLon, colonyLat) {
  terra::distance(matrix(c(lon, lat), ncol = 2, nrow = nrow(dat)), 
                  matrix(colonyLon, colonyLat, ncol = 2, nrow =1), 
                  lonlat = T)/1000 #distance from colony in km
}  

rm(getColDist)


dists <- terra::distance(matrix(c(dat$lon, dat$lat), ncol = 2, nrow =nrow(dat)), 
                         matrix(c(dep$dep_lon[dep$dep_id == dd], dep$dep_lat[dep$dep_id == dd]), ncol = 2, nrow =1), 
                         lonlat = T)/1000
summary(dists)

class(dists)


dists1<-dists

c(dists1)
