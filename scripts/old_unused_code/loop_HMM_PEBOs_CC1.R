library(dplyr)
library(seabiRds)
library(imputeTS)
library(ggplot2)
library(momentuHMM)
library(cowplot)
#library(geosphere) #may not be needed in CC because of geos module?
#library(raster) # for depth from bathymetric map

dep <- readRDS("dep_dataPeru_seabiRds.RDS")

#set model parameters
stateNames <- c("Flying", "Foraging", "Colony", "Resting") #, 'Resting' ADD A FORAGING 2, for 
nbStates <- length(stateNames)

wbfDist <- "gamma" # step distribution
wbfPar <- c(mean = c(4, 4, 0.01, 0.01), 
            sd = c(0.50, 1, 0.01, 0.01), 
            c(0.000001,0.001,0.99, 0.99) #maybe be careful with other birds 
) #  zero-mass included

colonyDist <- 'bern'
colonyPar <- c(0.1, 0.1, 0.99999, 0.0000000001)

#if working with only GPS data may incorporate Speed and Angle
#speedDist <-

#angleDist <-

diveDist <- 'bern'
divePar <- c(0.0000000001, 0.999999999, 0.0000000001, 0.0000000001)

#select only DLW deployment, for boobies ##NO need as the ifelse loop selects all the available files 
#in the working directory that match the dep sheet

#rownames(dep) <- 1:nrow(dep)
#dlw_idx<-grep('A', dep$dep_id) #keep all Axxy deployments (will keep Camera and Guco deployments)
#dep<-dep[dlw_idx,]

#rownames(dep) <- 1:nrow(dep)

#dlw_idx<-grep('C', dep$dep_id) #get rid of all the camera and gucos deployments
#dep<-dep[-dlw_idx,]

#rownames(dep) <- 1:nrow(dep)
#dlw_idx<-grep('G', dep$dep_id) #get rid of non DLW axxy deps for boobies
#dep<-dep[-dlw_idx,]

## Get list of raw data files to process
#
setwd("axxys pebos gucos")
fn <- list.files('/project/6005805/francisv/axxys pebos gucos', full.names = T, pattern = '.csv')

# output directory

out_dir <- 'processed_acc'
if (dir.exists(out_dir)== FALSE) { 
  dir.create(out_dir)
}

########################################
#split deployments in files of 2
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
    
    
    dat<-read.csv(fn[idx], stringsAsFactors = FALSE, sep = ",")
    
    dat <- dat %>% 
      dplyr::mutate(
        dep_id = dd,
        time = as.POSIXct(Timestamp, tz = 'UTC', format = "%d/%m/%Y %H:%M:%OS") 
      ) %>% 
      dplyr::select(dep_id, time, X, Y, Z, location.lon, location.lat, Depth) %>% 
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
        wbf = seabiRds::getPeakFrequency(data = z, time = time, method = 'fft',
                                         window = 30,
                                         maxfreq = 10, 
                                         threshold = 0.2,
                                         sample = 1),
        odba = seabiRds::getDBA(X = x, Y = y, Z = z, time = time, window = 60),
        odba = zoo::rollmean(odba, k =  60 * freq, fill = NA, na.rm = T),
        Pitch = seabiRds::getPitch(X = x, Y = y, Z = z, time = time, window = 1),
        Depth = imputeTS::na_interpolation(Depth),
      ) %>% slice(seq(1, nrow(dat), freq*5))
    
    dat<-seabiRds::filterSpeed(dat, lon = "lon", lat="lat", time="time", threshold = 101)
    
    d <- dat %>% 
      dplyr::select(dep_id, time, lon, lat, coldist, wbf, odba, Depth) %>% 
      rename(ID = dep_id) %>% 
      prepData(type = "LL", coordNames = c("lon", "lat")) %>% 
      mutate(
        colony = ifelse(coldist < 1, 1, 0), #could be parameter mindist
        wbf = ifelse(wbf < 2, 0, wbf),
        diving = ifelse(Depth > 0.5, 1, 0)#could be paramerte mindepth
      )
    
    
    m <- fitHMM(data=d,
                nbStates=nbStates,
                dist=list(wbf = wbfDist, colony = colonyDist, diving = diveDist),
                Par0=list( wbf = wbfPar, colony = colonyPar, diving = divePar),
                formula = ~ 1
    )
    
    dat$HMM <- viterbi(m)
    dat$HMM <- factor(dat$HMM, labels = stateNames)
    
    saveRDS(m, paste0(out_dir, '/', dd,'_HMM.RDS'))
    write.csv(dat, paste0(out_dir, '/', dd,'.csv'), row.names = F)
    
    bp <- dat %>% 
      dplyr::select(time, coldist, wbf, Depth, HMM) %>% 
      tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth')) %>% 
      ggplot2::ggplot(ggplot2::aes(x = HMM, y = value)) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
      ggplot2::theme(text = element_text(size = 10))
    
    
    tp <- dat %>% 
      dplyr::select(time, coldist, wbf, Depth, odba, HMM) %>% 
      tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth' )) %>% 
      ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(col = HMM)) +
      ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
      ggplot2::theme(legend.position = c(0.10, 0.90),
                     legend.background = element_blank(),
                     legend.key = element_blank(),
                     text = element_text(size = 10))
    
    p <- cowplot::plot_grid(bp, tp)
    ggsave(paste0(out_dir, '/', dd,'_plots.png'), p, units = 'in', width = 10, height = 5)
    
    rm(m)
    print(paste("Finished", dd, "at", format(Sys.time(), "%T")))
    
  }else{}
  
}


