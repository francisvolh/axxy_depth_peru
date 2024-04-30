#!/usr/bin/env Rscript

#code writen to run in the command line in bash, 3 birds at a time
# DBA window of 3 secs, mean of 1 second, and sliced at 1 second before HMMs

dba_wind <- 3
dba_mean <- 1
wbf_wind <-30
hmm_slice <- 1
my.args <- commandArgs(trailingOnly = TRUE) #be careful in the future with ARGUMENTS (eg. A01... A11)that dont match the logger ID name!!!


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

#select only DLW deployment, for boobies ##NO NEED TO SELECT WITH THE IF ELSE LOOP, 
#as it chooses from the available files

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
#setwd("C:/Users/francis van oordt/Downloads/done full axy")
#fn <- list.files(path="C:/Users/francis van oordt/Downloads/done full axy",full.names = T, pattern = '.csv')

setwd("axxys pebos gucos")
fn <- list.files('/project/6005805/francisv/axxys pebos gucos/processed_acc/raw_axxy', full.names = T, pattern = '.csv')

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

# create a list of the arguments from Bash, maybe there is a more efficiento way for this
id_bird1 <- fn[grep(my.args[1], fn)]
id_bird2 <- fn[grep(my.args[2], fn)]
id_bird3 <- fn[grep(my.args[3], fn)]
  
  
bird3_list <- c(id_bird1, id_bird3, id_bird3) #new list of files to work with according to arguments given in Bash


#to work with 3 files at a time 

birds3 <- NULL
  
for (i in 1:length(dep$dep_id) ) { #loop over only 3 birds
  
  dd <- dep$dep_id[i] # grabs a bird from the dep sheet, which may contain all deployed birds of the season
  
  
  idx <- grep(dd, bird3_list)
  
  if(!identical(idx, integer(0))){ # checks if the Axxy file for the dep sheet bird is present and runs, if not, goes to next
    print(paste('Start:', dd, 'at', format(Sys.time(), "%T")))
    
    #dat <- vroom(fn[idx], col_types= "c?nnncnnnnnndnnn", delim = ',') #reads fast but next steps are slower it seems
    print(paste("Started reading bird", my.args[i], "at", Sys.time()))
    
    dat<-read.csv(bird3_list[idx], stringsAsFactors = FALSE, sep = ",")
    
    dat <- dat |> 
      dplyr::mutate(
        dep_id = dd,
        time = as.POSIXct(Timestamp, tz = 'UTC', format = "%d/%m/%Y %H:%M:%OS") 
      ) |> 
      dplyr::select(dep_id, time, X, Y, Z, location.lon, location.lat, Depth) |> 
      dplyr::rename(x = X, y = Y, z = Z, lon = location.lon, lat = location.lat) |> 
      dplyr::filter(time > dep$time_released[dep$dep_id == dd],
                    time < dep$time_recaptured[dep$dep_id == dd])
    
    freq <- seabiRds::getFrequency(dat$time) # calculates the sampling rate of the axxy for each bird
    
    dat <- dat |> 
      dplyr::mutate(
        lon = imputeTS::na_interpolation(lon),
        lat = imputeTS::na_interpolation(lat),
        coldist = seabiRds::getColDist(lon = lon, lat = lat, 
                                       colonyLon = dep$dep_lon[dep$dep_id == dd],
                                       colonyLat = dep$dep_lat[dep$dep_id == dd]),
        wbf = seabiRds::getPeakFrequency(data = z, time = time, method = 'fft',
                                         window = wbf_wind,
                                         maxfreq = 10, ###set to 6
                                         threshold = 0.2,
                                         sample = 1),
        #### add step length as variable, but would need to interpolate CORRECTLY crawl (or ex-foisgrass)
        odba = seabiRds::getDBA(X = x, Y = y, Z = z, time = time, window = dba_wind),
        odba = zoo::rollmean(odba, k =  dba_mean * freq, fill = NA, na.rm = T),
        #Pitch = seabiRds::getPitch(X = x, Y = y, Z = z, time = time, window = 1),
        Depth = imputeTS::na_interpolation(Depth), 
      ) |> slice(seq(1, nrow(dat), freq*hmm_slice)) ### make average by minute instead of slicing
    
    dat<-seabiRds::filterSpeed(dat, lon = "lon", lat="lat", time="time", threshold = 101)
    print(paste("Finished reading bird", my.args[i], "at", Sys.time()))
    
   birds3 <- rbind(birds3, dat)
    
  }else{}
  
  
}
print(paste("Finished joining 3 birds", my.args[1], my.args[2], my.args[3], "at", Sys.time()))

#### split processing and HMM into different loops

d <- birds3 |> 
  dplyr::select(dep_id, time, lon, lat, coldist, wbf, odba, Depth) |> 
  rename(ID = dep_id) |> 
  prepData(type = "LL", coordNames = c("lon", "lat")) |> 
  mutate(
    colony = ifelse(coldist < 1, 1, 0), #could be parameter mindist
    wbf = ifelse(wbf < 2, 0, wbf),
    diving = ifelse(Depth > 0.5, 1, 0)#could be paramerte mindepth
  )

print(paste("Prepped data at", Sys.time()))

m <- fitHMM(data=d,
            nbStates=nbStates,
            dist=list(wbf = wbfDist, colony = colonyDist, diving = diveDist),
            Par0=list( wbf = wbfPar, colony = colonyPar, diving = divePar),
            formula = ~ 1
) ## run as a population

birds3$HMM <- viterbi(m)
birds3$HMM <- factor(birds3$HMM, labels = stateNames)

saveRDS(m, paste0(out_dir, '/', paste0(my.args[1],"_",my.args[2],"_", my.args[3]),'_HMM.RDS'))
write.csv(birds3, paste0(out_dir, '/', paste0(my.args[1],"_",my.args[2],"_", my.args[3]),'.csv'), row.names = F)

print(paste("Saved HMM model output at", Sys.time()))

for (i in unique(birds3$dep_id)){
  
  bp <- birds3 |>
    filter(dep_id == i) |> 
    dplyr::select(time, coldist, wbf, Depth, HMM) |> 
    tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth')) |> 
    ggplot2::ggplot(ggplot2::aes(x = HMM, y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
    ggplot2::theme(text = element_text(size = 10))
  
  
  tp <- test |> 
    #filter(dep_id == i) |> 
    dplyr::select(time, coldist, wbf, Depth, odba, HMM) |> 
    tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth' )) |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(col = HMM)) +
    ggplot2::facet_wrap(rows = ggplot2::vars(name), scales = 'free') +
    ggplot2::theme(legend.position = c(0.10, 0.90),
                   legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 10))
  
  p <- cowplot::plot_grid(bp, tp)
  ggsave(paste0(out_dir, '/', i,'_plots.png'), p, units = 'in', width = 10, height = 5)
  
  rm(m)
  print(paste("Finished", i, "at", format(Sys.time(), "%T")))
  
}
