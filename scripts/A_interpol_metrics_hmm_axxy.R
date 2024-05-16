# read in deployment data
deployments <- readRDS('C:/Users/francis van oordt/Documents/McGill/Field data/dep_dataPeru_seabiRds.RDS') |> 
  dplyr::select(dep_id, metal_band, site, nest, time_released, time_recaptured, 
                dep_lon, dep_lat, mass_on, mass_off, status_on, status_off) |>
  dplyr::filter(stringr::str_detect(dep_id, 'PEBO|A'))|>
  dplyr::filter(!stringr::str_detect(dep_id, 'C|G'))


# convert colony coordinates to my_crs 
col_loc <- unique(deployments[, c('site', 'dep_lon', 'dep_lat')])


col_loc <- sf::st_as_sf(col_loc, coords = c('dep_lon', 'dep_lat'), crs = 4326) ## I am not reprojecting because this is the tropics

col_loc <- sf::st_transform(col_loc, crs =5387 ) #things explode when reprojecting for the centroids after the crawl and prepData


  
dd <- deployments$dep_id

# load the location data from the GPS dataset
gps_data <- readRDS('C:/Users/francis van oordt/Documents/McGill/Field data/gps_data_seabiRds.RDS') |> 
  dplyr::filter(dep_id %in% dd) |> 
  dplyr::select(dep_id, time, lon, lat, coldist, dist, dt, speed
                )|>
  #dplyr::collect() |> 
  dplyr::arrange(dep_id, time) 

#recalculate movement metrics # no need as I am loading he data already run through seabirds

# make spatial points object
locs_sf <- sf::st_as_sf(gps_data, coords = c('lon', 'lat'), crs = 4326) ## I am not reprojecting because this is the tropics

locs_sf <- sf::st_transform(locs_sf, crs = 5387) ## 



# make tracks, 
#need to keep the data as lat lon (so not transform to porjected coordinates from the previous line)

tracks_sf <- locs_sf |> 
  dplyr::group_by(dep_id) |> 
  dplyr::summarize(
    tot_dist = sum(dist),
    n = dplyr::n(),
    do_union=FALSE) |> 
  dplyr::filter(n > 1) |> 
  sf::st_cast(to = 'LINESTRING')

# view tracks

turbo_pal <- c(viridis::turbo(n = length(dd), direction = -1)) # create a pallet of colour for all the dep_ids
col<-plyr::mapvalues(tracks_sf$dep_id,from=dd,to=turbo_pal) # create a vector for all the data to feed leaflet

leaflet::leaflet()|>
  leaflet::addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/stamen_terrain_background/{z}/{x}/{y}{r}.png",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/about" target="_blank">OpenStreetMap</a> contributors'),
    options = leaflet::tileOptions(variant='stamen_toner_lite', apikey = '76a5a6d3-2a88-4129-a816-849bdbaebd56') )|>
  leaflet::addPolylines(data = tracks_sf, color = col)


#allisons way
mapview::mapview(tracks_sf, zcol = 'dep_id') 

#in case the viewer does not work
ggplot2::ggplot()+
  ggplot2::geom_sf(data = tracks_sf, ggplot2::aes(color = dep_id))+
  ggplot2::theme(legend.position = "none")

#Prep data for interpolatitracks_sf#Prep data for interpolation
# time step for interpolating the gps data
time_step <- '1 min'

# generate a list of prediction times for each individual in gps
predTime <- gps_data |> 
  dplyr::group_by(dep_id) |> 
  dplyr::summarize(
    start = lubridate::round_date(min(time), time_step),
    end = lubridate::round_date(max(time), time_step))

pt <- lapply(1:nrow(predTime), function(x) seq.POSIXt(predTime$start[x], predTime$end[x], time_step))

names(pt) <- predTime$dep_id


# Use crawl to interpolate gps data at time-stemp

lnError <- crawl::argosDiag2Cov(50,50,0) # 50m isotropic error ellipse - this assumes a conservative 50 m error in the GPS


# format data for crawl
crawlData <- data.frame(ID = gps_data$dep_id,
                        time = gps_data$time,
                        x = sf::st_coordinates(locs_sf)[,1],
                        y = sf::st_coordinates(locs_sf)[,2],
                        ln.sd.x = lnError$ln.sd.x,
                        ln.sd.y = lnError$ln.sd.y,
                        error.corr = lnError$error.corr)

pt <- vector('list', length(unique(crawlData$ID)))


names(pt) <- unique(crawlData$ID)

for (i in names(pt)) pt[[i]] <- lubridate::round_date(seq.POSIXt(min(crawlData$time[crawlData$ID == i]), 
                                                                 max(crawlData$time[crawlData$ID == i]), 
                                                                 time_step),
                                                      time_step)

# fit the correlated random walk model
system.time({
  crwOut <- momentuHMM::crawlWrap(crawlData,
                                  theta = c(6.5,-.1),
                                  fixPar = c(1,1,NA,NA),
                                  err.model = list(x = ~ln.sd.x-1,
                                                  y = ~ln.sd.y-1,
                                                 rho = ~error.corr),
                                  predTime = pt, 
                                  attempts = 400)
})


saveRDS(crwOut, 'C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 1/crwOut.RDS')

# this will plot the tracks, it pages through each track so it's commented out to be able to run the code all at once
#plot(crwOut)


# load the acc data one deployment at a time, do some basic processing and join with tdr and gps data


fn <- list.files('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/raw axxys', full.names = T, pattern = '.csv')
#fn <-  fn[stringr::str_detect(fn, 'PEBO')]
#fn <-  fn[-stringr::str_detect(fn, 'G')]


# get axxy metrics (takes about 5 hours) -- produced in DRA in 25 minutes, 7 cores 3 birds per core
{
  
  dba_wind <- 3
  dba_mean <- 1
  wbf_wind <-30
# create an output dataframe
data <- data.frame()

for (i in 1:length(dd) ) { #loop over only 3 birds
  
  one_id <- dd[i] # grabs a bird from the dep sheet, which may contain all deployed birds of the season
  
   # checks if the Axxy file for the dep sheet bird is present and runs, if not, goes to next
    print(paste("Started reading bird", one_id, 'at', format(Sys.time(), "%T")))
    
    dat<-read.csv(fn[i], stringsAsFactors = FALSE, sep = ",")
    
    print(paste("Preping bird", one_id, 'data at', format(Sys.time(), "%T")))
    
    dat <- dat |> 
      dplyr::mutate(
        dep_id = one_id,
        time = as.POSIXct(Timestamp, tz = 'UTC', format = "%d/%m/%Y %H:%M:%OS") 
      ) |> 
      dplyr::select(dep_id, time, X, Y, Z, location.lon, location.lat, Depth) |> 
      dplyr::rename(x = X, y = Y, z = Z, lon = location.lon, lat = location.lat) |> 
      dplyr::filter(time > deployments$time_released[deployments$dep_id == one_id],
                    time < deployments$time_recaptured[deployments$dep_id == one_id])
    
    freq <- seabiRds::getFrequency(dat$time) # calculates the sampling rate of the axxy for each bird
    
    print(paste("Sampling freq bird", one_id, "at",freq, 'Hz at', format(Sys.time(), "%T")))
    
    print(paste("Calculating metrics for bird", one_id, 'at', format(Sys.time(), "%T")))
    
    dat <- dat |> 
      dplyr::mutate(
        lon = imputeTS::na_interpolation(lon),
        lat = imputeTS::na_interpolation(lat),
        coldist = seabiRds::getColDist(lon = lon, lat = lat, 
                                       colonyLon = deployments$dep_lon[deployments$dep_id == one_id],
                                       colonyLat = deployments$dep_lat[deployments$dep_id == one_id]),
        wbf = seabiRds::getPeakFrequency(data = z, time = time, method = 'fft',
                                         window = wbf_wind,
                                         maxfreq = 10, ###set to 6
                                         threshold = 0.2,
                                         sample = 1),
        #### add step length as variable, but would need to interpolate CORRECTLY crawl (or ex-foisgrass)
        vedba = seabiRds::getDBA(X = x, Y = y, Z = z, time = time, window = dba_wind),
        vedba = zoo::rollmean(vedba, k =  dba_mean * freq, fill = NA, na.rm = TRUE),
        #Pitch = seabiRds::getPitch(X = x, Y = y, Z = z, time = time, window = 1),
        Depth = imputeTS::na_interpolation(Depth), 
      ) #|> slice(seq(1, nrow(dat), freq*hmm_slice)) ### make average by minute instead of slicing
    
    #dat<-seabiRds::filterSpeed(dat, lon = "lon", lat="lat", time="time", threshold = 101)
    
    print(paste("Summarizing metrics for bird", one_id, 'at', format(Sys.time(), "%T")))
    
    
    temp <- dat |> 
      dplyr::filter(!is.na(wbf)) |> 
      dplyr::mutate(
        time = lubridate::round_date(time, '1 min')
      ) |> 
      dplyr::group_by(dep_id, time) |> 
      dplyr::mutate(
        in_dive = ifelse(Depth >0, 1, 0)
      )|>
      dplyr::summarize(
        wbf = quantile(wbf, 0.5, na.rm = TRUE),
        #pitch = median(pitch, na.rm = T),
        #dynz = quantile(dynamicz, 0.5),
        vedba.q = quantile(vedba, 0.5),
        vedba.um = sum(vedba, na.rm = TRUE),
        coldist = quantile(coldist, 0.5, na.rm = TRUE),
        Depth = max(Depth, na.rm = TRUE),
        pT_dive = sum(in_dive)/dplyr::n()
      ) 
    print(paste("Finished reading bird",one_id, "at", Sys.time()))
    
    data <- rbind(data, temp)

}
print(paste("Finished joining birds", "at", Sys.time()))


saveRDS(data, 'C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 1/acc_data.RDS')


}

################################## load re run files from DRA 
rds_files <-list.files("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/",
                       pattern = ".RDS")
#rds_files<-rds_files[-grep("hmm", rds_files)]
#rds_files<-rds_files[-grep("processed", rds_files)]

data_compiled<- data.frame()
for (i in rds_files) {
  
  
  one_file<-readRDS(paste0("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/", i))
  
  data_compiled <- rbind(data_compiled, one_file)
}

summary(data_compiled)
unique(data_compiled$dep_id)

assign("data", data_compiled)
rm(data_compiled)
#######################################################

# this combines the interpolated GPS data with the acc data processed in the previous step
#data <- readRDS('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 1/acc_data.RDS')|> 
 # dplyr::rename(ID = dep_id)

##
data <- data|> 
  dplyr::rename(ID = dep_id)

# merge with interpolated gps data
#crwOut<-readRDS('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 1/crwOut.RDS')
myData <- momentuHMM::crawlMerge(crwOut, data, 'time')



# best predicted track data
Sys.time()
system.time({
  hmmData <- momentuHMM::prepData(myData, #type ='LL',
                                  centers =as.matrix(sf::st_coordinates(col_loc))
  )})
#  user  system elapsed 
#176.92  113.07  433.78 


hmmData$coldist2 <- hmmData$center1.dist

saveRDS(hmmData, 'C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/hmmData.RDS')

#check the axxy values with some new interpolation  values
for (d in unique(hmmData$ID)) {
  p <- hmmData |> 
    dplyr::filter(ID == d) |> 
    dplyr::select(ID, time, coldist, coldist2, step, vedba.q, vedba.sum, Depth, pT_dive, #pitch, 
                  wbf
    ) |> 
    tidyr::pivot_longer(cols = c(coldist2, coldist, step, vedba.q, vedba.sum, Depth, pT_dive, #pitch, 
                                 wbf
    )) |> 
    ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
    ggplot2::labs(x = 'Time', y = '', col = 'behaviour', title = d)
  print(p)
  readline('next') # hit esc at anytime to exit
}

#load data to feed the HMM fit models
hmmData<- readRDS('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/hmmData.RDS')



# ------
# Classify tracks to 5 behaviours
stateNames <- c('Commuting', 'Foraging','Resting','Colony')
nbStates <- length(stateNames)


# Make a binomial (bernoulli) variable for at the colony
hmmData$colony <- ifelse(hmmData$coldist2 <= 500, 1, 0)

hmmData$in_dive <- ifelse(hmmData$pT_dive >= 0.025, 1, 0)

hmmData$wbf2 <- ifelse(hmmData$wbf >= 4, 4, hmmData$wbf)

# distributions used to model the data streams
dist <- list(step="gamma", #angle = 'vm', 
             colony = 'bern', #vedba = 'gamma', 
             in_dive = 'bern', wbf2 = 'gamma')

#  starting values for dive
divePar <- c(0.0000000001, 0.75, 0.0000000001, 0.0000000001)

# starting values for step
stepPar <- c(800, 200, 175, 10,  # mean of step for each state
             250, 200, 250, 30) # sd of step for each state

#starting values for angle
anglePar <- c(10, 0.1, 0.1, 0.1) # sd of angle for each state

# starting values for colony
colPar <- c(0.0000000001, 0.0000000001, 0.0000000001, 0.9999999) # probabilities of being in state when colony == 1

# starting values for vedba
#vedbaPar <- c(0.6, 0.3, 0.05, 0.05, # mean of step for each state
 #             0.1, 0.2, 0.1, 0.1) # sd of step for each state


wbfPar <- c(mean = c(4, 4, 0.1, 0.01), 
            sd = c(0.1, 0.1, 0.1, 0.1)#, 
            #c(0.000001,0.001,0.99, 0.99) #maybe be careful with other birds 
) #  zero-mass included


{
temp <- hmmData |> 
  dplyr::filter(ID %in% unique(hmmData$ID)[1:5]) # sample a small number of tracks to do an initial model fit


st <- Sys.time() # this is to time how long it takes
st
m <- momentuHMM::fitHMM(
  data = temp, # data
  nbStates = nbStates, # state names
  dist = dist, # state distributions
  formula = ~1, # formula for transitions
  Par0 = list(step = stepPar, #angle = anglePar, 
              colony = colPar, #vedba = vedbaPar, 
              wbf2 = wbfPar, 
              in_dive = divePar), # starting values
  #fixPar = list(beta = fixPar),
  stateNames = stateNames #state names
)
Sys.time() - st
}

plot(m)
# predict behaviours from the model
temp$behaviour <- momentuHMM::viterbi(m)
temp$behaviour <- factor(temp$behaviour, labels = stateNames[sort(unique(temp$behaviour))])



# ------
# fit the model to all data using the starting parameters from test run
# this took 12 hours on my computer

bPar0 <- momentuHMM::getPar(m)

{
st <- Sys.time()
m <- momentuHMM::fitHMM(
  data = hmmData, # data
  nbStates = nbStates, # state names
  dist = dist, # state distributions
  Par0=bPar0$Par,
  beta0=bPar0$beta,
  delta0=bPar0$delta,
  formula=~1,
  #fixPar = list(beta = fixPar),
  #DM = DM,
  stateNames = stateNames #state names
)
Sys.time() - st
}

m
plot(m)

# predict behaviours from the model
hmmData$behaviour <- momentuHMM::viterbi(m)
hmmData$behaviour <- factor(hmmData$behaviour, labels = stateNames[sort(unique(hmmData$behaviour))])

# table showing the number of data points classified to each behaviour for each individual
table(hmmData$ID, hmmData$behaviour)

# save model outputs
saveRDS(m, 'C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/hmm_modelv2.RDS')
saveRDS(hmmData,'C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/hmm_predictionsv2.RDS')


hmmData<-readRDS('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/hmm_predictionsv2.RDS')
### check plotting for each ind

for (i in unique(hmmData$ID)){
  
  bp <- hmmData |>
    dplyr::as_tibble()|>
    dplyr::filter(ID == i) |> 
    dplyr::select(time, coldist2, wbf, pT_dive,step, behaviour) |> 
    tidyr::pivot_longer(cols = c('coldist2', 'wbf', 'pT_dive', 'step')) |> 
    ggplot2::ggplot(ggplot2::aes(x = behaviour, y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~name, ncol = 1, scales = 'free') +
    ggplot2::theme(text =  ggplot2::element_text(size = 10))
  
  
  tp <- hmmData |> 
    dplyr::as_tibble()|>
    dplyr::filter(ID == i) |> 
    dplyr::select(time, coldist2, wbf, pT_dive, step, behaviour) |> 
    tidyr::pivot_longer(cols = c('coldist2', 'wbf', 'pT_dive', 'step' )) |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(col = behaviour)) +
    ggplot2::facet_wrap(~name, ncol =1, scales = 'free') +
    ggplot2::theme(legend.position = c(0.10, 0.90),
                   legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 10))
  
  p <- cowplot::plot_grid(bp, tp)
  ggplot2::ggsave(paste0('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/HMM_', i,'_plots.png'), p, units = 'in', width = 10, height = 5)
  
  rm(m)
  print(paste("Finished", i, "at", format(Sys.time(), "%T")))
  
}

coords_utm<-hmmData[,c("x", "y")]
coords_utm <- sf::st_as_sf(coords_utm, coords = c('x', 'y'), crs = 5387) ## I am not reprojecting because this is the tropics
coords_latlon<- sf::st_transform(coords_utm,  crs =4326)


## INCORPORATE DAY AND NIGHT categories and Calculate DBA in the colony for each 
#move package old style to define day and night
elev<-solartime::computeSunPosition(lubridate::with_tz(hmmData$time, "America/Lima"), sf::st_coordinates(coords_latlon)[,2],sf::st_coordinates(coords_latlon)[,1])[,'elevation']
hist(elev)
DayNight <- c("Night",'Day')[1+(elev>0)]
table(DayNight)
hmmData$DayNight <- DayNight
unique(hmmData$ID)

hmmData|>
  dplyr::filter(ID == unique(hmmData$ID)[7])|>
  dplyr::mutate(
    time = lubridate::with_tz(time, "America/Lima")
  )|>
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = time, y = coldist2, color = as.factor(DayNight)))+
  ggplot2::scale_x_datetime(date_breaks = "1 hours")+
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  
data_comp <-hmmData|>
  dplyr::as_tibble()|>
  dplyr::filter(behaviour == "Colony") |>
  dplyr::group_by(ID)|>
  dplyr::summarise(
    DBAdayCol = sum(vedba.sum[which(DayNight == "Day")])/length(DayNight[DayNight == "Day"])/60,
    timeDay = length(DayNight[DayNight == "Day"])/60,
    DBAnightCol = sum(vedba.sum[which(DayNight == "Night")])/length(DayNight[DayNight == "Night"])/60,
    timeNight = length(DayNight[DayNight == "Night"])/60
  )|>
  tidyr::pivot_longer( cols = c("DBAdayCol", "DBAnightCol"), names_to = "DBAcat", values_to = "DBAvalue")

summary(lm( data = data_comp, DBAvalue ~ DBAcat))


hmmData|>
  dplyr::as_tibble()|>
  dplyr::filter(behaviour == "Colony") |>
  dplyr::group_by(ID)|>
  dplyr::summarise(
    DBAdayCol = sum(vedba.sum[which(DayNight == "Day")])/length(DayNight[DayNight == "Day"])/60,
    timeDay = length(DayNight[DayNight == "Day"])/60,
    DBAnightCol = sum(vedba.sum[which(DayNight == "Night")])/length(DayNight[DayNight == "Night"])/60,
    timeNight = length(DayNight[DayNight == "Night"])/60
  )|>
  tidyr::pivot_longer( cols = c("DBAdayCol", "DBAnightCol"), names_to = "DBAcat", values_to = "DBAvalue")|>
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = DBAcat, y = DBAvalue), 
                      position =  ggplot2::position_jitter(width = 0.2))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab("Hourly DBA at the colony")+
  ggplot2::scale_x_discrete(labels = c("Day", "Night"))+
  ggplot2::theme_bw()
