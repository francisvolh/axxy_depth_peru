library(dplyr)
library(ggplot2)
library(jpeg)
library(patchwork)
#Re-plotting third run for example on 1 bird

#getwd()
#list.dirs()

#setwd("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run")
#list.files()

#allfiles<- list.files(pattern = ".csv")
alldat <- read.csv("data/processed_acc_third_run/all_birds_HMMS.csv")

#alldat$time<- as.POSIXct(alldat$time)

#freq <- seabiRds::getFrequency(alldat$time)

#plot all birds to check which is a good example - NOT NEEDED FOR THE PUB
{names(alldat)
plotlist<-list()
for (i in unique(alldat$dep_id)) {
  
  
 # bp <- alldat %>% 
  #  filter(dep_id == i) %>% 
   # dplyr::select(time, coldist, wbf, Depth, HMM) %>% 
    #tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth')) %>% 
    #ggplot2::ggplot(ggplot2::aes(x = HMM, y = value)) +
    #ggplot2::geom_boxplot() +
    #ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
    #ggplot2::theme(text = element_text(size = 10))
  
  
  tp <- alldat %>% 
    filter(dep_id == i) %>% 
    dplyr::select(time, coldist, wbf, Depth, odba, HMM) %>% 
    dplyr::mutate(time = as.POSIXct(time, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))%>% 
    tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth' )) %>% 
    ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(col = HMM)) +
    ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
    #ggplot2::theme(legend.position = c(0.10, 0.90),
     #              legend.background = element_blank(),http://127.0.0.1:21349/graphics/34aace5a-7370-433b-a077-499b1150736b.png
      #             legend.key = element_blank(),
       #            text = element_text(size = 10))+
    ggtitle(i)
  
  plotlist[[i]]<-tp
  
  #p <- cowplot::plot_grid(bp, tp)
  #ggsave(paste0(i,'_plots_trials.png'), p, units = 'in', width = 10, height = 5)
  
 
}

all_birds <- cowplot::plot_grid(plotlist =plotlist)
}
#ggsave("all_birds_plots_trials.png", all_birds,  units = 'in', width = 30, height = 15)

#pick A09 as sample for pub
###################################################################################################
#coloblind palette

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

i<-"A09PEBO_20191116_A31"
tp <- alldat |> 
  dplyr::filter(dep_id == i) |> 
  dplyr::select(time, "Distance to the colony" = coldist, "Wing beat frequency"= wbf, Depth, odba, HMM) |>
 # dplyr::rename(depth = Depth)|>
  dplyr::mutate(time = as.POSIXct(time, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))|> 
  dplyr::mutate(time = lubridate::with_tz(time, "America/Lima")) |>
  tidyr::pivot_longer(cols = c('Distance to the colony', 'Wing beat frequency', 'Depth' )) |>
  ggplot2::ggplot(ggplot2::aes(x = time, y = value), color = cbbPalette) +
  ggplot2::geom_line() +
  ggplot2::geom_point(ggplot2::aes(col = HMM)) +
  ggplot2::ylab(NULL)+
  ggplot2::xlab(NULL)+
  ggplot2::facet_grid(rows = ggplot2::vars(name), 
                      scales = 'free',  
                      switch = "y") +
  #ggplot2::theme(legend.position = c(0.10, 0.90),
  #              legend.background = element_blank(),http://127.0.0.1:21349/graphics/34aace5a-7370-433b-a077-499b1150736b.png
  #             legend.key = element_blank(),
  #            text = element_text(size = 10))+
  #ggtitle(i)+
  ggplot2::scale_colour_manual(values=cbPalette,
                      name = NULL,
                      labels = c("Colony (Dist. to col. < 1 km)",
                                 "Flying (WBF > 4)",
                                 "Plunging (Depth > 0.5 m)",
                                 "Resting (Dist. to col. > 1 km, WBF < 4)"),
                      breaks = c("Colony", "Flying", "Foraging", "Resting"))+
  
  ggplot2::theme_bw()+
  ggplot2::theme( legend.text=ggplot2::element_text(size=14),
                  text= ggplot2::element_text(size=16),
                  legend.position = "top"#,
                 #legend.box.background = ggplot2::element_rect(color = "black")
                 ) #c(0.75, 0.55) # could reposition with the coordinates
   #could remove the box line 
tp


#ggplot2::ggsave(paste0('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/plots/',i,'_plots_pubv3.png'), tp, units = 'in', width = 10, height = 8)


#plunge <- readJPEG("C:/Users/francis van oordt/Downloads/365328918_312005517941197_6014959739507994660_n.jpg",
  #                 native = TRUE)
#colony <- readJPEG("C:/Users/francis van oordt/Downloads/364657304_1775269122878458_5968267506518036350_n.jpg",
   #                native = TRUE)
#tp + 
 # inset_element(plunge, left = 0.1,
  #              bottom = 0.919,
   #             right = 0.17,
    #            top = .999) +
  #inset_element(colony, 
   #             left = .4,
    #            bottom = 0.7,
     #           right = .3,
      #          top = .8)


### sample mapping attempt
Peru <- sf::st_read("C:/Users/francis van oordt/Documents/Research/2021/Categorizacion 2020/chuita/gadm36_PER_shp/gadm36_PER_0.shp")
#map_sample<- 
  one_bird<-alldat |>
  dplyr::filter(dep_id == i) 
  
  one_bird_map <-one_bird |>
  dplyr::select(time, lon, lat, #coldist, wbf, Depth, odba, 
                HMM)|>
  dplyr::mutate(time = as.POSIXct(time, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))|> 
  dplyr::mutate(time = lubridate::with_tz(time, "America/Lima")) |> 
  #tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth')) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x = lon, y = lat)) +
  ggplot2::geom_point(data = one_bird[which(one_bird$HMM != "Flying"),], ggplot2::aes(x = lon, y = lat, color = HMM))+
  ggplot2::geom_sf(data = Peru)+
  #ggplot2::geom_point(ggplot2::aes(col = HMM)) +
  
  #ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
  #ggplot2::theme(legend.position = c(0.10, 0.90),
  #              legend.background = element_blank(),http://127.0.0.1:21349/graphics/34aace5a-7370-433b-a077-499b1150736b.png
  #             legend.key = element_blank(),
  #            text = element_text(size = 10))+
  #ggtitle(i)+
  ggplot2::scale_colour_manual(values=cbPalette,
                      name = "Behaviour",
                      labels = c("Colony (Coldist < 1 km)",
                                 "Flying (WBF > 4)",
                                 "Plunging (Depth > 0.5 m)",
                                 "Resting (Coldist > 1 km, WBF < 4)"),
                      breaks = c("Colony", "Flying", "Foraging", "Resting"))+
    ggplot2::theme_bw()+
    ggplot2::coord_sf( xlim = range(alldat[which(alldat$dep_id == i),]$lon), ylim= range(alldat[which(alldat$dep_id == i),]$lat))+
    ggplot2::theme( 
        text= ggplot2::element_text(size=15),
        legend.position = "none")+ #c(0.75, 0.55) # could repositi#on with the coordinates

    ggplot2::ylab(NULL)+
    ggplot2::xlab(NULL)+ 
    ggplot2::scale_x_continuous(breaks=c(-79.25, -79.15, -79))+
    ggsn::scalebar(x.min = range(alldat[which(alldat$dep_id == i),]$lon)[1],
                   x.max = range(alldat[which(alldat$dep_id == i),]$lon)[2],
                   y.min = range(alldat[which(alldat$dep_id == i),]$lat)[1],
                   y.max = range(alldat[which(alldat$dep_id == i),]$lat)[2], 
                   dist = 5, transform = TRUE, model = "WGS84", 
                   dist_unit = "km", 
                   location = "bottomleft")+
    ggplot2::annotate("text", x = -79.225, y = -8.15, label = "PACIFIC OCEAN", angle='-45', size = 6)+
  ggplot2::annotate("text", x = -79., y = -8.125, label = "PERU", size = 6)
  
one_bird_map

#ggplot2::ggsave(paste0('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/plots/one_bird_mapv2.png'), one_bird_map, units = 'in', width = 4, height = 6)

  
merge_class_plots <- cowplot::plot_grid(tp,one_bird_map, nrow = 1, labels = c('A', 'B'), rel_widths = c(1.75, 1))

ggplot2::ggsave(paste0('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/plots/merge_class_plotsv2.png'), 
       merge_class_plots, units = 'in', width = 19, height = 9, bg = "white")
