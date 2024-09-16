
#Re-plotting 3 sec wind DBA, 1 sec sum, 1 min grouping, new classes, run for example on 1 bird

# devtools::install_github('mansueto-institute/ggsn') # nee to instal branched version with no maptools dependency


#getwd()
#list.dirs()

#setwd("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run")
#list.files()

#allfiles<- list.files(pattern = ".csv")
alldat<-readRDS('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/new HMM classification run 3 prop dive/hmm_predictionsv2.RDS')

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
  dplyr::filter(ID == i) |> 
  dplyr::select(time, "Dist. to the colony" = coldist, "Wing beat freq."= wbf, Depth, "Step length" = step, behaviour) |>
 # dplyr::rename(depth = Depth)|>
  dplyr::mutate(time = as.POSIXct(time, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))|> 
  dplyr::mutate(time = lubridate::with_tz(time, "America/Lima")) |>
  tidyr::pivot_longer(cols = c('Dist. to the colony', 'Wing beat freq.', 'Depth', "Step length" )) |>
  ggplot2::ggplot(ggplot2::aes(x = time, y = value), color = cbbPalette) +
  ggplot2::geom_line() +
  ggplot2::geom_point(ggplot2::aes(col = behaviour)) +
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
                      labels = c("Colony",
                                 "Commuting",
                                 "Foraging",
                                 "Resting"),
                      breaks = c("Colony", "Commuting", "Foraging", "Resting"))+
  
  ggplot2::theme_bw()+
  ggplot2::theme( legend.text=ggplot2::element_text(size=20),
                  text= ggplot2::element_text(size=20),
                  legend.position = "top"#,
                 #legend.box.background = ggplot2::element_rect(color = "black")
                 ) #c(0.75, 0.55) # could reposition with the coordinates
   #could remove the box line 
tp


#ggplot2::ggsave(paste0('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/plots/',i,'_plots_pubv3.png'), tp, units = 'in', width = 10, height = 8)


### sample mapping attempt
Peru <- sf::st_read("C:/Users/francis van oordt/Documents/Research/2021/Categorizacion 2020/chuita/gadm36_PER_shp/gadm36_PER_0.shp")
#map_sample<- 
  one_bird <- alldat |>
  dplyr::filter(ID == i) 
  
  one_bird_map <- one_bird |>
  dplyr::select(time, x, y, #coldist, wbf, Depth, odba, 
                behaviour)|>
  dplyr::mutate(time = as.POSIXct(time, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))|> 
  dplyr::mutate(time = lubridate::with_tz(time, "America/Lima")) |> 
  #tidyr::pivot_longer(cols = c('coldist', 'wbf', 'Depth')) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point(data = one_bird[which(one_bird$behaviour != "Commuting"),], ggplot2::aes(x = x, y = y, color = behaviour))+
  ggplot2::geom_sf(data = sf::st_transform(Peru, crs = 5387))+
  #ggplot2::geom_point(ggplot2::aes(col = HMM)) +
  
  #ggplot2::facet_grid(rows = ggplot2::vars(name), scales = 'free') +
  #ggplot2::theme(legend.position = c(0.10, 0.90),
  #              legend.background = element_blank(),http://127.0.0.1:21349/graphics/34aace5a-7370-433b-a077-499b1150736b.png
  #             legend.key = element_blank(),
  #            text = element_text(size = 10))+
  #ggtitle(i)+
  ggplot2::scale_colour_manual(values=cbPalette,
                      #name = "Behaviour",
                      #labels = c("Colony (Coldist < 1 km)",
                       #          "Flying (WBF > 4)",
                        #         "Plunging (Depth > 0.5 m)",
                         #       "Resting (Coldist > 1 km, WBF < 4)"),
                      breaks = c("Colony", "Flying", "Foraging", "Resting"))+
    ggplot2::theme_bw()+
    ggplot2::coord_sf( xlim = range(one_bird$x), ylim= range(one_bird$y))+
    ggplot2::theme( 
        text= ggplot2::element_text(size=20),
        legend.position = "none")+ #c(0.75, 0.55) # could repositi#on with the coordinates

    ggplot2::ylab(NULL)+
    ggplot2::xlab(NULL)+ 
    ggplot2::scale_x_continuous(breaks = seq(-79.25, -78, by = 0.15))+
    ggsn::scalebar(x.min = range(one_bird$x)[1],
                   x.max = range(one_bird$x)[2],
                   y.min = range(one_bird$y)[1],
                   y.max = range(one_bird$y)[2], 
                   dist = 5, transform = FALSE,# model = "WGS84", 
                   dist_unit = "km", 
                   location = "bottomleft")+
    ggplot2::annotate("text", x = 33255.47, y = 9064144, label = "PACIFIC OCEAN", angle='-45', size = 6)+
  ggplot2::annotate("text", x = 60255.47, y = 9100000, label = "PERU", size = 6)
  
one_bird_map

#ggplot2::ggsave(paste0('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/plots/one_bird_mapv2.png'), one_bird_map, units = 'in', width = 4, height = 6)

  
merge_class_plots <- cowplot::plot_grid(tp,one_bird_map, nrow = 1, labels = c('A', 'B'), rel_widths = c(1.75, 1))

ggplot2::ggsave(paste0('C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/plots/merge_class_plotsv2.png'), 
       merge_class_plots, units = 'in', width = 19, height = 9, bg = "white")
