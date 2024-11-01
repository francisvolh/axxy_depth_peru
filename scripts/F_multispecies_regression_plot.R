library(dplyr)
library(ggplot2)

flights <- read.csv("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/Supplementary_Materials_Appendix1_PECO (1).csv")
c25 <- c(
  "dodgerblue2", 
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "steelblue4", 
  "black", 
  "gold1",
  "skyblue2", 
  "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  #"gray70",
  #"khaki2",
  "maroon",
  "orchid1", 
  "deeppink1", 
  "blue1", 
  "darkturquoise",
  "green1", 
  #"yellow4",
  "yellow3",
  "darkorange4", 
  "brown"
)


multispp_plot_fightCost<-flights|>
  dplyr::filter(Flight.mode == "Gliding")|>
  dplyr::mutate(
    Body.mass..g. = log10(Body.mass..g.) ,
    Metabolic.rate..W. = log10(Metabolic.rate..W.)
  )|>
  ggplot2::ggplot(ggplot2::aes(x = Body.mass..g., y = Metabolic.rate..W., label = Species
           ))+
  ggplot2::geom_point(ggplot2::aes(x = Body.mass..g., y = Metabolic.rate..W., color = Species))+
  ggplot2::theme_bw()+
  ggplot2::geom_smooth(ggplot2::aes(x = Body.mass..g., y = Metabolic.rate..W.),color = "black", method= "lm")+
  ggplot2::scale_color_manual( values = c25)+
  #ggplot2::geom_text(check_overlap = TRUE)+
  ggplot2::geom_point(data = flights[which(flights$Species == "Peruvian booby"),], 
                      ggplot2::aes(x = log10(Body.mass..g.), 
                                   y = log10(Metabolic.rate..W.)
                      ), cex = 5, color = "blue", fill="green", shape = 23)+
  
  ggrepel::geom_text_repel(ggplot2::aes(x = Body.mass..g., 
                               y = Metabolic.rate..W., 
                               label = Species), 
                           box.padding = 0.5)+
  ggplot2::theme(legend.position = "none")+
  ggplot2::xlab("Log Body mass (g)")+
  ggplot2::ylab("Log Metabolic Rate (W)")+
  ggplot2::xlim(c(0.975, 4.25))+
  ggplot2::theme( text= ggplot2::element_text(size=18))

multispp_plot_fightCost
setwd("C:/Users/francis van oordt/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/")
ggplot2::ggsave(multispp_plot_fightCost, filename = "plots/multispp_plot_fightCost.png", dpi = 300, width =9 , height = 6.55)
