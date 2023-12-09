library(dplyr)
library(ggplot2)

flights <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/Supplementary_Materials_Appendix1_PECO (1).csv")
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
  filter(Flight.mode == "Gliding")|>
  mutate(
    Body.mass..g. = log10(Body.mass..g.) ,
    Metabolic.rate..W. = log10(Metabolic.rate..W.)
  )|>
ggplot(aes(x = Body.mass..g., y = Metabolic.rate..W., label = Species
           ))+
  geom_point(aes(x = Body.mass..g., y = Metabolic.rate..W., color = Species))+
  theme_bw()+
  geom_smooth(aes(x = Body.mass..g., y = Metabolic.rate..W.),color = "black", method= "lm")+
  scale_color_manual( values = c25)+
  geom_text(check_overlap = TRUE)+
  theme(legend.position = "none")+
  xlab("Log Body mass (g)")+
  ylab("Log Metabolic Rate (W)")+
  xlim(c(0.975, 4.25))

multispp_plot_fightCost
setwd("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/")
ggsave(multispp_plot_fightCost, filename = "plots/multispp_plot_fightCost.png", dpi = 300, width =9 , height = 5.55)
