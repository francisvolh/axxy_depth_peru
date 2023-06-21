library(dplyr)
library(ggplot2)
library(ggpubr)

#join DLW results and cassification results: DLW_sheet_Boobies_2019.csv

#load DLW sheet 
dlw <- read.csv(file.choose()) 
# C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy


#load classification calculations
merged_summed2 <- read.csv(file.choose())
#"C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy\axxy_depth_peru\data\processed_acc_third_run\merged_summed2.csv"


calculations <- merge(merged_summed2, dlw, by.x = "dep_id", by.y ="Bird", all = TRUE)

summary(lm(data = calculations, DEE.KJ.d.g ~ TimeFlying))

ggplot(calculations, aes(TimeFlying, DEE.KJ.d.g))+
  geom_point( )+
  geom_smooth(method = 'lm')

ggscatter(calculations, y = "DEE.KJ.d.g", x = "TimeFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)
names(calculations)


#including outlier bird
calculations$TFlyFor <- calculations$TimeFlying + calculations$TimeForaging
calculations$TColRest <- calculations$TimeResting + calculations$TimeColony

#############################################################################################3

reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )

MuMIn::model.sel(reg1, reg2, reg3)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

calculations$DBAFlyFor <- calculations$ODBAFlying + calculations$ODBAForaging
calculations$DBAColRest <- calculations$ODBAResting + calculations$ODBAColony

reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg5 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + ODBAFlying + ODBAForaging)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )


MuMIn::model.sel(reg4, reg5, reg6)

reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6)

summary(reg6)

ggscatter(calculations, y = "DEE.KJ.d.g", x = "DBAColRest",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)
ggscatter(calculations, y = "DEE.KJ.d.g", x = "DBAFlyFor",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

###
#getting rid of outlier bird

calculations2 <- calculations %>% 
  dplyr::filter( !DEE.KJ.d.g > 1.2)

calculations2$TFlyFor <- calculations2$TimeFlying + calculations2$TimeForaging
calculations2$TColRest <- calculations2$TimeResting + calculations2$TimeColony

#############################################################################################3

reg1 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )

MuMIn::model.sel(reg1, reg2, reg3)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

calculations2$DBAFlyFor <- calculations2$ODBAFlying + calculations2$ODBAForaging
calculations2$DBAColRest <- calculations2$ODBAResting + calculations2$ODBAColony

reg4 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg5 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + ODBAFlying + ODBAForaging)
reg6 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )

MuMIn::model.sel(reg4, reg5, reg6)

reg0 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ 1 )

mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6)

mods_clean

summary(reg3)

ggplot(calculations2, aes(TimeFlying, DEE.KJ.d.g))+
  geom_point( )+
  geom_smooth(method = 'lm')

ggscatter(calculations2, y = "DEE.KJ.d.g", x = "TimeFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

ggscatter(calculations2, y = "DEE.KJ.d.g", x = "ODBAFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_full.csv")


mods_clean.df <- as.data.frame(mods_clean)
write.csv(mods_clean.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_clean.csv")
