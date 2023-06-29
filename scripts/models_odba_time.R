library(dplyr)
library(ggplot2)
library(ggpubr)

#join DLW results and cassification results: DLW_sheet_Boobies_2019.csv

#load DLW sheet 
#dlw <- read.csv(file.choose()) 
# C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy


#load classification calculations
#merged_summed2 <- read.csv(file.choose())
#"C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy\axxy_depth_peru\data\processed_acc_third_run\merged_summed2.csv"

#already saved 
calculationsraw <- read.csv(file.choose()) ##"C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy\axxy_depth_peru\data\processed_acc_third_run\calculations.csv"

calculations <- calculationsraw
#calculations <- merge(merged_summed2, dlw, by.x = "dep_id", by.y ="Bird", all = TRUE)

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

head(calculations)
#including outlier bird
#calculations$TFlyFor <- calculations$TimeFlying + calculations$TimeForaging
#calculations$TColRest <- calculations$TimeResting + calculations$TimeColony

#############################################################################################3
reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TimeColony + TimeResting +  TFlyFor + 0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$ODBAFlying + calculations$ODBAForaging
#calculations$DBAColRest <- calculations$ODBAResting + calculations$ODBAColony

reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg4, reg5, reg6)

reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_full
summary(reg7)

summary(reg4)

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


calculations$predicted1<-predict(reg7, newdata = calculations[ , c("DBAColRest", "DBAFlyFor")])

ggscatter(calculations, y = "DEE.KJ.d.g", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d.g and Pred DBAColRest DBAFlyFor (with outlier)"
)




###
#getting rid of outlier bird

calculations2 <- calculationsraw %>% 
  dplyr::filter( !DEE.KJ.d.g > 1.2)

#calculations2$TFlyFor <- calculations2$TimeFlying + calculations2$TimeForaging
#calculations2$TColRest <- calculations2$TimeResting + calculations2$TimeColony

#############################################################################################3

reg1 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ TimeColony + TimeResting +  TFlyFor + 0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$ODBAFlying + calculations2$ODBAForaging
#calculations2$DBAColRest <- calculations2$ODBAResting + calculations2$ODBAColony

reg5 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ 1 )

mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

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

calculations2$predicted1<-predict(reg3, newdata = calculations2[ , c("TColRest" , "TFlyFor")])

ggscatter(calculations2, y = "DEE.KJ.d.g", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d.g and Pred TColRest + TFlyFor (no outlier)"
)

#write.csv(calculations, "calculations.csv")


mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_full.csv")


mods_clean.df <- as.data.frame(mods_clean)
write.csv(mods_clean.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_clean.csv")

#### #### #### #### 
#### #### #### #### 
#### #### #### #### 
#### #### #### 
#### with total energy
#### #### #### 

calculations <- calculationsraw
summary(lm(data = calculations, DEE.kJ.d ~ TimeFlying))

ggplot(calculations, aes(TimeFlying, DEE.kJ.d))+
  geom_point( )+
  geom_smooth(method = 'lm')

ggscatter(calculations, y = "DEE.kJ.d", x = "TimeFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)


ggscatter(calculations, y = "DEE.kJ.d", x = "Mass.avg.",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

names(calculations)

head(calculations)
#including outlier bird
#calculations$TFlyFor <- calculations$TimeFlying + calculations$TimeForaging
#calculations$TColRest <- calculations$TimeResting + calculations$TimeColony

#############################################################################################3
reg1 <- lm(data = calculations, formula = DEE.kJ.d ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.kJ.d ~ TimeColony + TimeResting +  TFlyFor+0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$ODBAFlying + calculations$ODBAForaging
#calculations$DBAColRest <- calculations$ODBAResting + calculations$ODBAColony

reg5 <- lm(data = calculations, formula = DEE.kJ.d ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.kJ.d ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations, formula = DEE.kJ.d ~ 1 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)



### NULL MODEL IS BEST

ggscatter(calculations, y = "DEE.kJ.d", x = "DBAColRest",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)
ggscatter(calculations, y = "DEE.kJ.d", x = "DBAFlyFor",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)


calculations$predicted1<-predict(reg3, newdata = calculations[ , c("TColRest",  "TFlyFor")])

ggscatter(calculations, y = "DEE.kJ.d", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d and Pred TColRest + TFlyFor (with outlier)"
)


###
#getting rid of outlier bird

calculations2 <- calculationsraw %>% 
  dplyr::filter( !DEE.kJ.d > 1500)

#calculations2$TFlyFor <- calculations2$TimeFlying + calculations2$TimeForaging
#calculations2$TColRest <- calculations2$TimeResting + calculations2$TimeColony


ggscatter(calculations2, y = "DEE.kJ.d", x = "TimeFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
)


ggscatter(calculations2, y = "DEE.kJ.d", x = "Mass.avg.",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

#############################################################################################3

reg1 <- lm(data = calculations2, formula = DEE.kJ.d ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.kJ.d ~ TimeColony + TimeResting +  TFlyFor + 0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$ODBAFlying + calculations2$ODBAForaging
#calculations2$DBAColRest <- calculations2$ODBAResting + calculations2$ODBAColony

reg5 <- lm(data = calculations2, formula = DEE.kJ.d ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.kJ.d ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations2, formula =DEE.kJ.d ~ 1 )

mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_clean

summary(reg3)

ggplot(calculations2, aes(TimeFlying, DEE.kJ.d))+
  geom_point( )+
  geom_smooth(method = 'lm')

ggscatter(calculations2, y = "DEE.kJ.d", x = "TimeFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

ggscatter(calculations2, y = "DEE.kJ.d", x = "ODBAFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

calculations2$predicted1<-predict(reg3, newdata = calculations2[ , c("TColRest",  "TFlyFor")])

ggscatter(calculations2, y = "DEE.kJ.d", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d and Pred TColRest + TFlyFor (no outlier)"
)


#######################################################################################
####################################################################################### MALE AND FEMALE CALCS
#######################################################################################
#merge deployments original file  with calculations for male and female variable
raw_dep <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deploymentsAxxys2019.csv", stringsAsFactors = F)

#be sure the rows are in the right order
raw_dep$merge.col <- substr(raw_dep$dp_ID, 1, 7)
calculationsraw$merge.col <- substr(calculationsraw$dep_id, 1, 7)

calculationsraw_s <- merge(calculationsraw, raw_dep, by="merge.col")

#including outlier bird MALES
calculations <- calculationsraw_s %>% 
  dplyr::filter(sex == "M")

#############################################################################################3
reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TimeColony + TimeResting +  TFlyFor + 0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$ODBAFlying + calculations$ODBAForaging
#calculations$DBAColRest <- calculations$ODBAResting + calculations$ODBAColony

reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8)

mods_full
summary(reg3)


calculations$predicted1<-predict(reg2, newdata = calculations[ , c("TColRest","TimeFlying","TimeForaging")])

ggscatter(calculations, y = "DEE.KJ.d.g", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Males DEE.KJ.d.g and Pred DBAColRest DBAFlyFor (with outlier)"
)



calculations <- calculationsraw_s %>% 
  dplyr::filter(sex == "F")

#############################################################################################3
reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TimeColony + TimeResting +  TFlyFor + 0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$ODBAFlying + calculations$ODBAForaging
#calculations$DBAColRest <- calculations$ODBAResting + calculations$ODBAColony

reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8)

mods_full
summary(reg3)


###
#getting rid of outlier bird

calculations2 <- calculationsraw_s %>% 
  dplyr::filter( !DEE.KJ.d.g > 1.2) %>% 
  dplyr::filter(sex == "M")
  

#calculations2$TFlyFor <- calculations2$TimeFlying + calculations2$TimeForaging
#calculations2$TColRest <- calculations2$TimeResting + calculations2$TimeColony

#############################################################################################3

reg1 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ TimeColony + TimeResting +  TFlyFor + 0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$ODBAFlying + calculations2$ODBAForaging
#calculations2$DBAColRest <- calculations2$ODBAResting + calculations2$ODBAColony

reg5 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ 1 )

mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_clean



#### #### #### #### 
#### #### #### #### 
#### #### #### #### 
#### #### #### 
#### with total energy MALES AND FEMALES SEPARATELY
#### #### #### 

#males
calculations <- calculationsraw_s %>% 
  dplyr::filter(sex == "M")
  


#############################################################################################3
reg1 <- lm(data = calculations, formula = DEE.kJ.d ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.kJ.d ~ TimeColony + TimeResting +  TFlyFor+0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$ODBAFlying + calculations$ODBAForaging
#calculations$DBAColRest <- calculations$ODBAResting + calculations$ODBAColony

reg5 <- lm(data = calculations, formula = DEE.kJ.d ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.kJ.d ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations, formula = DEE.kJ.d ~ 1 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_full

### NULL MODEL IS BEST


calculations$predicted1<-predict(reg3, newdata = calculations[ , c("TColRest",  "TFlyFor")])

ggscatter(calculations, y = "DEE.kJ.d", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d and Pred TColRest + TFlyFor (with outlier)"
)



#Females

calculations <- calculationsraw_s %>% 
  dplyr::filter(sex == "F")



#############################################################################################3
reg1 <- lm(data = calculations, formula = DEE.kJ.d ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.kJ.d ~ TimeColony + TimeResting +  TFlyFor+0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$ODBAFlying + calculations$ODBAForaging
#calculations$DBAColRest <- calculations$ODBAResting + calculations$ODBAColony

reg5 <- lm(data = calculations, formula = DEE.kJ.d ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.kJ.d ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations, formula = DEE.kJ.d ~ 1 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_full


calculations$predicted1<-predict(reg3, newdata = calculations[ , c("TColRest",  "TFlyFor")])

ggscatter(calculations, y = "DEE.kJ.d", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d and Pred TColRest + TFlyFor (with outlier)"
)









###
#getting rid of outlier bird

# MALES

calculations2 <- calculationsraw_s %>% 
  dplyr::filter( !DEE.kJ.d > 1500) %>% 
  dplyr::filter(sex == "M")

#calculations2$TFlyFor <- calculations2$TimeFlying + calculations2$TimeForaging
#calculations2$TColRest <- calculations2$TimeResting + calculations2$TimeColony



#############################################################################################3

reg1 <- lm(data = calculations2, formula = DEE.kJ.d ~ TimeColony + TimeFlying + TimeForaging + TimeResting + 0)
reg2 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TimeFlying + TimeForaging + 0 )
reg3 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.kJ.d ~ TimeColony + TimeResting +  TFlyFor + 0)

MuMIn::model.sel(reg1, reg2, reg3, reg4)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$ODBAFlying + calculations2$ODBAForaging
#calculations2$DBAColRest <- calculations2$ODBAResting + calculations2$ODBAColony

reg5 <- lm(data = calculations2, formula = DEE.kJ.d ~ ODBAColony + ODBAFlying + ODBAForaging + ODBAResting)
reg6 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + ODBAFlying + ODBAForaging)
reg7 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.kJ.d ~ ODBAColony + ODBAResting +  DBAFlyFor)


MuMIn::model.sel(reg5, reg6, reg7, reg8)

reg0 <- lm(data = calculations2, formula =DEE.kJ.d ~ 1 )

mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_clean

summary(reg3)



calculations2$predicted1<-predict(reg3, newdata = calculations2[ , c("TColRest",  "TFlyFor")])

ggscatter(calculations2, y = "DEE.kJ.d", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Males DEE.KJ.d and Pred TColRest + TFlyFor (no outlier)"
)
