library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(lattice)
library(cowplot)

#join DLW results and cassification results: DLW_sheet_Boobies_2019.csv

#load DLW sheet 
#dlw <- read.csv(file.choose())  # DLW_sheet_Boobies_2019v2.csv latest version
# C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy


#load classification calculations
#merged_summed2 <- read.csv(file.choose())
#"C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy\axxy_depth_peru\data\processed_acc_third_run\merged_summed2.csv"

#calculations <- merge(merged_summed2, dlw, by.x = "dep_id", by.y ="Bird", all = TRUE)

#merge deployments original file  with calculations for male and female variable
#raw_dep <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/05 GN 2019 Nov/deploymentsAxxys2019.csv", stringsAsFactors = F)

#raw_dep <- raw_dep[ , c("dp_ID", "sex")]
#be sure the rows are in the right order
#raw_dep$merge.col <- substr(raw_dep$dp_ID, 1, 7)

#calculations$merge.col <- substr(calculations$dep_id, 1, 7)


#calculationsraw_s <- merge(calculations, raw_dep, by = "merge.col")


#calculationsraw_s <-  calculationsraw_s %>% 
 # mutate(
  #  TotalVeDBA = DBACol+DBAFly+DBAFor+DBARest
#  ) 

#############################################################################################
#calculations<-calculationsraw_s
#calculations$TFlyFor <- calculations$TFly + calculations$TFor
#calculations$TColRest <- calculations$TRest + calculations$TCol

#calculations$pTFlyFor <- calculations$pTFly + calculations$pTFor
#calculations$pTColRest <- calculations$pTRest + calculations$pTCol

#calculations$pTFFR <- calculations$pTFly + calculations$pTFor +calculations$pTRest


#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

#calculations$pDBAFlyFor <- calculations$pDBAFly + calculations$pDBAFor
#calculations$pDBAColRest <- calculations$pDBARest + calculations$pDBACol

#calculations$pDBAFFR <- calculations$pDBAFly + calculations$pDBAFor + calculations$pDBARest

#calculations <- calculations %>% 
# mutate(
#  pDBA = TotalVeDBA/SampTime,
# TotalDEE = DEE.kJ.d*SampTime/24,
#TotalDEEg = TotalDEE/Mass.avg.
#)

#write.csv(calculations, file ="C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/calculations.csv", row.names = FALSE)


#already saved #July 28th with new caloric coef and fixed time for A06
calculationsraw_s <- read.csv(file.choose()) ##"C:\Users\francis van oordt\OneDrive - McGill University\Documents\McGill\00Res Prop v2\Chap 1 - DLW axxy\axxy_depth_peru\data\calculations.csv"



calculations <- calculationsraw_s #<- calculations

names(calculations)
head(calculations)
#including outlier bird

ggscatter(calculations[which(calculations$SampTime <70),], y = "DEE.kJ.d", #[which(calculations$SampTime <50),]
          x = "Time.total",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")#,
          #title = "DEE.KJ.d.g ~ Sampling time"
)

ggscatter(calculations[which(calculations$SampTime <70),], y = "DEE.KJ.d.g", #[which(calculations$SampTime <50),]
          x = "Time.total",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")#,
          #title = "DEE.KJ.d.g ~ Sampling time"
)

summary(calculations[ , c("DEE.KJ.d.g","DEE.kJ.d")])

df_to_pivot <- calculations[ , c("merge.col" ,"DEE.KJ.d.g","DEE.kJ.d", "sex")]

for_summary <- df_to_pivot %>% 
  rename(DEEg = DEE.KJ.d.g, DEE = DEE.kJ.d ) %>% 
  pivot_longer(cols =c(DEE,DEEg) , names_to = "Variable", values_to = "values") %>% 
  group_by(Variable) %>% 
  summarise(
    mean=mean(values), 
    sd=sd(values),
    min = min(values),
    max = max(values)
  )

for_summary <- as.data.frame(for_summary)

write.csv(for_summary, "DEE_summary.csv")

for_S_summary <- df_to_pivot %>% 
  rename(DEEg = DEE.KJ.d.g, DEE = DEE.kJ.d ) %>% 
  pivot_longer(cols =c(DEE,DEEg) , names_to = "Variable", values_to = "values") %>% 
  group_by(Variable, sex) %>% 
  summarise(
    mean=mean(values), 
    sd=sd(values),
    min = min(values),
    max = max(values)
  )

write.csv(for_S_summary, "DEE_summary_sex.csv")




#relationship VeDBA and DEE(g)
 A<-ggscatter(calculations, y = "DEE.kJ.d", #[which(calculations$SampTime <50),]
          x = "pDBA",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")#,
          #title = "DEE.KJ.d.g ~ Sampling time"
)

B<-ggscatter(calculations, y = "DEE.KJ.d.g", 
          x = "pDBA",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")#,
          #title = "DEE.KJ.d.g ~ Sampling time"
)

cowplot::plot_grid(A, B, labels = c("A", "B"))

#eliminate bird A06
calculations <- calculations %>% 
  filter(!Time.total > 100) #getting rid of 

#total DEE and sampling time

A<-ggscatter(calculations, y = "TotalDEE", #[which(calculations$SampTime <50),]
          x = "SampTime",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          #title = "DEE.KJ.d.g ~ Sampling time"
          xlab = "Sampling time (h)",
          ylab = "Total DEE"
)

B<-ggscatter(calculations, y = "TotalDEEg", #[which(calculations$SampTime <50),]
          x = "SampTime",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          #title = "DEE.KJ.d.g ~ Sampling time"
          xlab = "Sampling time (h)",
          ylab = "Total DEE/g"
)

C<-ggscatter(calculations, y = "TotalDEE", #[which(calculations$SampTime <50),]
          x = "TotalVeDBA",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          #title = "DEE.KJ.d.g ~ Sampling time",
          xlab = "Total VeDBA",
          ylab = "Total DEE"
)

D<-ggscatter(calculations, y = "TotalDEEg", #[which(calculations$SampTime <50),]
          x = "TotalVeDBA",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          #title = "DEE.KJ.d.g ~ Sampling time"
          xlab = "Total VeDBA",
          ylab = "Total DEE/g"
)


cowplot::plot_grid(A, B, C, D, labels = c("A", "B", "C", "D"))


figure1 <- cowplot::plot_grid(A, B, C+xlim(5500,12000), D+xlim(5500,12000), labels = c("A", "B", "C", "D"))

ggsave("figure1.png", figure1, width = 28, height = 16, units = "cm", dpi = 320)

#looking into correlated variables
GGally::ggpairs(calculations[ , c("TCol" ,"TFly" ,"TFor","TRest", "TFlyFor", "TColRest")])


#modeling relationship of T an DEE/g, with or without outliers
reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )

reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor + 0 )

reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor + 0)

reg4i <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFFR + 0)

#reg1i <- lm(data = calculations, formula = DEE.KJ.d.g ~ TCol)
#reg1ii <- lm(data = calculations, formula = DEE.KJ.d.g ~  TFly)
#reg1iii <- lm(data = calculations, formula = DEE.KJ.d.g ~ TFor)
#reg1iv <- lm(data = calculations, formula = DEE.KJ.d.g ~  TRest)

#reg2i <- lm(data = calculations, formula =DEE.KJ.d.g ~ TCol + TFly)
#reg3i <- lm(data = calculations, formula =DEE.KJ.d.g ~ TFor + TFly)
#reg4i <- lm(data = calculations, formula = DEE.KJ.d.g ~ TFlyFor)
#reg4ii <- lm(data = calculations, formula = DEE.KJ.d.g ~ TColRest)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)

#MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg1ii, reg1i, reg1iii, reg1iv, reg2i, reg3i, reg4i, reg4ii)


#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).


reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pDBACol + pDBAFly + pDBAFor + pDBARest)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pDBAColRest + pDBAFly + pDBAFor)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pDBAColRest + pDBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pDBACol + pDBARest +  pDBAFlyFor)
reg8i <- lm(data = calculations, formula = DEE.KJ.d.g ~ pDBACol +  pDBAFFR)
#reg5i <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol )
#reg5ii <- lm(data = calculations, formula = DEE.KJ.d.g ~DBAFly)
#reg5iii <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBAFor)
#reg5iv <- lm(data = calculations, formula = DEE.KJ.d.g ~  DBARest)

#reg6i <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol + DBAFly)
#reg7i <- lm(data = calculations, formula = DEE.KJ.d.g ~  DBAFly + DBAFor)
#reg7ii <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest)
#reg7iii <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAFlyFor)
           
MuMIn::model.sel(reg0, reg5, reg6,  reg7, reg8)

#MuMIn::model.sel(reg0, reg5, reg5i, reg5ii, reg5iii, reg5iv, reg6, reg6i,  reg7, reg7i, reg7ii, reg7iii, reg8)

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8,reg8i )


#MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg1ii, reg1iii, reg1iv, reg2i, reg3i, reg4i, reg4ii,
 #                reg5, reg5i, reg5ii, reg5iii, reg5iv, reg6, reg6i,  reg7, reg7i, reg7ii, reg7iii, reg8
     #            )

mods_full

summary(reg7)
summary(reg8i)

mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_full.csv")




calculations$predicted1<-predict(reg7, newdata = calculations[ , c("pDBAColRest", "pDBAFlyFor")])

C <- ggscatter(calculations, y = "DEE.KJ.d.g", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d.g ~ predicted (pDBAColRest + pDBAFlyPlu)"
)
C

calculations$predicted2<-predict(reg8i, newdata = calculations[ , c("pDBACol", "pDBAFFR")])

D <- ggscatter(calculations, y = "DEE.KJ.d.g", 
               x = "predicted2",
               color = "black", shape = 21, #size = 3, # Points color, shape and size
               add = "reg.line",  # Add regressin line
               add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
               conf.int = TRUE, # Add confidence interval
               cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                     label.sep = "\n"),
               title = "DEE.KJ.d.g ~ predicted (pDBACol+ pDBAFPR)"
)
D

cowplot::plot_grid(C, D, labels = c("A", "B"))


A <- ggscatter(calculations, y = "DEE.KJ.d.g", x = "pDBACol",
               color = "black", shape = 21, #size = 3, # Points color, shape and size
               add = "reg.line",  # Add regressin line
               add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
               conf.int = TRUE, # Add confidence interval
               cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                     label.sep = "\n"),
               title = "DEE.KJ.d.g and pDBACol"
)
B <- ggscatter(calculations, y = "DEE.KJ.d.g", x = "pDBAFFR",
               color = "black", shape = 21, #size = 3, # Points color, shape and size
               add = "reg.line",  # Add regressin line
               add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
               conf.int = TRUE, # Add confidence interval
               cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                     label.sep = "\n"),
               title = "DEE.KJ.d.g and pDBAFPR"
)

cowplot::plot_grid(A,B, labels = c("A", "B"))
###
#getting rid of outlier bird

calculations2 <- calculations %>% 
  dplyr::filter( !DEE.KJ.d.g > 1.2)


#calculations2$TFlyFor <- calculations2$TFly + calculations2$TFor
#calculations2$TColRest <- calculations2$TRest + calculations2$TCol

#############################################################################################3
reg0 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ 1 )


reg1 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + 0)
reg2 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor + 0 )
reg3 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor + 0)
reg4i <- lm(data = calculations2, formula = DEE.KJ.d.g ~ pTCol + pTFFR + 0)


MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)


#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$DBAFly + calculations2$DBAFor
#calculations2$DBAColRest <- calculations2$DBARest + calculations2$DBACol

reg5 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ pDBACol + pDBAFly + pDBAFor + pDBARest)
reg6 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ pDBAColRest + pDBAFly + pDBAFor)
reg7 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ pDBAColRest + pDBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ pDBACol + pDBARest +  pDBAFlyFor)
reg8i <- lm(data = calculations2, formula = DEE.KJ.d.g ~ pDBACol +  pDBAFFR)

MuMIn::model.sel(reg0, reg5, reg6,  reg7, reg8)



mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg4i, reg6, reg7, reg8, reg8i)
mods_clean

summary(reg3)

ggplot(calculations2, aes(TFly, DEE.KJ.d.g))+
  geom_point( )+
  geom_smooth(method = 'lm')

ggscatter(calculations2, y = "DEE.KJ.d.g", x = "TColRest",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d.g and TColRest (without outlier)"
)

ggscatter(calculations2, y = "DEE.KJ.d.g", x = "TFlyFor",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d.g and TFlyFor (without outlier)"
)



#write.csv(calculations, "calculations.csv")

mods_clean.df <- as.data.frame(mods_clean)
write.csv(mods_clean.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_clean.csv")




#### #### #### #### 
#### #### #### #### 
#### #### #### #### 
#### #### #### 
#### with total energy
#### #### #### 

calculations <- calculationsraw_s


ggscatter(calculations, y = "DEE.kJ.d", x = "Mass.avg.",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d and mass (all birds)"
)

#including outlier bird

#############################################################################################
reg0 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ 1 )


reg1 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ pTCol + pTFly + pTFor + pTRest + 0)
reg2 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ pTColRest + pTFly + pTFor + 0 )
reg3 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ pTColRest + pTFlyFor + 0 )
reg4 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ pTCol + pTRest +  pTFlyFor + 0)
reg4i <- lm(data = calculations, formula = log(DEE.kJ.d) ~ pTCol + pTFFR + 0)


MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg4i)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

reg5 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = log(DEE.kJ.d) ~ DBACol + DBARest +  DBAFlyFor)
reg8i <- lm(data = calculations, formula = log(DEE.kJ.d) ~ pDBACol +  pDBAFFR)


MuMIn::model.sel(reg0, reg5, reg6,  reg7, reg8)

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8, reg8i)

mods_full #BEST IS NULL MODEL

mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_full_DEEtot.csv")

summary(reg0)

### NULL MODEL IS BEST

calculations$logDEE.kJ.d <- log(calculations$DEE.kJ.d)
  
ggscatter(calculations, y = "logDEE.kJ.d", x = "pTColRest",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

ggscatter(calculations, y = "logDEE.kJ.d", x = "TFlyFor",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)


calculations$predicted1<-predict(reg3, newdata = calculations[ , c("TColRest",  "TFlyFor")])

ggscatter(calculations, y = "log(DEE.kJ.d)", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "log(DEE.kJ.d) and Pred TColRest + TFlyFor (with outlier)"
)


###
#getting rid of outlier bird

calculations2 <- calculationsraw_s %>% 
  dplyr::filter( !log(DEE.kJ.d) > 1500)

#calculations2$TFlyFor <- calculations2$TFly + calculations2$TFor
#calculations2$TColRest <- calculations2$TRest + calculations2$TCol

ggscatter(calculations2, y = "log(DEE.kJ.d)", x = "Mass.avg.",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "log(DEE.kJ.d) and mass (without outlier)"
)

#############################################################################################3
reg0 <- lm(data = calculations2, formula =DEE.kJ.d ~ 1 )


reg1 <- lm(data = calculations2, formula = DEE.kJ.d ~ TCol + TFly + TFor + TRest + 0)
reg2 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TFly + TFor + 0 )
reg3 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.kJ.d ~ TCol + TRest +  TFlyFor + 0)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$DBAFly + calculations2$DBAFor
#calculations2$DBAColRest <- calculations2$DBARest + calculations2$DBACol

reg5 <- lm(data = calculations2, formula = DEE.kJ.d ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.kJ.d ~ DBACol + DBARest +  DBAFlyFor)


MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_clean

summary(reg3)
plot(reg3)
plot(resid(reg3),(calculations2$DEE.kJ.d))


mods_clean.df <- as.data.frame(mods_clean)
write.csv(mods_clean.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_full_DEEtotClean.csv")


ggplot(calculations2, aes(TFly, DEE.kJ.d))+
  geom_point( )+
  geom_smooth(method = 'lm')

ggscatter(calculations2, y = "DEE.kJ.d", x = "TColRest",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d and TColRest (without outlier)"
)

ggscatter(calculations2, y = "DEE.kJ.d", x = "TFlyFor",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "DEE.KJ.d and TFlyFor (without outlier)"
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
#look into sex differences

ggscatter(calculationsraw_s, y = "DEE.KJ.d.g", 
          x = "Mass.avg.",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Males DEE.KJ.d.g and Pred DBAColRest DBAFlyFor (with outlier)",
          facet.by = "sex", 
          scales = "free"
)

#including outlier bird MALES
calculations <- calculationsraw_s %>% 
  dplyr::filter(sex == "M")

#############################################################################################

reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )


reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TCol + TFly + TFor + TRest + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFly + TFor + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TCol + TRest +  TFlyFor + 0)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol + DBARest +  DBAFlyFor)


MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8)

mods_full
summary(reg2)

mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df,"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Mfull.csv")

calculations$predicted1<-predict(reg2, newdata = calculations[ , c("TColRest","TFly","TFor")])

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

#############################################################################################
reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )


reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TCol + TFly + TFor + TRest + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFly + TFor + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ TCol + TRest +  TFlyFor + 0)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol + DBARest +  DBAFlyFor)


MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8)

mods_full
summary(reg3)
mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df,"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Ffull.csv")

calculations$predicted1<-predict(reg3, newdata = calculations[ , c("TColRest","TFlyFor")])

ggscatter(calculations, y = "DEE.KJ.d.g", 
          x = "predicted1",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Females DEE.KJ.d.g and Pred TColRest TFlyFor (with outlier)"
)


###
#getting rid of outlier bird

calculations2 <- calculationsraw_s %>% 
  dplyr::filter( !DEE.KJ.d.g > 1.2) %>% 
  dplyr::filter(sex == "M")
  

#calculations2$TFlyFor <- calculations2$TFly + calculations2$TFor
#calculations2$TColRest <- calculations2$TRest + calculations2$TCol

#############################################################################################3
reg0 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ 1 )

reg1 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ TCol + TFly + TFor + TRest + 0)
reg2 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TFly + TFor + 0 )
reg3 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ TCol + TRest +  TFlyFor + 0)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$DBAFly + calculations2$DBAFor
#calculations2$DBAColRest <- calculations2$DBARest + calculations2$DBACol

reg5 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations2, formula =DEE.KJ.d.g ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.KJ.d.g ~ DBACol + DBARest +  DBAFlyFor)


MuMIn::model.sel(reg0 , reg5, reg6, reg7, reg8)


mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_clean

mods_clean.df <- as.data.frame(mods_clean)
write.csv(mods_clean.df,"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Mclean.csv")

summary(reg3)
#### #### #### #### 
#### #### #### #### 
#### #### #### #### 
#### #### #### 
#### with total energy MALES AND FEMALES SEPARATELY
#### #### #### 

#males
calculations <- calculationsraw_s %>% 
  dplyr::filter(sex == "M")
  


#############################################################################################
reg0 <- lm(data = calculations, formula = DEE.kJ.d ~ 1 )

reg1 <- lm(data = calculations, formula = DEE.kJ.d ~ TCol + TFly + TFor + TRest + 0)
reg2 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TFly + TFor + 0 )
reg3 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.kJ.d ~ TCol + TRest +  TFlyFor+0)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

reg5 <- lm(data = calculations, formula = DEE.kJ.d ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.kJ.d ~ DBACol + DBARest +  DBAFlyFor)


MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_full

summary(reg3)
mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df , "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Mfull_DEE.csv")

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
reg0 <- lm(data = calculations, formula = DEE.kJ.d ~ 1 )

reg1 <- lm(data = calculations, formula = DEE.kJ.d ~ TCol + TFly + TFor + TRest + 0)
reg2 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TFly + TFor + 0 )
reg3 <- lm(data = calculations, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.kJ.d ~ TCol + TRest +  TFlyFor+0)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

reg5 <- lm(data = calculations, formula = DEE.kJ.d ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations, formula = DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.kJ.d ~ DBACol + DBARest +  DBAFlyFor)


MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_full

summary(reg3)

mods_full.df <- as.data.frame(mods_full)
write.csv(mods_full.df , "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Ffull_DEE.csv")


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
          title = "Females DEE.KJ.d and Pred TColRest + TFlyFor"
)









###
#getting rid of outlier bird

# MALES

calculations2 <- calculationsraw_s %>% 
  dplyr::filter( !DEE.kJ.d > 1500) %>% 
  dplyr::filter(sex == "M")

#calculations2$TFlyFor <- calculations2$TFly + calculations2$TFor
#calculations2$TColRest <- calculations2$TRest + calculations2$TCol



#############################################################################################3
reg0 <- lm(data = calculations2, formula =DEE.kJ.d ~ 1 )

reg1 <- lm(data = calculations2, formula = DEE.kJ.d ~ TCol + TFly + TFor + TRest + 0)
reg2 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TFly + TFor + 0 )
reg3 <- lm(data = calculations2, formula = DEE.kJ.d ~ TColRest + TFlyFor + 0 )
reg4 <- lm(data = calculations2, formula = DEE.kJ.d ~ TCol + TRest +  TFlyFor + 0)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)

#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations2$DBAFlyFor <- calculations2$DBAFly + calculations2$DBAFor
#calculations2$DBAColRest <- calculations2$DBARest + calculations2$DBACol

reg5 <- lm(data = calculations2, formula = DEE.kJ.d ~ DBACol + DBAFly + DBAFor + DBARest)
reg6 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + DBAFly + DBAFor)
reg7 <- lm(data = calculations2, formula =DEE.kJ.d ~ DBAColRest + DBAFlyFor )
reg8 <- lm(data = calculations2, formula = DEE.kJ.d ~ DBACol + DBARest +  DBAFlyFor)


MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


mods_clean <- MuMIn::model.sel(reg0, reg1, reg2, reg3,reg4, reg5, reg6, reg7, reg8)

mods_clean

summary(reg3)

mods_clean.df <- as.data.frame(mods_clean)
write.csv(mods_clean.df , "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Mclean_DEE.csv")


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

#differences in DEEg per sex

calculations <- calculationsraw_s

summary(lm(data= calculations, DEE.KJ.d.g ~ sex))

ggplot()+
  geom_boxplot(data= calculations, aes(y =DEE.KJ.d.g , x=sex))
        


summary(lm(data= calculations, DEE.kJ.d ~ sex))

ggplot()+
  geom_boxplot(data= calculations, aes(y =DEE.kJ.d , x=sex))


calculations2 <- calculationsraw_s %>% 
  dplyr::filter( !DEE.KJ.d.g > 1.2)

summary(lm(data= calculations2, DEE.KJ.d.g ~ sex))

ggplot()+
  geom_boxplot(data= calculations2, aes(y =DEE.KJ.d.g , x=sex))



summary(lm(data= calculations2, DEE.kJ.d ~ sex))

ggplot()+
  geom_boxplot(data= calculations2, aes(y =DEE.kJ.d , x=sex))
