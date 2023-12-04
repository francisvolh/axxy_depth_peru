library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(lattice)
library(cowplot)
library(MuMIn)

set.seed(12345)


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
#calculationsraw_s <- read.csv(file.choose()) 
calculationsraw_s <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/calculations.csv")

calculations <- calculationsraw_s #<- calculations


# get some time sampling values numbers (Excluding long deployed bird, which will be excluded for all calculations later as well) 
calculations[which(calculations$Time.total <100),] %>% 
  summarise(
    meanTime = mean(Time.total),
    maxTime = max(Time.total),
    minTime = min(Time.total),
    stdTime = sd(Time.total)
  )

#because pDBA total and for each activity was the total DBA per deployment divided by sampling time, 
# I had to make it daily multiplying by 24h
#vars1 <- c("pDBACol", "pDBAFly", "pDBAFor", "pDBARest", "pDBA", "pDBAFlyFor", "pDBAColRest","pDBAFFR")

#calculations <- calculations %>% 
 # mutate(
  #  across(all_of(vars1), ~.x*24, .names = paste0("d","{col}"))#total time and ODBA of each activty divided by total Sampling time
  #)

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

#write.csv(for_summary, "DEE_summary.csv")

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

#write.csv(for_S_summary, "DEE_summary_sex.csv")

#checking for sex differences in DEEg
dee.g.lm <- lm(data = calculations, DEE.KJ.d.g ~ sex)

summary(dee.g.lm)

plot(resid(dee.g.lm))
boxplot(resid(dee.g.lm)~ calculations$sex)

car::leveneTest(resid(dee.g.lm)~ calculations$sex)

car::influencePlot(dee.g.lm)

deeg.pred <- ggeffects::ggpredict(
  dee.g.lm,
  terms = c("sex"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
)

ggplot(data = calculations)+
  geom_point( aes(y = DEE.KJ.d.g, x = sex) )


ggplot(data = calculations)+ #, color = Spec
  geom_point(aes(y = DEE.KJ.d.g, x = sex), position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggpredict(
    dee.g.lm,
    terms = c("sex"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ) ,
                  aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab("Sex")+
  ylab("DEE/g")+
  guides(color = "none")+
  theme_bw()



ggplot(data = calculations)+ #, color = Spec
  geom_point(aes(y = DEE.kJ.d, x = sex), position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  xlab("Sex")+
  ylab("DEE")+
  guides(color = "none")+
  theme_bw()

####
#### MUST DO bird has not a full axxy cycle
####
#eliminate bird A06
calculations <- calculations %>% 
  filter(!Time.total > 100) #getting rid of 


####################################################################### 
# models for time budget

#looking into correlated variables
#GGally::ggpairs(calculations[ , c("pTCol" ,"pTFly" ,"pTFor","pTRest", "pTFlyFor", "pTColRest")])

#GGally::ggpairs(calculations[ , c("dpDBACol" ,"dpDBAFly" ,"dpDBAFor", "dpDBARest", "dpDBAColRest","dpDBAFlyFor")])

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

#MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)

#MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg1ii, reg1i, reg1iii, reg1iv, reg2i, reg3i, reg4i, reg4ii)


#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).


reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest + 0)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly + dpDBAFor)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFlyFor)
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +  dpDBAFlyFor)
reg8i <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol +  dpDBAFFR)

#reg9i <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol )
#reg9ii <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBAFly )
#reg9iii <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBAFor)
#reg9iv <- lm(data = calculations, formula = DEE.KJ.d.g ~  dpDBARest)
reg10 <- lm(data = calculations, formula = DEE.KJ.d.g ~  dpDBA)

#reg5i <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol )
#reg5ii <- lm(data = calculations, formula = DEE.KJ.d.g ~DBAFly)
#reg5iii <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBAFor)
#reg5iv <- lm(data = calculations, formula = DEE.KJ.d.g ~  DBARest)

#reg6i <- lm(data = calculations, formula = DEE.KJ.d.g ~ DBACol + DBAFly)
#reg7i <- lm(data = calculations, formula = DEE.KJ.d.g ~  DBAFly + DBAFor)
#reg7ii <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAColRest)
#reg7iii <- lm(data = calculations, formula =DEE.KJ.d.g ~ DBAFlyFor)
           
#MuMIn::model.sel(reg0, reg5, reg6,  reg7, reg8)

#MuMIn::model.sel(reg0, reg5, reg5i, reg5ii, reg5iii, reg5iv, reg6, reg6i,  reg7, reg7i, reg7ii, reg7iii, reg8)

MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8,reg8i, reg10 )

mods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8,reg8i , reg10)

#including model with 1 parameter only for DBA, NOT IN THE PAPER
#MuMIn::model.sel(reg0, reg2, reg3, reg4, reg4i, reg6, reg7, reg8,reg8i, reg9i, reg9ii, reg9iii, reg9iv , reg10)

#MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg1ii, reg1iii, reg1iv, reg2i, reg3i, reg4i, reg4ii,
 #                reg5, reg5i, reg5ii, reg5iii, reg5iv, reg6, reg6i,  reg7, reg7i, reg7ii, reg7iii, reg8
     #            )

mods_full

summary(reg8i) # besta ranked model of all the 2 parameter or more
summary(reg7)

mods_full.df <- as.data.frame(mods_full)
#write.csv(mods_full.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_full.csv")

#model prediction for each paramter of the best model
# paramter 1 at Colony
mod_pred_1A<-ggeffects::ggpredict(
  reg8i,
  terms = c("dpDBACol"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
) #asking to predict only with 1 term to make the plot better, modify if needed


mod_pred1_A_PLOT <- plot(mod_pred_1A, add.data = TRUE, show.title = FALSE, colors = "bw")+
  labs(
    x = "DBA at the colony",
    y = "Mass-specific Daily Energy Expenditure (kJ/d*g)"
  )
mod_pred1_A_PLOT


#################trying with ggplot only############################
# plot of model model predictions -  NOT WORKING nicely

#ggplot(data = calculations)+ #, color = Spec
 # geom_point(aes(y = DEE.KJ.d.g, x = dpDBACol),# position = position_jitter(height = 0, width = 0.1),
 #            alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
#  geom_smooth(data=mod_pred_1A,
#  aes(x = x, y = predicted),
 # method = "lm"
  #  )+
#  xlab("dpDBACol")+
 # ylab("DEE.KJ.d.g")+
  #guides(color = "none")+
  #theme_bw()
###############################################################

mod_pred_1B<-ggeffects::ggpredict(
  reg8i,
  terms = c("dpDBAFFR"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
) #asking to predict only with 1 term to make the plot better, modify if needed

mod_pred_1B_PLOT <- plot(mod_pred_1B, add.data = TRUE, show.title = FALSE, colors = "bw")+
  labs(
    x = "DBA outsite of the colony (Flying, Plunging, Resting)",
    y = "Mass-specific Daily Energy Expenditure (kJ/d*g)"
  )
mod_pred_1B_PLOT
#plot(mod_pred_1, colors = "bw", ci = TRUE, ci.style = "dash")

#two plots with one plot indepently per parameter predictions
cowplot::plot_grid(mod_pred1_A_PLOT, mod_pred_1B_PLOT)
predic.two.models <- cowplot::plot_grid(mod_pred1_A_PLOT, mod_pred_1B_PLOT)

#ggsave("plots/predic.twoPar.models.png", predic.two.models, dpi = 300, bg = "white", units = 'in', width = 15, height = 6)

##############################################################

#alternative 2 for model prediction graph with both terms together

mod_pred_2<-ggeffects::ggpredict(
  reg7,
  terms = c("dpDBAColRest","dpDBAFlyFor"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
)
  

mod_pred_2_PLOT <- plot(mod_pred_2, add.data = TRUE, show.title = FALSE)+labs(
  x = "DBA at the colony",
  y = "Mass-specific Daily Energy Expenditure (kJ/d*g)",
  colour = "DBA away from Colony"
)
mod_pred_2_PLOT



#summary(reg8)
#ggeffects::ggpredict(
 # reg8,
  #terms = c(" dpDBACol","dpDBARest", "dpDBAFlyFor"),
  #ci.lvl = 0.95,
  #type = "fe",
  #back.transform= FALSE, 
  #typical = "mean"
#)


### ### ### ### ### ### ### ### ### ### ### ### 
### predicted vs actual values scatter plots

#best model
calculations$predicted1<-predict(reg8i, newdata = calculations[ , c("dpDBACol", "dpDBAFFR")])

C <- ggscatter(calculations, y = "DEE.KJ.d.g", 
         x = "predicted1",
         color = "black", shape = 21, #size = 3, # Points color, shape and size
         add = "reg.line",  # Add regressin line
         add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
         conf.int = TRUE, # Add confidence interval
         cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
         cor.coeff.args = list(method = "pearson", #label.x = 3, 
                               label.sep = "\n"),
         title = NULL,
         xlab = "DEE (kJ/d*g) from DBA models",
         ylab = FALSE
)
C

## second best model

calculations$predicted2<-predict(reg7, newdata = calculations[ , c("dpDBAColRest", "dpDBAFlyFor")])

D <- ggscatter(calculations, y = "DEE.KJ.d.g", 
              x = "predicted2",
              color = "black", shape = 21, #size = 3, # Points color, shape and size
              add = "reg.line",  # Add regressin line
              add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
              conf.int = TRUE, # Add confidence interval
              cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
              cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                    label.sep = "\n"),
              title = NULL,
              xlab = "DEE (kJ/d*g) from DBA models",
              ylab = FALSE
)
D




### predictions for total DEE and actual values - NOT IN PAPER
calculations$predicted3 <- predict(reg10, newdata = data.frame(dpDBA = calculations[ , c("dpDBA")]))

E <- ggscatter(calculations, y = "DEE.KJ.d.g", 
               x = "predicted3",
               color = "black", shape = 21, #size = 3, # Points color, shape and size
               add = "reg.line",  # Add regressin line
               add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
               conf.int = TRUE, # Add confidence interval
               cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                     label.sep = "\n"),
               #title = "DEE.KJ.d.g ~ predicted (dpDBA)",
               xlab = "DEE (kJ/d*g) from DBA models",
               ylab = "DEE (kJ/d*g) from DLW"
)
E
cowplot::plot_grid(E, C, D, labels = c("A", "B", 
                                       "C"), nrow = 1)

plot_predictionsFull <- cowplot::plot_grid(C, D, labels = c("A", "B", "C"), nrow = 1)

#ggsave("plots/plot_predictionsFull.png", plot_predictionsFull, units = 'in', width = 22.5, height = 6)

  
#for for each parameter of the best model  DONE DIRECTLHY WITH PLOT from GGPrEDICT above

#A <- ggscatter(calculations, y = "DEE.KJ.d.g", x = "dpDBACol",
 #              color = "black", shape = 21, #size = 3, # Points color, shape and size
  #             add = "reg.line",  # Add regressin line
   #            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
    #           conf.int = TRUE, # Add confidence interval
     #          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
      #         cor.coeff.args = list(method = "pearson", #label.x = 3, 
       #                              label.sep = "\n"),
        #       title = "DEE.KJ.d.g and dpDBACol"
#)
#B <- ggscatter(calculations, y = "DEE.KJ.d.g", x = "dpDBAFFR",
 #              color = "black", shape = 21, #size = 3, # Points color, shape and size
  #             add = "reg.line",  # Add regressin line
   #            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
    #           conf.int = TRUE, # Add confidence interval
     #          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
      #         cor.coeff.args = list(method = "pearson", #label.x = 3, 
       #                              label.sep = "\n"),
        #       title = "DEE.KJ.d.g and dpDBAFFR"
#)

#cowplot::plot_grid(A,B, labels = c("A", "B"))





################## 
#summaries of Time budgets per activity and
### activity specific energy expenditure

#in both cases for models with Fly and For(plunging) merged!

# time budgets
#reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + 0)
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor + 0)

reg4
reg4$coefficients
timecoefsErr <-summary(reg4)$coefficients[, 2] # std errors for paramters

timecoefs <-as.data.frame(reg4$coefficients)
timecoefs$activity <- rownames(timecoefs)
timecoefs$timecoefsErr<-timecoefsErr
Timesummaries <- calculations %>% 
  select(dep_id, pTCol,pTFlyFor,pTRest, sex) %>% 
  pivot_longer(cols = c(pTCol,pTFlyFor,pTRest), names_to = "pTime", values_to = "pTimeVal") %>%
  group_by(pTime) %>% 
  summarise(
    MeanTime = mean(pTimeVal),
    Hourpday = MeanTime*24,
    Stdev = sd(pTimeVal),
    Min = min(pTimeVal),
    Max = max(pTimeVal)
  )

timemerge<-merge(Timesummaries, timecoefs, by.x = c("pTime"), by.y = c("activity"))
timemerge$DEEactivity <- timemerge$`reg4$coefficients`*timemerge$MeanTime



write.csv(timemerge, "TimesummariesFINAL.csv")

###################################################################################################
#coloblind palette

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

timebudgetplotALL <- calculations %>% 
  select(dep_id, pTCol,pTFly,pTFor,pTRest, sex) %>% 
  pivot_longer(cols = c(pTCol,pTFly,pTFor,pTRest), names_to = "pTime", values_to = "pTimeVal") %>% 
  ggplot(aes(x=pTime, y = pTimeVal, col = pTime), color = cbbPalette) +
  scale_colour_manual(values=cbPalette, labels =c('Colony', 'Flying', 'Plunging', 'Resting'))+
  geom_point(position = position_jitter(width = 0.2), alpha = 0.4 ) +  # Jittered raw data points
  stat_summary(fun = mean, geom = "point", size = 3 ,#fill = "blue", 
               position = position_dodge(width = 0.8)) +  # Mean values as bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, position = position_dodge(width = 0.8)) +  # Mean standard error bars
  labs(x = "Category", y = "Proportion of hours in the day "#,
       #title = "Mean Values per Category"
  )  +
  #facet_wrap(~sex)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  guides(color=guide_legend(title="Activity"))
timebudgetplotALL

#ggsave(timebudgetplotALL, filename = "plots/timebudgetplotALL.png", dpi = 300, width =10 , height = 7)

#######################################################################################
#not in paper

test_TimeBudgets <- calculations %>% 
  select(dep_id, pTCol,pTFly,pTFor,pTRest, sex) %>% 
  pivot_longer(cols = c(pTCol,pTFly,pTFor,pTRest), names_to = "pTime", values_to = "pTimeVal")

summary(lm(data = test_TimeBudgets, pTimeVal ~ pTime))

model<-aov(data = test_TimeBudgets, pTimeVal ~ pTime)
tukeyTime<-TukeyHSD(model, conf.level=.95)
tukeyTime

test_TimeBudgets %>% 
  ggplot()+
  geom

summary(lm(data = calculations, pTCol ~ pDBACol))
cor(calculations$pTCol, calculations$pDBACol)

summary(lm(data = calculations, pTFFR ~ dpDBAFFR))
cor(calculations$pTFFR, calculations$dpDBAFFR)
#######################################################################################

# DBA
#reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest)
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +  dpDBAFlyFor)
reg8$coefficients # estimate coefficients
dbacoefsErr <-summary(reg8)$coefficients[, 2] # std errors for paramters

dbacoefs <-as.data.frame(reg8$coefficients)
dbacoefs$activity <- rownames(dbacoefs)
dbacoefs$dbacoefsErr <- dbacoefsErr

##summaries of DBA per activity

DBAsummaries <- calculations %>% 
  select(dep_id, dpDBACol, dpDBARest, dpDBAFlyFor, sex) %>% 
  pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFlyFor), names_to = "dpDBA", values_to = "dpDBAVal") %>%
  group_by(dpDBA) %>% 
  summarise(
    MeanDBA = mean(dpDBAVal),
    Stdev = sd(dpDBAVal),
    Min = min(dpDBAVal),
    Max = max(dpDBAVal)
  )

dbamerge<-merge(DBAsummaries, dbacoefs, by.x = c("dpDBA"), by.y = c("activity"))
dbamerge$DEEactivity <- dbamerge$`reg8$coefficients`*dbamerge$MeanDBA

write.csv(dbamerge, "DBAsummariesFINAL.csv")


#not in paper
test_DBAcats <- calculations %>% 
  select(dep_id, dpDBACol, dpDBARest, dpDBAFly, dpDBAFor, sex) %>% 
  pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFly, dpDBAFor), names_to = "dpDBA", values_to = "dpDBAVal")

summary(lm(data = test_DBAcats, dpDBAVal ~ dpDBA))

modeldba <- aov(data = test_DBAcats, dpDBAVal ~ dpDBA)
tukeydba <- TukeyHSD(modeldba, conf.level=.95)
tukeydba




#######################################################################################
#######################################################################################
######### MALE AND FEMALE CALCS ### NOT ON PAPER
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
  dplyr::filter(sex == "M")%>% 
  filter(!Time.total > 100)

#############################################################################################

reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )


reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor + 0)
reg4i <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFFR + 0)


MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol

reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly + dpDBAFor)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +  dpDBAFlyFor)
reg8i <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol +  dpDBAFFR)
reg10 <- lm(data = calculations, formula = DEE.KJ.d.g ~  dpDBA)

MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


Mmods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8, reg8i, reg10)

Mmods_full
summary(reg4)

Mmods_full.df <- as.data.frame(Mmods_full) # NULL IS BEST
write.csv(Mmods_full.df,"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Mfull.csv")

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


################################ FEMALES

calculations <- calculationsraw_s %>% 
  dplyr::filter(sex == "F")%>% 
  filter(!Time.total > 100)

#############################################################################################
reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )


reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + 0)
reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor + 0 )
reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor + 0 )
reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor + 0)
reg4i <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFFR + 0)

#MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4)
#which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

#calculations$DBAFlyFor <- calculations$DBAFly + calculations$DBAFor
#calculations$DBAColRest <- calculations$DBARest + calculations$DBACol


reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest)
reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly + dpDBAFor)
reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFlyFor )
reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +  dpDBAFlyFor)
reg8i <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol +  dpDBAFFR)

reg10 <- lm(data = calculations, formula = DEE.KJ.d.g ~  dpDBA)

#MuMIn::model.sel(reg0, reg5, reg6, reg7, reg8)


Fmods_full <- MuMIn::model.sel(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8,reg8i, reg10)

Fmods_full
summary(reg4i)
Fmods_full.df <- as.data.frame(Fmods_full)
write.csv(Fmods_full.df,"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_Ffull.csv")

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


############################## NOT IN PAPER #
#differences in pTcol per sex 

calculations <- calculationsraw_s[which(calculationsraw_s$Time.total <100),]

pTcol.lm <- lm(data = calculations, pTCol ~ sex)

summary(pTcol.lm)

plot(resid(pTcol.lm))
boxplot(resid(pTcol.lm)~ calculations$sex)

car::leveneTest(resid(pTcol.lm)~ calculations$sex)

car::influencePlot(pTcol.lm)

pTCol.pred <- ggeffects::ggpredict(
  pTcol.lm,
  terms = c("sex"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
)

ggplot(data = calculations)+
  geom_point( aes(y = pTCol, x = sex) )


ggplot(data = calculations)+ #, color = Spec
  geom_point(aes(y = pTCol, x = sex), position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggpredict(
    pTcol.lm,
    terms = c("sex"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ) ,
  aes(x = x, y = predicted, 
      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab("Sex")+
  ylab("pTcol.lm")+
  guides(color = "none")+
  theme_bw()


#### plot time budgets split by sexes
calculations %>% 
  select(dep_id, pTCol, pTRest, pTFly, pTFor, sex) %>% 
  pivot_longer(cols = c(pTCol, pTRest, pTFly, pTFor), names_to = "pTime", values_to = "pTimeVal") %>% 
ggplot()+
  geom_point(aes(x=pTime, y = pTimeVal, col = pTime), position = position_jitter(height = 0, width = 0.1))+
  facet_wrap(~sex)+
  theme_bw()

#### plot DBA split by sexes
calculations %>% 
  select(dep_id, dpDBACol, dpDBARest, dpDBAFly, dpDBAFor, sex) %>% 
  pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFly, dpDBAFor), names_to = "dpDBA", values_to = "dpDBAVal") %>% 
  ggplot()+
  geom_point(aes(x=dpDBA, y = dpDBAVal, col = dpDBA), position = position_jitter(height = 0, width = 0.1))+
  facet_wrap(~sex)+
  theme_bw()

#point plots mean daily DBA activiy
calculations %>% 
  select(dep_id, dpDBACol, dpDBARest, dpDBAFly, dpDBAFor, sex) %>% 
  pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFly, dpDBAFor), names_to = "dpDBA", values_to = "dpDBAVal") %>% 
  ggplot(aes(x=dpDBA, y = dpDBAVal, col = dpDBA)) +
  geom_point( position = position_jitter(width = 0.2), alpha = 0.5) +  # Jittered raw data points
  stat_summary(fun = mean, geom = "point", fill = "blue", position = position_dodge(width = 0.8)) +  # Mean values as bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.8)) +  # Mean standard error bars
  labs(x = "Category", y = "Value"#,
       #title = "Mean Values per Category"
       )  +
  facet_wrap(~sex)+
  theme_bw()+
  theme(axis.text.x = element_blank())


#point plots mean Time budget per activiy
calculations %>% 
  select(dep_id, pTCol,pTFly,pTFor,pTRest, sex) %>% 
  pivot_longer(cols = c(pTCol,pTFly,pTFor,pTRest), names_to = "pTime", values_to = "pTimeVal") %>% 
  ggplot(aes(x=pTime, y = pTimeVal, col = pTime)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5 ) +  # Jittered raw data points
  stat_summary(fun = mean, geom = "point", fill = "blue", position = position_dodge(width = 0.8)) +  # Mean values as bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.8)) +  # Mean standard error bars
  labs(x = "Category", y = "Value"#,
       #title = "Mean Values per Category"
  )  +
  facet_wrap(~sex)+
  theme_bw()+
  theme(axis.text.x = element_blank())


#boxplots mean daily DBA per activity
DBAbox <- calculations %>% 
  select(dep_id, dpDBACol, dpDBARest, dpDBAFly, dpDBAFor, sex) %>% 
  pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFly, dpDBAFor), names_to = "dpDBA", values_to = "dpDBAVal") %>% 
  ggplot(aes(x=dpDBA, y = dpDBAVal, col = dpDBA)) +
  geom_boxplot( ) +  # Jittered raw data points
  #stat_summary(fun = mean, geom = "point", fill = "blue", position = position_dodge(width = 0.8)) +  # Mean values as bars
  #stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.8)) +  # Mean standard error bars
  labs(x = "Category", y = "Value"#,
       #title = "Mean Values per Category"
  )  +
  facet_wrap(~sex)+
  theme_bw()+
  theme(axis.text.x = element_blank())


#time plots per sex

timebox <- calculations %>% 
  select(dep_id, pTCol,pTFly,pTFor,pTRest, sex) %>% 
  pivot_longer(cols = c(pTCol,pTFly,pTFor,pTRest), names_to = "pTime", values_to = "pTimeVal") %>% 
  ggplot(aes(x=pTime, y = pTimeVal, col = pTime)) +
  geom_boxplot( ) +  # Jittered raw data points
  #stat_summary(fun = mean, geom = "point", fill = "blue", position = position_dodge(width = 0.8)) +  # Mean values as bars
  #stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.8)) +  # Mean standard error bars
  labs(x = "Category", y = "Value"#,
       #title = "Mean Values per Category"
  )  +
  facet_wrap(~sex)+
  theme_bw()+
  theme(axis.text.x = element_blank())

cowplot::plot_grid(timebox, DBAbox, labels = c("A", "B"), nrow = 2)


### dpDBACol comparison between males and females
calculations <- calculationsraw_s[which(calculationsraw_s$Time.total <100),]

dpDBACol.lm <- lm(data = calculations, dpDBACol ~ sex)

summary(dpDBACol.lm)

plot(resid(dpDBACol.lm))
boxplot(resid(dpDBACol.lm)~ calculations$sex)

car::leveneTest(resid(dpDBACol.lm)~ calculations$sex)

car::influencePlot(dpDBACol.lm)

dpDBACol.pred <- ggeffects::ggpredict(
  dpDBACol.lm,
  terms = c("sex"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
)

ggplot(data = calculations)+
  geom_point( aes(y = dpDBACol, x = sex) )


ggplot(data = calculations)+ #, color = Spec
  geom_point(aes(y = dpDBACol, x = sex), position = position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  geom_pointrange(data=ggeffects::ggpredict(
    dpDBACol.lm,
    terms = c("sex"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ) ,
  aes(x = x, y = predicted, 
      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  xlab("Sex")+
  ylab("dpDBACol.lm")+
  guides(color = "none")+
  theme_bw()



