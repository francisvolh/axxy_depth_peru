library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(lattice)
library(cowplot)
library(MuMIn)

set.seed(12345)



#already saved #July 28th with new caloric coef and fixed time for A06
#calculationsraw_s <- read.csv(file.choose()) 
calculationsraw_s <- read.csv("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/calculations.csv")

calculations <- calculationsraw_s #<- calculations


# get some time sampling values numbers (Excluding long deployed bird, which will be excluded for all calculations later as well) 
calculations[which(calculations$Time.total <100),] |>
  dplyr::summarise(
    meanTime = mean(Time.total),
    maxTime = max(Time.total),
    minTime = min(Time.total),
    stdTime = sd(Time.total)
  )



names(calculations)
head(calculations)

#####
#visuals only
#including outlier bird
{ggpubr::ggscatter(calculations[which(calculations$SampTime <70),], y = "DEE.kJ.d", #[which(calculations$SampTime <50),]
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
  
  ggpubr::ggscatter(calculations[which(calculations$SampTime <70),], y = "DEE.KJ.d.g", #[which(calculations$SampTime <50),]
                    x = "Time.total",
                    color = "black", shape = 21, #size = 3, # Points color, shape and size
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE, # Add confidence interval
                    cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                    cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                          label.sep = "\n")#,
                    #title = "DEE.KJ.d.g ~ Sampling time"
  )}

#####################################
# Summaries for DEE and DEEg

summary(calculations[ , c("DEE.KJ.d.g","DEE.kJ.d")])

df_to_pivot <- calculations[ , c("merge.col" ,"DEE.KJ.d.g","DEE.kJ.d", "sex", "Mass.avg.")]

for_summary <- df_to_pivot |>
  dplyr::rename(DEEg = DEE.KJ.d.g, DEE = DEE.kJ.d )|>
  tidyr::pivot_longer(cols =c(DEE,DEEg) , names_to = "Variable", values_to = "values")|> 
  dplyr::group_by(Variable) |> 
  dplyr::summarise(
    mean=mean(values), 
    sd=sd(values),
    min = min(values),
    max = max(values),
    mass = mean(Mass.avg.),
    sd.mass=sd(Mass.avg.)
  )

for_summary <- as.data.frame(for_summary)

#write.csv(for_summary, "DEE_summary.csv")

for_S_summary <- df_to_pivot |>
  dplyr::rename(DEEg = DEE.KJ.d.g, DEE = DEE.kJ.d ) |>
  tidyr::pivot_longer(cols =c(DEE,DEEg) , names_to = "Variable", values_to = "values") |>
  dplyr::group_by(Variable, sex) |>
  dplyr::summarise(
    mean=mean(values), 
    sd=sd(values),
    min = min(values),
    max = max(values),
    mass = mean(Mass.avg.),
    sd.mass=sd(Mass.avg.)
  )

#write.csv(for_S_summary, "DEE_summary_sex.csv")

#####
#checking for sex differences in DEEg and DEE total

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

ggplot2::ggplot(data = calculations)+
  ggplot2::geom_point( ggplot2::aes(y = DEE.KJ.d.g, x = sex) )


ggplot2::ggplot(data = calculations)+ #, color = Spec
  ggplot2::geom_point(ggplot2::aes(y = DEE.KJ.d.g, x = sex), position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggpredict(
    dee.g.lm,
    terms = c("sex"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ) ,
  ggplot2::aes(x = x, y = predicted, 
                      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab("Sex")+
  ggplot2::ylab("DEE/g")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()


# DEE total - not mass specific
dee.lm <- lm(data = calculations, DEE.kJ.d ~ sex)

summary(dee.lm)

ggplot2::ggplot(data = calculations)+ #, color = Spec
  ggplot2::geom_point(ggplot2::aes(y = DEE.kJ.d, x = sex), position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::xlab("Sex")+
  ggplot2::ylab("DEE")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()

######
#### MUST DO bird has not a full axxy cycle
####
#eliminate bird A06

calculations <- calculationsraw_s |>
 dplyr::filter(!Time.total > 100) #getting rid of 


####################################################################### 
#create df with models names for later use
reg0 <- "Null"

reg1 <- "pTCol + pTFly + pTFor + pTRest"
reg2 <- "pTColRest + pTFly + pTFor"
reg3 <- "pTColRest + pTFlyFor"
reg4 <- "pTCol + pTRest +  pTFlyFor"
reg4i <- "pTCol + pTFFR"


reg5 <- "dpDBACol + dpDBAFly + dpDBAFor + dpDBARest"
reg6 <-"dpDBAColRest + dpDBAFly + dpDBAFor"
reg7 <-  "dpDBAColRest + dpDBAFlyFor"
reg8 <- "dpDBACol + dpDBARest +  dpDBAFlyFor"
reg8i <-  "dpDBACol +  dpDBAFFR"
reg10 <-  "dpDBA"

model_namesdf<-data.frame( model = c(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8, reg8i, reg10),
                           name = c("reg0", "reg1", "reg2",  "reg3", "reg4", "reg4i", "reg5", "reg6", "reg7", "reg8", "reg8i", "reg10")
)

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

#model not accurate with no zero intercept
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

summary(reg8i)# best ranked model of all the 2 parameter or more



summary(reg7)

mods_full.df <- as.data.frame(mods_full)

mods_full.df$names<-rownames(mods_full.df)

mods_full.df <- merge( x = mods_full.df, y = model_namesdf, by.x = "names", by.y ="name" )

mods_full.df <- mods_full.df[order(mods_full.df$delta),]

#write.csv(mods_full.df, "C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/processed_acc_third_run/mods_full.csv")
## total DBA model 

mod_pred_TotalDBA<-ggeffects::ggpredict(
  reg10,
  terms = c("dpDBA"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
) #asking to predict only with 1 term to make the plot better, modify if needed


mod_pred_TotDBA_PLOT <- plot(mod_pred_TotalDBA, add.data = TRUE, show.title = FALSE, colors = "bw")+
  ggplot2::labs(
    x = "Total daily DBA",
    y = "Mass-specific Daily Energy Expenditure (kJ/d*g)"
  )+
  ggplot2::theme(  text= ggplot2::element_text(size=18))
mod_pred_TotDBA_PLOT

##ggsave("plots/total_dailyDBAmodel.png", mod_pred_TotDBA_PLOT, dpi = 300, bg = "white", units = 'in', width = 7.5, height = 6)



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
  ggplot2::labs(
    x = "DBA at the colony",
    y = "Mass-specific Daily Energy Expenditure (kJ/d*g)"
  )+
  ggplot2::theme(  text= ggplot2::element_text(size=18))
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

# model results with prediction confidence
### NOT IN THE PAPER
mod_pred_1B<-ggeffects::ggpredict(
  reg8i,
  terms = c("dpDBAFFR"),
  ci.lvl = 0.95,
  type = "fe",
  back.transform= FALSE, 
  typical = "mean"
) #asking to predict only with 1 term to make the plot better, modify if needed

mod_pred_1B_PLOT <- plot(mod_pred_1B, add.data = TRUE, show.title = FALSE, colors = "bw")+
  ggplot2::labs(
    x = "DBA outsite of the colony (Flying, Plunging, Resting)",
    y = "Mass-specific Daily Energy Expenditure (kJ/d*g)"
  )+
  ggplot2::theme(  text= ggplot2::element_text(size=18))
mod_pred_1B_PLOT
#plot(mod_pred_1, colors = "bw", ci = TRUE, ci.style = "dash")

#two plots with one plot indepently per parameter predictions
cowplot::plot_grid(mod_pred1_A_PLOT, mod_pred_1B_PLOT)
predic.two.models <- cowplot::plot_grid(mod_pred1_A_PLOT, mod_pred_1B_PLOT)

#ggplot2::ggsave("plots/predic.twoPar.modelsv2.png", predic.two.models, dpi = 300, bg = "white", units = 'in', width = 15, height = 6)


##############################################################
#alternative 2 for model prediction graph with both terms together
### NOT IN THE PAPER
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


######## IN THE PAPER
### predicted vs actual values scatter plots

#best model parametrized
calculations$predicted1<-predict(reg8i, newdata = calculations[ , c("dpDBACol", "dpDBAFFR")])

C <- ggpubr::ggscatter(calculations, y = "DEE.KJ.d.g", 
         x = "predicted1",
         color = "black", shape = 21, #size = 3, # Points color, shape and size
         add = "reg.line",  # Add regressin line
         add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
         conf.int = TRUE, # Add confidence interval
         cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
         cor.coeff.args = list(method = "pearson", #label.x = 3, 
                               label.sep = "\n"),
         title = NULL,
         #xlab = "DEE (kJ/d*g) from model (DBA col + DBA away)",
         ylab = FALSE
)+
  ggplot2::labs(x= expression("Predictted DEE (kJ/d*g) from model DBA"[italic(c)]+"DBA"[italic(fpr)]))

C

## second best model parametrized

calculations$predicted2<-predict(reg7, newdata = calculations[ , c("dpDBAColRest", "dpDBAFlyFor")])

D <- ggpubr::ggscatter(calculations, y = "DEE.KJ.d.g", 
              x = "predicted2",
              color = "black", shape = 21, #size = 3, # Points color, shape and size
              add = "reg.line",  # Add regressin line
              add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
              conf.int = TRUE, # Add confidence interval
              cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
              cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                    label.sep = "\n"),
              title = NULL,
              #xlab = "MeasuredDEE (kJ/d*g) from model (DBAcr + DBA fp)",
              ylab = FALSE
)+
  ggplot2::labs(x= expression("Predicted EE (kJ/d*g) from model DBA"[cr]+"BDA"[fp]))
D




### predictions for total DEE and actual values - BEST MODEL
calculations$predicted3 <- predict(reg10, newdata = data.frame(dpDBA = calculations[ , c("dpDBA")]))

E <- ggpubr::ggscatter(calculations, y = "DEE.KJ.d.g", 
               x = "predicted3",
               color = "black", shape = 21, #size = 3, # Points color, shape and size
               add = "reg.line",  # Add regressin line
               add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
               conf.int = TRUE, # Add confidence interval
               cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                     label.sep = "\n"),
               #title = "DEE.KJ.d.g ~ predicted (dpDBA)",
               #xlab = "DEE (kJ/d*g) from total daily DBA",
               ylab = "Measured DEE (kJ/d*g) from DLW"
)+
  ggplot2::labs(x= "Predicted DEE (kJ/d*g) from model of moean daily DBA")

E
cowplot::plot_grid(E, C, D, labels = c("A", "B", 
                                       "C"), nrow = 1)

plot_predictionsFull <- cowplot::plot_grid(E, C, D, labels = c("A", "B", "C"), nrow = 1)

#ggplot2::ggsave("plots/plot_predictionsFullv2.png", plot_predictionsFull, units = 'in', width = 14, height = 5)

#####
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
#SUMMARIES of Time budgets per activity and
### activity specific energy expenditure

#in both cases for models with Fly and For(plunging) merged!

# TIME BUDGETS ALL BIRDS all 4 activities!!
reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + 0)
#reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor + 0)

reg1
reg1$coefficients
timecoefsErr <-summary(reg1)$coefficients[, 2] # std errors for paramters

timecoefs <-as.data.frame(reg1$coefficients)
timecoefs$activity <- rownames(timecoefs)
timecoefs$timecoefsErr<-timecoefsErr

Timesummaries <- calculations |>
  dplyr::select(dep_id, pTCol,pTFly, pTFor,pTRest, sex) |> 
  tidyr::pivot_longer(cols = c(pTCol,pTFly, pTFor,pTRest), names_to = "pTime", values_to = "pTimeVal")|>
  dplyr::group_by(pTime) |>
  dplyr::summarise(
    MeanTime = mean(pTimeVal),
    Hourpday = MeanTime*24,
    Stdev = sd(pTimeVal),
    Min = min(pTimeVal),
    Max = max(pTimeVal)
  )


#not on paper
#timemerge<-merge(Timesummaries, timecoefs, by.x = c("pTime"), by.y = c("activity"))

#timemerge$DEEactivity <- timemerge$`reg1$coefficients`*timemerge$MeanTime



#write.csv(timemerge, "TimesummariesFINAL.csv")

#trials
#reg4$coefficients[ c("pTFlyFor")]

#flight_costs_all <- calculations[ , c("dep_id", "Mass.avg.")]
#flight_costs_all$flight_coef <- reg4$coefficients[ c("pTFlyFor")]
#flight_costs_all$watts <- flight_costs_all$flight_coef*1000*flight_costs_all$Mass.avg./(24*60*60)
#mean(flight_costs_all$watts)
#mean(flight_costs_all$Mass.avg.)
###################################################################################################
#colorblind friendly palette

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

timebudgetplotALL <- calculations |>
  dplyr::select(dep_id, pTCol,pTFly,pTFor,pTRest, sex) |>
  tidyr::pivot_longer(cols = c(pTCol,pTFly,pTFor,pTRest), 
                      names_to = "pTime", values_to = "pTimeVal") |> 
  ggplot2::ggplot(ggplot2::aes(x=pTime, y = pTimeVal, col = pTime), color = cbbPalette) +
 
  ggplot2::geom_point(position =  ggplot2::position_jitter(width = 0.2), alpha = 0.4 ,
                      ggplot2::aes(shape=sex), cex = 3) +  # Jittered raw data points
  ggplot2::stat_summary(fun = mean, geom = "point", size = 3 ,#fill = "blue", 
               position =  ggplot2::position_dodge(width = 0.8)) +  # Mean values as bars
  ggplot2::stat_summary( fun.data =  ggplot2::mean_se, geom = "errorbar", 
                         width = 0, position =  ggplot2::position_dodge(width = 0.8)) +  # Mean standard error bars
  ggplot2::labs(x = "Activity", y = "Proportion of the day "#,
       #title = "Mean Values per Category"
  )  +
  #facet_wrap(~sex)+
  ggplot2::theme_bw()+
  
  ggplot2::scale_colour_manual(values=cbPalette#, labels =c('Colony', 'Flying', 'Plunging', 'Resting')
                                                  )+
  ggplot2::scale_x_discrete(labels =c('Colony', 'Flying', 'Plunging', 'Resting'))+
  ggplot2::theme(text=ggplot2::element_text(size=20),
                 legend.position = "none")
  #theme(axis.text.x = element_blank())+
 # guides(color=guide_legend(title="Activity"))
timebudgetplotALL


#ggplot2::ggsave(timebudgetplotALL, filename = "plots/timebudgetplotALLv2.png", dpi = 300, width =10 , height = 7)

fly<-tiff::readTIFF("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/drawings/Peruvian_Boobies 2.tiff",
)
colony<-tiff::readTIFF("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/drawings/Peruvian_Boobies 4.tiff",
)
resting <- tiff::readTIFF("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/drawings/Peruvian_Boobies.tiff",
)
plunge <- tiff::readTIFF("C:/Users/francis van oordt/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/drawings/Peruvian_Boobies 3.tiff",
)


pfinal <-
    timebudgetplotALL + 
    ggplot2::annotation_custom(grid::rasterGrob(fly), 
                               xmin = 1.5, xmax = 2.5, ymin = .55, ymax = 1
                                        )+ 
    ggplot2::annotation_custom(grid::rasterGrob(colony), 
                               xmin = 0.5, xmax = 1.3, ymin = .05, ymax = .45
    )+ 
    ggplot2::annotation_custom(grid::rasterGrob(plunge), 
                               xmin = 2.5, xmax = 3.5, ymin = .35, ymax = .75
    )+ 
    ggplot2::annotation_custom(grid::rasterGrob(resting), 
                               xmin = 3.5, xmax = 4.5, ymin = .025, ymax = .35
    )
ggplot2::ggsave(pfinal, filename = "plots/timebudgetplotALLv3.png", dpi = 300, width =10 , height = 7)
    
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

#write.csv(dbamerge,file ="DBAsummariesFINAL.csv")

DBAsummaries <- calculations %>% 
  select(dep_id, dpDBACol, dpDBARest, dpDBAFly, dpDBAFor, sex) %>% 
  pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFly, dpDBAFor), names_to = "dpDBA", values_to = "dpDBAVal") %>%
  group_by(dpDBA) %>% 
  summarise(
    MeanDBA = mean(dpDBAVal),
    Stdev = sd(dpDBAVal),
    Min = min(dpDBAVal),
    Max = max(dpDBAVal)
  )


#write.csv(DBAsummaries,file ="DBAsummaries4ParamFINAL.csv")


#not in paper
test_DBAcats <- calculations %>% 
  select(dep_id, dpDBACol, dpDBARest, dpDBAFly, dpDBAFor, sex) %>% 
  pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFly, dpDBAFor), names_to = "dpDBA", values_to = "dpDBAVal")

summary(lm(data = test_DBAcats, dpDBAVal ~ dpDBA))

modeldba <- aov(data = test_DBAcats, dpDBAVal ~ dpDBA)
tukeydba <- TukeyHSD(modeldba, conf.level=.95)
tukeydba

ggplot(data = test_DBAcats)+
  geom_boxplot(aes(x = dpDBA, y = dpDBAVal, col = dpDBA ))+
  theme_bw()+
  scale_x_discrete(labels=c("Colony","Flying","Plunging","Resting"))+
  xlab("Activity")+
  ylab("DBA daily Value")

test_DBAcats %>% 
  ggplot(aes(x=dpDBA, y = dpDBAVal, col = dpDBA), color = cbbPalette) +
  
  geom_point(position = position_jitter(width = 0.2), alpha = 0.4 ) +  # Jittered raw data points
  stat_summary(fun = mean, geom = "point", size = 3 ,#fill = "blue", 
               position = position_dodge(width = 0.8)) +  # Mean values as bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, position = position_dodge(width = 0.8)) +  # Mean standard error bars
  labs(x = "Activity", y = "DBA daily Value"#,
       #title = "Mean Values per Category"
  )  +
  #facet_wrap(~sex)+
  theme_bw()+
  
  scale_colour_manual(values=cbPalette#, labels =c('Colony', 'Flying', 'Plunging', 'Resting')
  )+
  scale_x_discrete(labels =c('Colony', 'Flying', 'Plunging', 'Resting'))+
  theme(legend.position = "none")
#theme(axis.text.x = element_blank())+
# guides(color=guide_legend(title="Activity"))


#######################################################################################
#######################################################################################
######### MALE AND FEMALE CALCS ### 
#######################################################################################

#####
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
Mmods_full.df$names<-rownames(Mmods_full.df)

Mmods_full.df <- merge( x = Mmods_full.df, y = model_namesdf, by.x = "names", by.y ="name" )
Mmods_full.df <- Mmods_full.df[order(Mmods_full.df$delta),]

#write.csv(Mmods_full.df,"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/mods_Mfull.csv")

calculations$predicted1<-predict(reg2, newdata = calculations[ , c("TColRest","TFly","TFor")])
#####
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

#####
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

Fmods_full.df$names<-rownames(Fmods_full.df)

Fmods_full.df <- merge( x = Fmods_full.df, y = model_namesdf, by.x = "names", by.y ="name" )

Fmods_full.df <- Fmods_full.df[order(Fmods_full.df$delta),]

#write.csv(Fmods_full.df,"C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/mods_Ffull.csv")

calculations$predicted1<-predict(reg3, newdata = calculations[ , c("TColRest","TFlyFor")])

#####
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

############################## ############################## ############################## 
############################## ############################## 
############################## ############################## 
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

# pTFly per sex differences
pTfly.lm <- lm(data = calculations, pTFly ~ sex)

summary(pTfly.lm)

#### plot time budgets split by sexes
calculations %>% 
  select(dep_id, pTCol, pTRest, pTFly, pTFor, sex) %>% 
  pivot_longer(cols = c(pTCol, pTRest, pTFly, pTFor), names_to = "pTime", values_to = "pTimeVal") %>% 
ggplot()+
  geom_point(aes(x=pTime, y = pTimeVal, col = pTime), position = position_jitter(height = 0, width = 0.1))+
  facet_wrap(~sex)+
  theme_bw()



#### plot DBA split by sexes
calculations|> 
  dplyr::select(dep_id, dpDBACol, dpDBARest, dpDBAFly, dpDBAFor, sex) |> 
  tidyr::pivot_longer(cols = c(dpDBACol, dpDBARest, dpDBAFly, dpDBAFor), names_to = "dpDBA", values_to = "dpDBAVal") |> 
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=dpDBA, y = dpDBAVal, col = dpDBA), position = ggplot2::position_jitter(height = 0, width = 0.1))+
  ggplot2::facet_wrap(~sex)+
  ggplot2::theme_bw()

dpDBACol.lm <- lm(data = calculations, dpDBACol ~ sex)
summary(dpDBACol.lm)
dpDBAFly.lm <- lm(data = calculations, dpDBAFly ~ sex)
summary(dpDBAFly.lm)

#point plots mean daily DBA activiy, split sexes
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


#point plots mean Time budget per activiy, split sexes
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


#boxplots mean daily DBA per activity, split sexes
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


ggplot2::ggplot(data = calculations)+ #, color = Spec
  ggplot2::geom_point(ggplot2::aes(y = dpDBACol, x = sex), position = ggplot2::position_jitter(height = 0, width = 0.1),
             alpha = 0.3)+
  #scale_color_manual(values= c("magenta4", "darkgreen"))+
  ggplot2::geom_pointrange(data=ggeffects::ggpredict(
    dpDBACol.lm,
    terms = c("sex"),
    ci.lvl = 0.95,
    type = "fe",
    back.transform= FALSE, 
    typical = "mean"
  ) ,
  ggplot2::aes(x = x, y = predicted, 
      ymin = predicted-std.error, ymax = predicted+std.error, color = x))+
  ggplot2::xlab("Sex")+
  ggplot2::ylab("dpDBACol.lm")+
  ggplot2::guides(color = "none")+
  ggplot2::theme_bw()

