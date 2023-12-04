#model sets unsued (before making the daily proportional DBA calculations = dpDBA)



####################################################################### plots only #################################
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
calculations <- calculations %>% 
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
