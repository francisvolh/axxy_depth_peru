#join DLW results and cassification results: DLW_sheet_Boobies_2019.csv

#load DLW sheet 
dlw <- read.csv(file.choose())

#load classification calculations
merged_summed2 <- read.csv(file.choose())


calculations <- merge(merged_summed2, dlw, by.x = "dep_id", by.y ="Bird", all = TRUE)

summary(lm(data = calculations, DEE.KJ.d.g~ TimeFlying))
ggplot(calculations, aes(DEE.KJ.d.g, TimeFlying))+
  geom_point( )+
  geom_smooth(method = 'lm')

ggscatter(calculations, x = "DEE.KJ.d.g", y = "TimeFlying",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n")
)

#############################################################################################3
DEE ~ Tcol + Tfly+Trest + Tfor + 0
DEE ~ Tcolrest + Tfly+Tfor + 0
DEE ~ Tcolrest + Tflyfor + 0

which has lowest AIC? What are P-values? That is, what activities are similar to one another in terms of energetics? (Where Tcolrest = the sum of the colony of Tcol + Trest).

DEE ~ ODBAcol + ODBAfly+ODBArest + ODBAfor
DEE ~ ODBAcolrest + ODBAfly+ODBAfor
DEE ~ ODBAcolrest + ODBAflyfor