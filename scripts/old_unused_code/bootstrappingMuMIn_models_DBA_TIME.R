library(dplyr)
library(MuMIn)

##method with the MuMIn boot weitghs function, included in the paper

calculationsraw_s <- read.csv("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 1 - DLW axxy/axxy_depth_peru/data/calculations.csv")

calculations <- calculationsraw_s #<- calculations

calculations <- calculationsraw_s%>% 
  filter(!Time.total > 100)# %>% 
  #filter(sex == "F") # didnt REPORT bootstrapping for sexes in the paper 
set.seed(12345)
#lmtest<-glm(data=calculations, DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest  + 0, 
  #          na.action = na.fail,
   #         family = gaussian(),
    #        x = TRUE)

#not using the Sex parameter, but splitting sexes for each run
#models <- list(
 # list(formula =DEE.KJ.d.g ~ 1),
  #list(formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + sex+ 0),
#   list(formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor +sex+  0),
# list(formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor + sex+ 0),
# list(formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor+ sex+  0),
# list(formula = DEE.KJ.d.g ~ pTCol + pTFFR + sex+ 0),
# list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor +sex+  dpDBARest),
# list(formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly + sex+ dpDBAFor),
# list(formula =DEE.KJ.d.g ~ dpDBAColRest +sex+  dpDBAFlyFor),
# list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest + sex+  dpDBAFlyFor),
# list(formula = DEE.KJ.d.g ~ dpDBACol + sex+  dpDBAFFR)
  # Add more models to the list with their respective formulas
# )

models <- list(
  list(formula =DEE.KJ.d.g ~ 1),
  list(formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest +  0),
  list(formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor +  0),
  list(formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor +  0),
  list(formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor+   0),
  list(formula = DEE.KJ.d.g ~ pTCol + pTFFR +  0),
  list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor +  dpDBARest),
  list(formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly +  dpDBAFor),
  list(formula =DEE.KJ.d.g ~ dpDBAColRest +  dpDBAFlyFor),
  list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +   dpDBAFlyFor),
  list(formula = DEE.KJ.d.g ~ dpDBACol +   dpDBAFFR),
  list(formula = DEE.KJ.d.g ~ dpDBA ) # incorporated total DBA model to check 
  #list(formula = DEE.KJ.d.g ~ dpDBACol ),
  #list(formula = DEE.KJ.d.g ~ dpDBAFly ),
  #list( formula = DEE.KJ.d.g ~ dpDBAFor),
  #list(formula = DEE.KJ.d.g ~  dpDBARest)
  # Add more models to the list with their respective formulas
)

models.list <- list()
x<-NULL
for (m in 1:length(models)) {
  model <- models[[m]]
  formula <- model$formula
  form.text <- toString(formula)
  x <- rbind(x, form.text)
  lmtest<-glm(data=calculations, formula, 
              na.action = na.fail,
              family = gaussian(),
              x = TRUE)
  #bootstrapped_aiccs <- numeric(n_bootstrap)
  models.list[[m]]<-lmtest
  
}

sum.bs <-MuMIn::bootWeights(models.list,  R = 10000)

models.df <-data.frame(x=x, y=sum.bs)

models.df <- models.df[order(models.df$y, decreasing = TRUE),]
models.df
models.df$val <- log(models.df$y) * -2
models.df$deltaic  <- models.df$val - min(models.df$val)
models.df
#write.csv(models.df, "models.df.bs.All.csv")

#best model for females after bootstrapping

lmbestF<-glm(data=calculations,  DEE.KJ.d.g ~ pTColRest + pTFlyFor +  0, 
            na.action = na.fail,
            family = gaussian(),
            x = TRUE)

calculations$predictedF<-predict(lmbestF, newdata = calculations[ , c("pTColRest","pTFlyFor")])

ggscatter(calculations, y = "DEE.KJ.d.g", 
          x = "predictedF",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regression line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Females DEE.KJ.d.g and Pred TColRest TFlyFor (with outlier)"
)


#best model for males after bootstrapping, NULL IS BEST, RUNNING SECOND BEST
calculations <- calculationsraw_s%>% 
  filter(!Time.total > 100) %>% 
filter(sex == "M")
set.seed(12345)
lmbestM<-glm(data=calculations,  DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest, 
             na.action = na.fail,
             family = gaussian(),
             x = TRUE)

calculations$predictedM<-predict(lmbestM, newdata = calculations[ , c("dpDBACol" ,"dpDBAFly","dpDBAFor","dpDBARest")])

ggscatter(calculations, y = "DEE.KJ.d.g", 
          x = "predictedM",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regression line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Males DEE.KJ.d.g and Pred dpDBACol + dpDBAFly + dpDBAFor + dpDBARest"
)
