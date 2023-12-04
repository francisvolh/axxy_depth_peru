library(MuMIn)
######## method with resampling ## not used in the paper

your_data <- calculationsraw_s%>% 
  filter(!Time.total > 100)# with no bird6, incomplete track!!



#including outlier bird MALES
#your_data <- your_data %>% 
 # dplyr::filter(sex == "F")

set.seed(123)  # For reproducibility lumped (either with or without sex as a factor)

#set.seed(12345)  # For MALES reproducibility

#set.seed(54321) # For FEMALES reproducibility

n_bootstrap <- 1000  # Number of bootstrap samples

#models <- list(reg0, reg1, reg2, reg3, reg4, reg4i, reg5, reg6, reg7, reg8,reg8i)  # Add more models to the list



# Assuming you have defined your models as mentioned earlier
models <- list(
  list(formula =DEE.KJ.d.g ~ 1),
  list(formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + sex + 0),
  list(formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor + sex+ 0),
  list(formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor+ sex + 0),
  list(formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor+ sex + 0),
  list(formula = DEE.KJ.d.g ~ pTCol + pTFFR + sex+ 0),
  list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest+ sex),
  list(formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly + dpDBAFor+ sex),
  list(formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFlyFor+ sex),
  list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +  dpDBAFlyFor+ sex),
  list(formula = DEE.KJ.d.g ~ dpDBACol +  dpDBAFFR+ sex)
  # Add more models to the list with their respective formulas
)


models <- list(
  list(formula =DEE.KJ.d.g ~ 1),
  list(formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest  + 0),
  list(formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor + 0),
  list(formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor + 0),
  list(formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor+  0),
  list(formula = DEE.KJ.d.g ~ pTCol + pTFFR + 0),
  list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest),
  list(formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly + dpDBAFor),
  list(formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFlyFor),
  list(formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +  dpDBAFlyFor),
  list(formula = DEE.KJ.d.g ~ dpDBACol +  dpDBAFFR)
  # Add more models to the list with their respective formulas
)

#reg0 <- lm(data = calculations, formula =DEE.KJ.d.g ~ 1 )
#reg1 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFly + pTFor + pTRest + 0)
#reg2 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFly + pTFor + 0 )
#reg3 <- lm(data = calculations, formula =DEE.KJ.d.g ~ pTColRest + pTFlyFor + 0 )
#reg4 <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTRest +  pTFlyFor + 0)
#reg4i <- lm(data = calculations, formula = DEE.KJ.d.g ~ pTCol + pTFFR + 0)
#reg5 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBAFly + dpDBAFor + dpDBARest)
#reg6 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFly + dpDBAFor)
#reg7 <- lm(data = calculations, formula =DEE.KJ.d.g ~ dpDBAColRest + dpDBAFlyFor )
#reg8 <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol + dpDBARest +  dpDBAFlyFor)
#reg8i <- lm(data = calculations, formula = DEE.KJ.d.g ~ dpDBACol +  dpDBAFFR)


# Calculate average AICc values for each model
average_aiccs <- numeric(length(models))


for (m in 1:length(models)) {
  model <- models[[m]]
  formula <- model$formula
  bootstrapped_aiccs <- numeric(n_bootstrap)
  
  for (i
       in 1:n_bootstrap) {
    bootstrap_sample <- your_data[sample(nrow(your_data), replace = TRUE), ]
    bootstrap_model <- lm(formula, data = bootstrap_sample)
    bootstrapped_aiccs[i] <- AICc(bootstrap_model)
  }
  
  average_aiccs[m] <- mean(bootstrapped_aiccs)
}

# Rank models based on average AICc values
ranked_models <- order(average_aiccs)

# Print ranked models
for (m in ranked_models) {
  cat("Model", m, " - Average AICc:", average_aiccs[m], "\n")
  cat("Formula:", deparse(models[[m]]$formula), "\n")
  cat("\n")
}

best_model_index <- ranked_models[1]
best_model <- models[[best_model_index]]
best_formula <- best_model$formula

# Fit the best model to the entire dataset
best_final_model <- lm(best_formula, data = your_data)


### FIX DEPENDING IF YOU ARE RUNNING LUMPED SEXES OR EACH SEX
# Obtain predicted values from the best model
your_data$predicted_values <- predict(best_final_model, newdata = your_data[ , c("dpDBACol", "dpDBAFly", "dpDBAFor", "dpDBARest")])

your_data$predicted_values <- predict(best_final_model, newdata = your_data[ , c("pTColRest" , "pTFly" , "pTFor" , "sex")])



# Print predicted values
print(your_data$predicted_values)

ggscatter(your_data, y = "DEE.KJ.d.g", 
          x = "predicted_values",
          color = "black", shape = 21, #size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", #label.x = 3, 
                                label.sep = "\n"),
          title = "Males DEE.KJ.d.g ~ predicted (dpDBACol + dpDBAFly + dpDBAFor + dpDBARest)"
)


