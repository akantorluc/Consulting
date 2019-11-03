library(lavaan)

sqrt_model3 <- 'sqrt_Typha13 ~ sqrt_Typha12 + waterDepth12 + trmt_yes + site
         sqrt_Typha12 ~ Typha11 + waterDepth11 + trmt_yes + site'
sqrt_modfit3 <- sem(sqrt_model3, data = Consult_yearly, estimator = "mlm")
summary(sqrt_modfit3) # .437


sqrt_model4 <- 'Typha13 ~ Typha12
          Typha12 ~ Typha11 + trmt_yes
          vegCover13 ~ Typha12 + waterDepth12
          vegCover12 ~ vegCover11 
          vegCover13 ~~ Typha13
          vegCover12 ~~ Typha12'
sqrt_modfit4 <- sem(sqrt_model4, data = Consult_yearly, estimator = "mlm")
summary(sqrt_modfit4)


#no transform needed
sqrt_model5 <- 'Typha13 ~ Typha12
          Typha12 ~ Typha11 + trmt_yes'
sqrt_modfit5 <- sem(sqrt_model5, data = Consult_yearly, estimator = "mlm")
summary(sqrt_modfit5)
