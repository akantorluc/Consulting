library(lavaan)

model1 <- 'potber13 ~ potber12 + Typha12 + waterDepth12
          Typha13 ~ Typha12 + waterDepth12 + trmt_yes
potber12 ~ potber11 + Typha11 + waterDepth11
Typha12 ~ Typha11 + waterDepth11 + trmt_yes
potber13 ~~ Typha13
potber12 ~~ Typha12'
modfit <- sem(model1, data = Consult_yearly, estimator = "mlm")
summary(modfit) # .298

model2 <- 'potber13 ~ potber12 + utrmin12 + utrvul12 + Typha12 + waterDepth12
utrmin13 ~ potber12 + utrmin12 + utrvul12 + Typha12 + waterDepth12
utrvul13 ~ potber12 + utrmin12 + utrvul12 + Typha12 + waterDepth12
Typha13 ~ Typha12 + potber12 + utrmin12 + utrvul12 + waterDepth12 + trmt_yes
potber12 ~ potber11 + utrmin11 + utrvul11 + Typha11 + waterDepth11
utrmin12 ~ potber11 + utrmin11 + utrvul11 + Typha11 + waterDepth11
utrvul12 ~ potber11 + utrmin11 + utrvul11 + Typha11 + waterDepth11
Typha12 ~ Typha11 + waterDepth11 + trmt_yes
potber13 ~~ Typha13
potber12 ~~ Typha12
utrmin13 ~~ Typha13
utrmin12 ~~ Typha12
utrvul13 ~~ Typha13
utrvul12 ~~ Typha12
potber13 ~~ utrmin13
potber12 ~~ utrmin12
potber13 ~~ utrvul13
potber12 ~~ utrvul12
utrmin13 ~~ utrvul13
utrmin12 ~~ utrvul12'
modfit <- sem(model2, data = Consult_yearly, estimator = "mlm")
summary(modfit) 

model3 <- 'Typha13 ~ Typha12 + waterDepth12 + trmt_yes + site
          Typha12 ~ Typha11 + waterDepth11 + trmt_yes + site'
modfit <- sem(model3, data = Consult_yearly, estimator = "mlm")
summary(modfit) # .437

model4 <- 'Typha13 ~ Typha12
          Typha12 ~ Typha11 + trmt_yes
          vegCover13 ~ Typha12 + waterDepth12
          vegCover12 ~ vegCover11
          vegCover13 ~~ Typha13
          vegCover12 ~~ Typha12'
modfit <- sem(model4, data = Consult_yearly, estimator = "mlm")
summary(modfit) 

model5 <- 'Typha13 ~ Typha12
          Typha12 ~ Typha11 + trmt_yes'
modfit <- sem(model5, data = Consult_yearly, estimator = "mlm")
summary(modfit) # .779