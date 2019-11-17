library(lavaan)

#original model by Brian also not good
mod1 <- 'typhaBM ~ waterDepth
          detritusAW ~ typhaBM
          totalVegCover ~ detritusAW
          aquatics ~ totalVegCover + detritusAW'
mod11 = sem(mod1, data = global_data, estimator="mlm")
summary(mod11)

# Good model - does it make sense?
mod2 <- 'typhaBM100 ~ waterDepth + H + unVegCover
          graminoids ~ H
          detritusAW ~ typhaBM100 + NH4_N + waterDepth + H
          richness ~ H + NO3_N + typhaBM100 + organic
          aquatics ~ H + NO3_N + organic + waterDepth'
mod22 = sem(mod2, data = global_data, estimator="mlm")
summary(mod22, fit.measures = T)

# based on correlations
mod.33 <- 'organic10 ~ H + rushes + aquatics + unVegCover10
          detritusAW10 ~ waterDepth10 + H + aquatics + forbs
          H ~ waterDepth10
          richness ~ H + waterDepth10 + rushes + aquatics
          typhaBM100 ~ unVegCover10
          totalVegCover ~ graminoids + typCover
          unVegCover10 ~ NO3_N10
          graminoids ~ H + typCover
          rushes ~ H
          aquatics ~ H + waterDepth10 + NO3_N10
          forbs ~ waterDepth10
          typCover ~ H'
mod33 = sem(mod.33, data = global_data, estimator="mlm")
summary(mod33, fit.measures = T)

library(regsem)
mod33.reg <- cv_regsem(mod33, type = "lasso", n.lambda = 20, jump = 0.05)
plot(mod33.reg, show.minimum = "BIC")
summary(mod33.reg)
mod33.reg$final_pars

mod.34 <- 'organic10 ~ rushes + aquatics + unVegCover10
          detritusAW10 ~ H
          H ~ waterDepth10
          richness ~ H
          typhaBM100 ~ unVegCover10
          totalVegCover ~ graminoids
          unVegCover10 ~ NO3_N10
          graminoids ~ H + typCover
          rushes ~ H
          aquatics ~ H + waterDepth10 + NO3_N10
          forbs ~ waterDepth10
          typCover ~ H'
mod34 = sem(mod.34, data = global_data, estimator="mlm")
summary(mod34)
