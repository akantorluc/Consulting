library(lavaan)

#original model by Brian also not good
mod1 <- 'typhaBM ~ waterDepth
          detritusAW ~ typhaBM
          totalVegCover ~ detritusAW
          aquatics ~ totalVegCover + detritusAW'
mod11 = sem(mod1, data = global_data, estimator="mlm")
summary(mod11)


mod2 <- 'typhaBM100 ~ waterDepth + H
          graminoids ~ H
          detritusAW ~ typhaBM100 + H + NH4_N + waterDepth
          richness ~ H + NO3_N + typhaBM100 + organic
          aquatics ~ H + NO3_N + organic + waterDepth'
mod22 = sem(mod2, data = global_data, estimator="mlm")
summary(mod22)
