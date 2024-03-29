library(lavaan)


attach(long_data)
lmod1 <- 'YR2016_typhaBM100 ~ YR2015_unVegCover + YR2015_graminoids  + YR2015_typhaBM100
          YR2017_typhaBM100 ~ YR2016_graminoids + YR2016_typhaBM100
          YR2018_typhaBM100 ~ YR2017_unVegCover + YR2017_graminoids + YR2017_typhaBM100
          logYR2016_detritusAW ~ YR2015_typhaBM100 + YR2015_NH4_N + YR2015_waterDepth + logYR2015_detritusAW
          logYR2017_detritusAW ~ YR2016_typhaBM100 + YR2016_NH4_N + YR2016_waterDepth + logYR2016_detritusAW 
          logYR2018_detritusAW ~ YR2017_typhaBM100 + YR2017_NH4_N + YR2017_waterDepth + logYR2017_detritusAW  
          YR2016_richness ~ YR2015_H + YR2015_NO3_N + YR2015_graminoids + YR2015_typhaBM100 + YR2015_organic + YR2015_richness  
          YR2017_richness ~ YR2016_H + YR2016_NO3_N + YR2016_graminoids + YR2016_typhaBM100 + YR2016_organic + YR2016_richness 
          YR2018_richness ~ YR2017_H + YR2017_graminoids + YR2017_typhaBM100 + YR2017_organic + YR2017_richness 
          logYR2016_aquatics ~ YR2015_H + YR2015_NO3_N + YR2015_graminoids + YR2015_waterDepth + logYR2015_aquatics
          logYR2017_aquatics ~ YR2016_H + YR2016_NO3_N + YR2016_graminoids + YR2016_waterDepth + logYR2016_aquatics
          logYR2018_aquatics ~ YR2017_H + YR2017_graminoids + YR2017_waterDepth + logYR2017_aquatics
          YR2016_typhaBM100 ~~ logYR2016_detritusAW
          YR2016_typhaBM100 ~~ YR2016_richness
          YR2016_typhaBM100 ~~ logYR2016_aquatics
          logYR2016_detritusAW ~~ YR2016_richness
          logYR2016_detritusAW ~~ logYR2016_aquatics
          YR2016_richness ~~ logYR2016_aquatics
          YR2017_typhaBM100 ~~ logYR2017_detritusAW
          YR2017_typhaBM100 ~~ YR2017_richness
          YR2017_typhaBM100 ~~ logYR2017_aquatics
          logYR2017_detritusAW ~~ YR2017_richness
          logYR2017_detritusAW ~~ logYR2017_aquatics
          YR2017_richness ~~ logYR2017_aquatics
          YR2018_typhaBM100 ~~ logYR2018_detritusAW
          YR2018_typhaBM100 ~~ YR2018_richness
          YR2018_typhaBM100 ~~ logYR2018_aquatics
          logYR2018_detritusAW ~~ YR2018_richness
          logYR2018_detritusAW ~~ logYR2018_aquatics
          YR2018_richness ~~ logYR2018_aquatics'
modl1 = sem(lmod1, data = long_data, estimator="mlm")
summary(modl1)


lmod16 <- 'YR2016_typhaBM100 ~ YR2015_unVegCover
          logYR2016_detritusAW ~ YR2015_NH4_N + YR2015_waterDepth
          YR2016_richness ~ YR2015_H + YR2015_NO3_N + YR2015_organic
          logYR2016_aquatics ~ YR2015_H + YR2015_NO3_N + YR2015_waterDepth
          YR2016_typhaBM100 ~~ logYR2016_detritusAW
          YR2016_typhaBM100 ~~ YR2016_richness
          YR2016_typhaBM100 ~~ logYR2016_aquatics
          logYR2016_detritusAW ~~ YR2016_richness
          logYR2016_detritusAW ~~ logYR2016_aquatics
          YR2016_richness ~~ logYR2016_aquatics'
modl16 = sem(lmod16, data = long_data, estimator="mlm")
summary(modl16)


