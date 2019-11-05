library(lavaan)

model1 <- 'Typha ~ waterDepth
          detritus ~ Typha
          TotalVegCover ~ detritus
          aquatics ~ TotalVegCover + detritus'

attach(long_data)
library(nlme)
library(piecewiseSEM)

new.modlist <- psem(
  lme(YR2016_typhaBM ~ YR2015_typhaBM + YR2015_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2017_typhaBM ~ YR2016_typhaBM + YR2016_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2018_typhaBM ~ YR2017_typhaBM + YR2017_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2016_detritusAW ~ YR2015_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2017_detritusAW ~ YR2016_detritusAW + YR2016_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2018_detritusAW ~ YR2017_detritusAW + YR2017_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2016_totalVegCover ~ YR2015_totalVegCover, random = ~1|plot, data = long_data),
  lme(YR2018_totalVegCover ~ YR2017_detritusAW, random = ~1|plot, data = long_data),
  lme(YR2016_aquatics ~ YR2015_aquatics + YR2015_totalVegCover, random = ~1|plot, data = long_data),
  lme(YR2017_aquatics ~ YR2016_aquatics + YR2016_totalVegCover + YR2016_detritusAW, random = ~1|plot, data = long_data),
  lme(YR2018_aquatics ~ YR2017_aquatics + YR2017_detritusAW, random = ~1|plot, data = long_data)
)
summary(new.modlist, long_data)

# accounting for relationship between same year responses since there is no covariance
new.modlist1 <- psem(
  lme(YR2016_typhaBM ~ YR2015_typhaBM + YR2015_waterDepth + YR2016_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2017_typhaBM ~ YR2016_typhaBM + YR2016_waterDepth + YR2017_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2018_typhaBM ~ YR2017_typhaBM + YR2017_waterDepth + YR2018_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2016_detritusAW ~ YR2015_typhaBM + YR2016_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2017_detritusAW ~ YR2016_detritusAW + YR2016_typhaBM + YR2017_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2018_detritusAW ~ YR2017_detritusAW + YR2017_typhaBM + YR2018_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2016_totalVegCover ~ YR2016_detritusAW + YR2015_totalVegCover, random = ~1|plot, data = long_data),
  lme(YR2018_totalVegCover ~ YR2017_detritusAW + YR2018_detritusAW, random = ~1|plot, data = long_data),
  lme(YR2016_aquatics ~ YR2015_aquatics + YR2015_totalVegCover + YR2016_totalVegCover, random = ~1|plot, data = long_data),
  lme(YR2017_aquatics ~ YR2016_aquatics + YR2016_totalVegCover + YR2016_detritusAW + YR2017_detritusAW, random = ~1|plot, data = long_data),
  lme(YR2018_aquatics ~ YR2017_aquatics + YR2017_detritusAW + YR2018_detritusAW + YR2018_totalVegCover, random = ~1|plot, data = long_data)
)
summary(new.modlist1, long_data)

# Adding in significant relationships
new.modlist2 <- psem(
  lme(YR2016_typhaBM ~ YR2015_typhaBM + YR2015_waterDepth + YR2016_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2017_typhaBM ~ YR2016_typhaBM + YR2016_waterDepth + YR2017_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2018_typhaBM ~ YR2017_typhaBM + YR2017_waterDepth + YR2018_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2016_detritusAW ~ YR2015_typhaBM + YR2016_typhaBM + YR2015_waterDepth + YR2016_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2017_detritusAW ~ YR2016_detritusAW + YR2016_typhaBM + YR2017_typhaBM + YR2016_waterDepth + YR2017_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2018_detritusAW ~ YR2017_detritusAW + YR2017_typhaBM + YR2018_typhaBM + YR2017_waterDepth + YR2018_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2016_totalVegCover ~ YR2016_detritusAW + YR2015_totalVegCover + YR2015_waterDepth + YR2016_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2018_totalVegCover ~ YR2017_detritusAW + YR2018_detritusAW + YR2016_totalVegCover + YR2017_waterDepth + YR2018_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2016_aquatics ~ YR2015_aquatics + YR2015_totalVegCover + YR2016_totalVegCover, random = ~1|plot, data = long_data),
  lme(YR2017_aquatics ~ YR2016_aquatics + YR2016_totalVegCover + YR2016_detritusAW + YR2017_detritusAW, random = ~1|plot, data = long_data),
  lme(YR2018_aquatics ~ YR2017_aquatics + YR2017_detritusAW + YR2018_detritusAW + YR2018_totalVegCover, random = ~1|plot, data = long_data)
)
summary(new.modlist2, long_data)

# removing unnecessary links
new.modlist2 <- psem(
  lme(YR2016_typhaBM ~ YR2015_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2017_typhaBM ~ YR2016_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2018_typhaBM ~ YR2017_typhaBM, random = ~1|plot, data = long_data),
  lme(YR2016_detritusAW ~ YR2015_typhaBM + YR2016_typhaBM + YR2015_waterDepth + YR2016_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2017_detritusAW ~ YR2016_detritusAW + YR2016_typhaBM + YR2017_typhaBM + YR2016_waterDepth + YR2017_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2018_detritusAW ~ YR2017_detritusAW + YR2017_typhaBM + YR2018_typhaBM + YR2017_waterDepth + YR2018_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2016_totalVegCover ~ YR2016_detritusAW + YR2015_totalVegCover + YR2015_waterDepth + YR2016_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2018_totalVegCover ~ YR2017_detritusAW + YR2018_detritusAW + YR2016_totalVegCover + YR2017_waterDepth + YR2018_waterDepth, random = ~1|plot, data = long_data),
  lme(YR2016_aquatics ~ YR2015_aquatics + YR2015_totalVegCover + YR2016_totalVegCover, random = ~1|plot, data = long_data),
  lme(YR2017_aquatics ~ YR2016_aquatics + YR2016_totalVegCover + YR2016_detritusAW + YR2017_detritusAW, random = ~1|plot, data = long_data),
  lme(YR2018_aquatics ~ YR2017_aquatics + YR2017_detritusAW + YR2018_detritusAW + YR2018_totalVegCover, random = ~1|plot, data = long_data)
)
summary(new.modlist2, long_data)


