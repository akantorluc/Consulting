library(devtools)
library(piecewiseSEM)
library(lmerTest)
library(nlme)
library(gamlss)

#'typhaBM100 ~ waterDepth + H + unVegCover
#          graminoids ~ H
#          detritusAW ~ typhaBM100 + NH4_N + waterDepth + H
#          richness ~ H + NO3_N + typhaBM100 + organic
#          aquatics ~ H + NO3_N + organic + waterDepth'

pmod1 <- psem(
  lme(typhaBM100 ~ waterDepth + H + unVegCover, random = ~1|plot, data = global_data), 
  lme(graminoids ~ H, random = ~1|plot, data = global_data),
  lme(detritusAW ~ typhaBM100 + NH4_N + waterDepth + H, random = ~1|plot, data = global_data),
  lme(richness ~ H + NO3_N + typhaBM100 + organic, random = ~1|plot, data = global_data),
  lme(aquatics ~ H + NO3_N + organic + waterDepth, random = ~1|plot, data = global_data)
)
summary(pmod1)

# Adding in missing connections
pmod2 <- psem(
  lme(typhaBM100 ~ waterDepth + H + unVegCover, random = ~1|plot, data = global_data), 
  lme(graminoids ~ H + typhaBM100, random = ~1|plot, data = global_data),
  lme(detritusAW ~ typhaBM100 + NH4_N + waterDepth + H, random = ~1|plot, data = global_data),
  lme(richness ~ H + NO3_N + typhaBM100 + organic + graminoids, random = ~1|plot, data = global_data),
  lme(aquatics ~ H + NO3_N + organic + waterDepth + graminoids, random = ~1|plot, data = global_data)
)
summary(pmod2)

# Taking out insignificant terms
pmod3 <- psem(
  lme(typhaBM100 ~ unVegCover, random = ~1|plot, data = global_data), 
  lme(graminoids ~ H + unVegCover + waterDepth, random = ~1|plot, data = global_data),
  lme(detritusAW ~ typhaBM100 + NH4_N + waterDepth, random = ~1|plot, data = global_data),
  lme(richness ~ H + NO3_N + typhaBM100 + organic + graminoids, random = ~1|plot, data = global_data),
  lme(aquatics ~ H + NO3_N + waterDepth + graminoids, random = ~1|plot, data = global_data)
)
summary(pmod3) # 0.562

pmod4 <- psem(
  lme(typhaBM100 ~ unVegCover, random = ~1|plot, data = global_data), 
  lme(graminoids ~ H + unVegCover + waterDepth + typhaBM100, random = ~1|plot, data = global_data),
  lme(detritusAW ~ typhaBM100 + NH4_N + waterDepth, random = ~1|plot, data = global_data),
  lme(richness ~ H + NO3_N + typhaBM100 + organic + graminoids, random = ~1|plot, data = global_data),
  lme(aquatics ~ H + NO3_N + waterDepth + graminoids, random = ~1|plot, data = global_data)
)
summary(pmod4) # 0.825

# Best model = no missing terms or bad relationships. No graminoids response. 
pmod5 <- psem(
  lme(typhaBM100 ~ unVegCover + graminoids, random = ~1|plot, data = global_data), 
  lme(log(detritusAW+1) ~ typhaBM100 + NH4_N + waterDepth, random = ~1|plot, data = global_data),
  lme(richness ~ H + NO3_N + graminoids + typhaBM100 + organic, random = ~1|plot, data = global_data),
  lme(log(aquatics) ~ H + NO3_N + graminoids + waterDepth, random = ~1|plot, data = global_data)
)
summary(pmod5) # 0.669

# Checking model assumptions...
lmt <- glm(typhaBM100 ~ unVegCover + graminoids, data = global_data)
hist(lmt$residuals, breaks = 20)# 3 outliers so normal
plot(lmt)
par(mfrow = c(2, 2))

lmd <- glm(log(detritusAW+1) ~ typhaBM100 + NH4_N + waterDepth, data = subset(global_data, detritusAW != 99))
hist(lmd$residuals)
plot(lmd) # good enough

lmr <- glm(richness ~ H + NO3_N + graminoids + typhaBM100 + organic, data = global_data)
plot(lmr) # Looks ok
hist(lmr$residuals) # normal

lma <- glm(log(aquatics) ~ H + NO3_N + graminoids + waterDepth, data = subset(global_data, aquatics != 0))
plot(lma) # Lots of zeros
hist(lma$residuals, breaks = 20) # Heavy tailed

