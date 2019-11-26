library(piecewiseSEM)
library(lmerTest)
library(nlme)
# # Adding in year - best model!!!!!!!!!!!
# pmod7 <- psem(
#   lme(typhaBM100 ~ unVegCover + graminoids + NO3_N + organic + year, random = ~1|plot, data = global_data), 
#   lme(log(detritusAW+1) ~ waterDepth + typhaBM100, random = ~1|plot, data = global_data),
#   lme(richness ~ H + NO3_N + graminoids + typhaBM100 + organic, random = ~1|plot, data = global_data),
#   lme(log(aquatics+1) ~ H + NO3_N + graminoids + waterDepth + typhaBM100 + richness + year, random = ~1|plot, data = global_data)
# )
# summary(pmod7) # .803

pmod8 <- psem(
    lme(typhaBM100 ~ totalVegCover + year, random = ~1|plot, data = global_data),
    lme(log(detritusAW+1) ~ waterDepth + typhaBM100, random = ~1|plot, data = global_data),
    lme(richness ~ H + NO3_N + graminoids + typhaBM100 + organic, random = ~1|plot, data = global_data),
    lme(log(aquatics+1) ~ H + NO3_N + graminoids + waterDepth + typhaBM100 + richness + year, random = ~1|plot, data = global_data)
)
summary(pmod8)


#new bestest model
pmod9 <- psem(
  lme(typhaBM100 ~ totalVegCover + year, random = ~1|plot, data = global_data),
  lme(log(detritusAW+1) ~ waterDepth + typhaBM100, random = ~1|plot, data = global_data),
  lme(H ~ richness + NO3_N + graminoids + typhaBM100 + organic, random = ~1|plot, data = global_data),
  lme(log(aquatics+1) ~ H + NO3_N + graminoids + waterDepth + typhaBM100 + richness, random = ~1|plot, data = global_data)
)
summary(pmod9)
############check assumptions mod9######################
par(mfrow = c(2, 2))
lmt <- glm(typhaBM100 ~ totalVegCover + year, data = global_data)
hist(lmt$residuals, breaks = 20)# 3 outliers so normal
plot(lmt)

lmt <- glm(H ~ richness + NO3_N + graminoids + typhaBM100 + organic, data = global_data)
hist(lmt$residuals, breaks = 20)# 3 outliers so normal
plot(lmt)
##################################################

pmod10 <- psem(
  lme(typhaBM100 ~ graminoids + year, random = ~1|plot, data = global_data),
  lme(log(detritusAW+1) ~ waterDepth + typhaBM100, random = ~1|plot, data = global_data),
  lme(totalVegCover ~ graminoids + NO3_N + typhaBM100, random = ~1|plot, data = global_data),
  lme(H ~ richness + NO3_N + graminoids + typhaBM100 + organic, random = ~1|plot, data = global_data),
  lme(log(aquatics+1) ~ H + NO3_N + graminoids + waterDepth + typhaBM100 + richness, random = ~1|plot, data = global_data)
)
summary(pmod10)

#######################checking assumptions mod10###########################
lmt <- glm(typhaBM100 ~ graminoids + year, data = global_data)
hist(lmt$residuals, breaks = 20)# 3 outliers so normal
plot(lmt)

lmt <- glm(totalVegCover ~ graminoids + NO3_N + typhaBM100, data = global_data)
hist(lmt$residuals, breaks = 20)# 3 outliers so normal
plot(lmt)
##################################################



















