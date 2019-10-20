library(devtools)
library(piecewiseSEM)
library(lmerTest)
library(nlme)
library(gamlss)

# piecewise SEM model without plots in it
# We have two hierarchies. Between plot and years within plot
Consult.modlist <- list(
  lme(waterDepth ~ year, random = ~1|plot, data = Consult), 
  lme(Typha ~ waterDepth + trmt_yes, random = ~1|plot, data = Consult), 
  glmer(potber ~ waterDepth + Typha + (1|plot), family = poisson(link = "log"), data = Consult)
  )
summary(Consult.modlist,Consult)

Consult.modlist <- psem(
  lmer(waterDepth ~ year + (1|plot), data = Consult_hurdle),
  glmer(Typha_gtzero ~ waterDepth + trmt_yes + (1|plot), family = binomial(link=logit), data=Consult_hurdle),
  glmer(Typha_dec ~ waterDepth + trmt_yes + (1|plot), 
      family = Gamma(link=log), data = subset(Consult_hurdle, Typha_gtzero == 1)),
  glm(potber_gtzero ~ 1, family = binomial(link=logit), data=Consult_hurdle),
  glmer(potber_dec ~ waterDepth + Typha_dec + (1|plot), 
      family = Gamma(link=log), data = subset(Consult_hurdle, potber_gtzero == 1))
  )
summary(Consult.modlist)

#try psem for binomials and then psem for gamma stuff
Consult.modlist <- psem(
  glm(waterDepth ~ year + site, data = Consult_hurdle),
  glm(Typha_gtzero ~ year + trmt_yes, family = binomial(link=logit), data=Consult_hurdle),
  glm(Typha_dec ~ year + trmt_yes, 
        family = poisson(link=log), data = subset(Consult_hurdle, Typha_gtzero == 1)),
  glm(potber_gtzero ~ Typha_dec, family = binomial(link=logit), data=Consult_hurdle),
  glm(potber_dec ~ Typha_dec, 
        family = poisson(link=log), data = subset(Consult_hurdle, potber_gtzero == 1))
)
summary(Consult.modlist, conserve = TRUE)

# try the same thing with binomial instead of gamma: p = .648
Consult.modlist <- psem(
  glm(waterDepth ~ year, data = Consult_hurdle),
  glm(Typha_gtzero ~ waterDepth + trmt_yes, family = binomial(link=logit), data=Consult_hurdle),
  glm(Typha_dec ~ waterDepth + trmt_yes, 
      family = binomial(link = "logit"), data = subset(Consult_hurdle, Typha_gtzero == 1)),
  glm(potber_gtzero ~ waterDepth + Typha_dec, family = binomial(link=logit), data=Consult_hurdle),
  glm(potber_dec ~ waterDepth + Typha_dec, 
      family = binomial(link = "logit"), data = subset(Consult_hurdle, potber_gtzero == 1))
)
summary(Consult.modlist, Consult_hurdle)

# Model without potber ~ waterDepth: p = .746
Consult.modlist <- psem(
  glm(waterDepth ~ year, data = Consult_hurdle),
  glm(Typha_gtzero ~ waterDepth + trmt_yes, family = binomial(link=logit), data=Consult_hurdle),
  glm(Typha_dec ~ waterDepth + trmt_yes, 
      family = binomial(link = "logit"), data = subset(Consult_hurdle, Typha_gtzero == 1)),
  glm(potber_gtzero ~ waterDepth + Typha_dec, family = binomial(link=logit), data=Consult_hurdle),
  glm(potber_dec ~ Typha_dec, 
      family = binomial(link = "logit"), data = subset(Consult_hurdle, potber_gtzero == 1))
)
summary(Consult.modlist, Consult_hurdle)

# Piecewise links
# https://jonlefcheck.net/2014/07/06/piecewise-structural-equation-modeling-in-ecological-research/
# https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/08-1034.1
