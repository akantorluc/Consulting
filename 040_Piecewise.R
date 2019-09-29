library(devtools)
library(piecewiseSEM)
library(lmerTest)
library(nlme)

# piecewise SEM model without plots in it
# We have two hierarchies. Between plot and years within plot
Consult.modlist <- list(
  lme(waterDepth ~ year, random = ~1|plot, data = Consult), 
  lme(Typha ~ waterDepth + trmt_yes, random = ~1|plot, data = Consult), 
  glmer(potber ~ waterDepth + Typha + (1|plot), family = poisson(link = "log"), data = Consult))
sem.fit(Consult.modlist,Consult)

Consult.modlist <- psem(
  lme(waterDepth ~ year, random = ~1|plot, data = Consult), 
  lme(Typha ~ waterDepth + trmt_yes, random = ~1|plot, data = Consult), 
  glmer(potber ~ waterDepth + Typha + (1|plot), family = poisson(link = "log"), data = Consult))
summary(Consult.modlist)

# Piecewise links
# https://jonlefcheck.net/2014/07/06/piecewise-structural-equation-modeling-in-ecological-research/
# https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/08-1034.1
