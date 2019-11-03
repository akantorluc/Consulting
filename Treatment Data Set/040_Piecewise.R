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

model4 <- 'Typha13 ~ Typha12 + waterDepth12 + trmt_yes
          Typha12 ~ Typha11 + waterDepth11 + trmt_yes
          vegCover13 ~ Typha12 + vegCover12 + waterDepth12 + trmt_yes
          vegCover12 ~ Typha11 + vegCover11 + waterDepth11 + trmt_yes
          vegCover13 ~~ Typha13
          vegCover12 ~~ Typha12'
modfit <- sem(model4, data = Consult_yearly, estimator = "mlm")
summary(modfit) 

model5 <- 'Typha13 ~ Typha12
          Typha12 ~ Typha11 + trmt_yes'
modfit <- sem(model5, data = Consult_yearly, estimator = "mlm")
summary(modfit) # .779