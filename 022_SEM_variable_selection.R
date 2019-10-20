library(lavaan)
library(AICcmodavg)


mod.full = 'waterDepth10 ~ year + site
          trmt_yes ~ waterDepth10
          Typha10 ~ trmt_yes + year + site + waterDepth10
          potber ~ Typha10
          utrmin ~ Typha10
          utrvul ~ Typha10'
mod_full = sem(mod.full, data = Consult, estimator = "mlm")
summary(mod_full) #0.032

mod.no_trtwatr = 'waterDepth10 ~ year + site
          Typha10 ~ trmt_yes + year + site + waterDepth10
          potber ~ Typha10
          utrmin ~ Typha10
          utrvul ~ Typha10'
mod_no_trtwatr = sem(mod.no_trtwatr, data = Consult, estimator = "mlm")
summary(mod_no_trtwatr) #0.019

mod.no_watr = 'waterDepth10 ~ year + site
          Typha10 ~ trmt_yes + year + site
          potber ~ Typha10
          utrmin ~ Typha10
          utrvul ~ Typha10'
mod_no_watr = sem(mod.no_watr, data = Consult, estimator = "mlm")
summary(mod_no_watr) #0.119

mod.no_site = 'waterDepth10 ~ year + site
          Typha10 ~ trmt_yes + year + waterDepth10
          potber ~ Typha10
          utrmin ~ Typha10
          utrvul ~ Typha10'
mod_no_site = sem(mod.no_site, data = Consult, estimator = "mlm")
summary(mod_no_site)

mod.no_sitewatr = 'waterDepth10 ~ year + site
          Typha10 ~ trmt_yes + year
          potber ~ Typha10
          utrmin ~ Typha10
          utrvul ~ Typha10'
mod_no_sitewatr = sem(mod.no_sitewatr, data = Consult, estimator = "mlm")
summary(mod_no_sitewatr)

mod.no_sitewatryr = 'waterDepth10 ~ year + site
                  Typha10 ~ trmt_yes
                  potber ~ Typha10
                  utrmin ~ Typha10
                  utrvul ~ Typha10'
mod_no_sitewatryr = sem(mod.no_sitewatryr, data = Consult, estimator = "mlm")
summary(mod_no_sitewatryr)

#lowest AIC is mod_no_sitewatr and simplest model
aictab(list(mod_full, mod_no_site, mod_no_sitewatr, mod_no_sitewatryr, mod_no_trtwatr, mod_no_watr),
              c("full", "no site", "no sitewatr", "no sitewatryr", "no trtwatr", "no watr"))

