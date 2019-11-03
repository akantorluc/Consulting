library(lavaan)
library(AICcmodavg)

mod.full = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
          potber ~ site_isCedar + Typha10 + waterDepth10
          utrmin ~ site_isCedar + Typha10 + waterDepth10
          utrvul ~ site_isCedar + Typha10 + waterDepth10
          vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + trmt_yes + potber + utrmin + utrvul'
mod_full = sem(mod.full, data = Consult, estimator = "mlm")
summary(mod_full)


mod.1 = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
          potber ~ Typha10 + waterDepth10
          utrmin ~ site_isCedar + Typha10 + waterDepth10
          utrvul ~ site_isCedar + Typha10 + waterDepth10
          vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + trmt_yes + potber + utrmin + utrvul'
mod_1 = sem(mod.1, data = Consult, estimator = "mlm")
summary(mod_1)

mod.2 = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
          potber ~ Typha10 + waterDepth10
          utrmin ~ site_isCedar + Typha10
          utrvul ~ site_isCedar + Typha10 + waterDepth10
          vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + trmt_yes + potber + utrmin + utrvul'
mod_2 = sem(mod.2, data = Consult, estimator = "mlm")
summary(mod_2)

mod.3 = 'waterDepth10 ~ year + site_isCedar
        Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
        potber ~ Typha10 + waterDepth10
        utrmin ~ site_isCedar + Typha10
        utrvul ~ site_isCedar + Typha10 + waterDepth10
        vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + potber + utrmin + utrvul'
mod_3 = sem(mod.3, data = Consult, estimator = "mlm")
summary(mod_3)


mod.4 = 'waterDepth10 ~ year + site_isCedar
        Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
        potber ~ Typha10
        utrmin ~ site_isCedar + Typha10
        utrvul ~ site_isCedar + Typha10 + waterDepth10
        vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + potber + utrmin + utrvul'
mod_4 = sem(mod.4, data = Consult, estimator = "mlm")
summary(mod_4)


mod.5 = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
          potber ~ Typha10
          utrmin ~ Typha10
          utrvul ~ site_isCedar + Typha10 + waterDepth10
          vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + potber + utrmin + utrvul'
mod_5 = sem(mod.5, data = Consult, estimator = "mlm")
summary(mod_5)

#5 is best so far
aictab(list(mod_full, mod_1, mod_2, mod_3, mod_4, mod_5),
       c("full", "1", "2", "3", "4", "5"))

mod.6 = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
          potber ~ Typha10
          utrvul ~ site_isCedar + Typha10 + waterDepth10
          vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + potber + utrmin + utrvul'
mod_6 = sem(mod.6, data = Consult, estimator = "mlm")
summary(mod_6)

mod.7 = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
          potber ~ Typha10
          utrvul ~ site_isCedar + Typha10 + waterDepth10
          vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + utrmin + utrvul'
mod_7 = sem(mod.7, data = Consult, estimator = "mlm")
summary(mod_7)

mod.8 = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
          potber ~ Typha10
          utrvul ~ site_isCedar + waterDepth10
          vegCover10 ~ site_isCedar  + Typha10 + waterDepth10 + utrmin + utrvul'
mod_8 = sem(mod.8, data = Consult, estimator = "mlm")
summary(mod_8)

mod.9 = 'waterDepth10 ~ year + site_isCedar
        Typha10 ~ site_isCedar + waterDepth10 + trmt_yes
        potber ~ Typha10
        utrvul ~ site_isCedar + waterDepth10
        vegCover10 ~ site_isCedar + Typha10 + waterDepth10 + utrmin'
mod_9 = sem(mod.9, data = Consult, estimator = "mlm")
summary(mod_9)

#model 8 is best so far
aictab(list(mod_5, mod_6, mod_7, mod_8, mod_9),
       c("5", "6", "7", "8", "9"))

mod.10 = 'waterDepth10 ~ year + site_isCedar
          vegCover10 ~ site_isCedar + potber + utrmin + waterDepth10'
mod_10 = sem(mod.10, data = Consult, estimator = "mlm")
summary(mod_10)

mod.11 = 'waterDepth10 ~ year + site_isCedar
          Typha10 ~ waterDepth10
          potber ~ Typha10 + waterDepth10
          utrmin ~ Typha10 + waterDepth10
          utrvul ~ Typha10 + waterDepth10
          vegCover10 ~ site_isCedar + waterDepth10 + trmt_yes'
mod_11 = sem(mod.11, data = Consult, estimator = "mlm")
summary(mod_11)

aictab(list(mod_11, mod_10),
       c("11", "10"))
