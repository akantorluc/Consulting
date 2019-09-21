library(lavaan)

mod.1 = 'waterDepth ~ year
        Typha ~ waterDepth + trmt_above + trmt_below + trmt_Mow
        nonTypha ~ Typha
        Typha ~ year'
mod = sem(mod.1, data = Consult)
summary(mod)


mod.2 = 'waterDepth ~ year
        Typha ~ waterDepth + trmt_above + trmt_below + trmt_Mow
        Typha ~ nonTypha
        Typha ~ year'
mod2 = sem(mod.2, data = Consult)
summary(mod2)


mod.3 = 'waterDepth ~ year
        Typha ~ waterDepth + trmt_above + trmt_below + trmt_Mow
        Typha ~ potber + utrmin + utrvul
        Typha ~ year
        potber ~ Typha
        utrmin ~ Typha
        utrvul ~ Typha'
mod3 = sem(mod.3, data = Consult)
#water depth and Typha coefficients should be multiplied by 10
summary(mod3)

mod.4 = 'waterDepth ~ year
        Typha ~ waterDepth + trmt_above + trmt_below + trmt_Mow
        Typha ~ nonTypha
        Typha ~ year
        nonTypha ~ year + waterDepth + trmt_above + trmt_below + trmt_Mow'
mod4 = sem(mod.4, data = Consult)
summary(mod4)