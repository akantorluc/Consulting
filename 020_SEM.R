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


#after talking with Brian
mod.5 = 'waterDepth ~ year
        potber ~ waterDepth + trmt_above + trmt_below + trmt_Mow + Typha + year'
mod5 = sem(mod.5, data = Consult, estimator = "mlm")
summary(mod5)
# New test stat is 6.16/.878, .135 p-value

mod.51 = 'waterDepth ~ year
        potber ~ Typha + year'
mod51 = sem(mod.51, data = Consult, estimator = "mlm")
summary(mod51)

# trmt_yes is in models 6-8 instead of individual treatments. Got better p-values. 
mod.6 = 'waterDepth ~ year
        Typha ~ trmt_yes +year
        potber ~ Typha + year'
mod6 = sem(mod.6, data = Consult, estimator = "mlm")
summary(mod6)

mod.7 = 'waterDepth ~ year
        Typha ~ trmt_yes + year
        utrmin ~ Typha'
mod7 = sem(mod.7, data = Consult, estimator = "mlm")
summary(mod7)

mod.8 = 'waterDepth ~ year
        Typha ~ trmt_yes + year
        utrvul ~ Typha'
mod8 = sem(mod.8, data = Consult, estimator = "mlm")
summary(mod8)

# 2011 models
mod.9 = 'Typha ~ trmt_above + trmt_below + trmt_Mow + waterDepth
        utrvul ~ Typha + waterDepth'
mod9 = sem(mod.9, data = consult_2013, estimator = "mlm")
summary(mod9)
