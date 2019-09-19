library(lavaan)

mod.1 = 'waterDepth ~ year
        vegCover ~ waterDepth
        vegCover ~ year'
mod = sem(mod.1, data = Consult)
summary(mod)
