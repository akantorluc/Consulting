library(lavaan)

mod1 <- 'Typha ~ waterDepth
          detritus ~ Typha
          TotalVegCover ~ detritus
          aquatics ~ TotalVegCover + detritus'
mod11 = sem(mod1, data = newdata)
summary(mod11)