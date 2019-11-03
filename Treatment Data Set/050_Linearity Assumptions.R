waterDepth10 ~ year + site
Typha10 ~ trmt_yes + year
potber ~ Typha10
utrmin ~ Typha10
utrvul ~ Typha10

attach(Consult)

mod_wd_y<- lm(waterDepth10 ~ year)
mod_wd_s<- lm(waterDepth10 ~ site) # Looks great

mod_Typ_y <- lm(Typha10 ~ year)
mod_Typ_trmt <- lm(Typha10^.5 ~ trmt_yes) # This transformation looks good

mod_pot <- lm(1 + potber ~ Typha10)
mod_pot_1 <- lm(log1p(potber) ~ log1p(Typha10)) # Log transformation
mod_pot_2 <- lm(-1/(1+potber)**2 ~ Typha10) # Negative inverse

mod_min <- lm(1 + utrmin ~ Typha10)
mod_vul <- lm(1 + utrvul ~ Typha10)

par(mfrow = c(2, 2))
plot(mod_Typ_trmt)

# Trying to transform the responses
library(MASS)
pot_bc <- boxcox(mod_pot, lambda = seq(-2, 2))
best.lam <- pot_bc$x[which(pot_bc$y == max(pot_bc$y))]
pot_bc_best_lam <- lm((1+potber)^-3 ~ Typha10)
plot(pot_bc_best_lam)

min_bc <- boxcox(mod_min, lambda = seq(-5, 2))
best.lam <- min_bc$x[which(min_bc$y == max(min_bc$y))]
best.lam
min_bc_best_lam <- lm((1+utrmin)^-4 ~ Typha10)
plot(min_bc_best_lam)

# Test for waterDepth on typha which is in piecewise model
mod_Typ_w <- lm(Typha10^.5 ~ waterDepth)
par(mfrow = c(2, 2))
plot(mod_Typ_w)