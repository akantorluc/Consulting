# model3 <- 'Typha13 ~ Typha12 + waterDepth12 + trmt_yes + site
#          Typha12 ~ Typha11 + waterDepth11 + trmt_yes + site'
# modfit <- sem(model3, data = Consult_yearly, estimator = "mlm")
# summary(modfit) # .437

par(mfrow = c(2, 2))
plot(lm(Typha13 ~ Typha12 + waterDepth12 + trmt_yes + site, data = Consult_yearly))
#looks good as sqrt

plot(lm(Typha12 ~ Typha11 + waterDepth11 + trmt_yes + site, data = Consult_yearly))
#also looks good as sqrt

plot(lm(sqrt(Typha13) ~ sqrt(Typha12) + waterDepth12 + trmt_yes + site, data = Consult_yearly))
plot(lm(sqrt(Typha12) ~ sqrt(Typha11) + waterDepth11 + trmt_yes + site, data = Consult_yearly))


# model4 <- 'Typha13 ~ Typha12
#           Typha12 ~ Typha11 + trmt_yes
#           vegCover13 ~ Typha12 + waterDepth12
#           vegCover12 ~ vegCover11
#           vegCover13 ~~ Typha13
#           vegCover12 ~~ Typha12'
# modfit <- sem(model4, data = Consult_yearly, estimator = "mlm")
# summary(modfit) 

plot(lm(Typha13 ~ Typha12, data = Consult_yearly))
#variance is fine but not normal which is probably fine
plot(lm(Typha12 ~ Typha11 + trmt_yes, data = Consult_yearly))
#good but split due to the control vs trmt groups
plot(lm(vegCover13 ~ Typha12 + waterDepth12, data = Consult_yearly))
#good
plot(lm(vegCover12 ~ vegCover11 + waterDepth11, data = Consult_yearly))
#this is a better plot than our original model, double check model fit with this

# model5 <- 'Typha13 ~ Typha12
#           Typha12 ~ Typha11 + trmt_yes'
# modfit <- sem(model5, data = Consult_yearly, estimator = "mlm")
# summary(modfit) # .779

plot(lm(Typha13 ~ Typha12, data = Consult_yearly))
#variance is fine but not normal which is probably fine
plot(lm(Typha12 ~ Typha11 + trmt_yes, data = Consult_yearly))
#good but split due to the control vs trmt groups

plot(lm(sqrt(Typha13) ~ sqrt(Typha12), data = Consult_yearly))







