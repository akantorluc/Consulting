library(dplyr)


#create variables combining other plants that are not Typha and split trmt variable into dummies
Consult = Consult %>% 
          mutate(nonTypha = potber + utrmin + utrvul, trmt_above = ifelse(trmt == 'Above', 1, 0),
                 trmt_below = ifelse(trmt == 'Below', 1, 0), trmt_Mow = ifelse(trmt == 'Mow', 1, 0),
                 trmt_control = ifelse(trmt == 'Control', 1, 0), trmt_yes = ifelse(trmt == "Control", 0, 1),
                 site_isCedar = ifelse(site == 'Cedarville', 1, 0))


#high variance issues with water depth and typha so we 
#divided it by 10 to be at the same scale as the other variables
#_dec are the percent values /100 for binomial models
Consult = Consult %>% mutate(Typha10 = Typha/10, waterDepth10 = waterDepth/10, vegCover10 = vegCover/10) %>% 
                      mutate(potber_dec = potber/100, utrmin_dec=utrmin/100, 
                             utrvul_dec = utrvul/100, Typha_dec = Typha/100)

Consult_hurdle = Consult %>% mutate(Typha_gtzero = ifelse(Typha > 0, 1, 0), potber_gtzero = ifelse(potber>0, 1, 0))


#change all chr vars into fct vars
Consult[,c(1,3:5)] = lapply(Consult[,c(1,3:5)], as.factor) 

#main data set
save(Consult, file="Consult_data.Rdata")


#data sets per year
consult_2011 = Consult %>% dplyr::filter(year == 2011)
consult_2012 = Consult %>% dplyr::filter(year == 2012)
consult_2013 = Consult %>% dplyr::filter(year == 2013)

# variables by year

Consult_yearly = read.csv("Plants_By_Year.csv", header = TRUE)
Consult_yearly = Consult_yearly %>% mutate(trmt_yes = ifelse(trmt == "Control", 0, 1))
names(Consult_yearly)[1] <- "site"
