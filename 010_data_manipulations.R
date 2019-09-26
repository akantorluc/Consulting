library(dplyr)


#create variables combining other plants that are not Typha and split trmt variable into dummies
Consult = Consult %>% 
          mutate(nonTypha = potber + utrmin + utrvul, trmt_above = ifelse(trmt == 'Above', 1, 0),
                 trmt_below = ifelse(trmt == 'Below', 1, 0), trmt_Mow = ifelse(trmt == 'Mow', 1, 0),
                 trmt_control = ifelse(trmt == 'Control', 1, 0))


#high variance issues with water depth and typha so we 
#divided it by 10 to be at teh same scal as teh otehr variables
Consult = Consult %>% mutate(Typha = Typha/10, waterDepth = waterDepth/10)

save(Consult, file="Consult_data.Rdata")
