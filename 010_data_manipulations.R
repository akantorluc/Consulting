library(dplyr)

#create variables combining otehr plants that are not Typha and split trmt variable into dummies
Consult = Consult %>% 
          mutate(nonTypha = potber + utrmin + utrvul, trmt_above = ifelse(trmt == 'Above', 1, 0),
                 trmt_below = ifelse(trmt == 'Below', 1, 0), trmt_Mow = ifelse(trmt == 'Mow', 1, 0))
