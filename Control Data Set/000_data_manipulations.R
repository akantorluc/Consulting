library(tidyverse)
library(reshape2)
library(dplyr)
library(readr)

long_data = dcast(melt(newData), plot + subplot ~ year + variable, fill = NA)
long_data = long_data %>% mutate(YR2016_detritusAW = ifelse(is.na(YR2016_detritusAW), 99, YR2016_detritusAW))
long_data = long_data %>% mutate(YR2017_totalVegCover = (YR2016_totalVegCover + YR2018_totalVegCover)/2, 
                                 YR2017_unVegCover = (YR2016_unVegCover + YR2018_unVegCover)/2,
                                 YR2015_detritusAW = YR2016_detritusAW,
                                 YR2015_typhaDens = YR2016_typhaDens)
long_data = long_data %>% mutate(logYR2015_detritusAW = log(YR2015_detritusAW + 1), logYR2016_detritusAW = log(YR2016_detritusAW + 1), 
                                 logYR2017_detritusAW = log(YR2017_detritusAW + 1), logYR2018_detritusAW = log(YR2018_detritusAW + 1), 
                                 logYR2015_aquatics = log(YR2015_aquatics + 1), logYR2016_aquatics = log(YR2016_aquatics + 1), 
                                 logYR2017_aquatics = log(YR2017_aquatics + 1), logYR2018_aquatics = log(YR2018_aquatics + 1))
long_data = long_data %>% mutate(YR2015_typhaBM100 = YR2015_typhaBM / 100, YR2016_typhaBM100 = YR2016_typhaBM / 100, 
                                 YR2017_typhaBM100 = YR2017_typhaBM / 100, YR2018_typhaBM100 = YR2018_typhaBM / 100)

global_data = Global_Control_Cheboygan
global_data = global_data %>% filter(year=="YR2015" | year =="YR2016" | year=="YR2017" | year=="YR2018")
global_data[5:23] = as.data.frame(lapply(global_data[5:23], as.double))
global_data = global_data %>% mutate(typhaBM100 = typhaBM/100)

global_data = global_data %>% mutate(detritusAW10 = detritusAW/10, unVegCover10 = unVegCover / 10, 
                                     waterDepth10 = waterDepth / 10, NO3_N10 = NO3_N / 10, 
                                     organic10 = organic / 10)
global_data[1:4] = as.data.frame(lapply(global_data[1:4], as.factor))
global_data$year = as.character(global_data$year)
global_data$year = parse_number(global_data$year)
