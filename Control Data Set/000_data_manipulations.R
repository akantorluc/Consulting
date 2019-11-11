library(tidyverse)
library(reshape2)
library(dplyr)
long_data = dcast(melt(newData), plot + subplot ~ year + variable, fill = NA)
long_data = long_data %>% mutate(YR2016_detritusAW = ifelse(is.na(YR2016_detritusAW), 99, YR2016_detritusAW))
long_data = long_data %>% mutate(YR2017_totalVegCover = (YR2016_totalVegCover + YR2018_totalVegCover)/2, 
                                 YR2017_unVegCover = (YR2016_unVegCover + YR2018_unVegCover)/2,
                                 YR2015_detritusAW = YR2016_detritusAW,
                                 YR2015_typhaDens = YR2016_typhaDens)
global_data = Global_Control_Cheboygan
global_data = global_data %>% filter(year=="YR2015" | year =="YR2016" | year=="YR2017" | year=="YR2018")
global_data[5:23] = as.data.frame(lapply(global_data[5:23], as.double))
global_data = global_data %>% mutate(typhaBM100 = typhaBM/100)
