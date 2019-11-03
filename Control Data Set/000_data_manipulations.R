library(tidyverse)
library(reshape2)
long_data = dcast(melt(newData), plot + subplot ~ year + variable, fill = NA)
