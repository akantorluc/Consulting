library(ggplot2)
ggplot(global_data, aes(x=year, y = waterDepth, colour = plot, group=plot)) + 
 geom_smooth(method = "loess", size=2, alpha=0.15) +
  theme_classic()

ggplot(global_data, aes(x=year, y = typhaBM, colour = plot, group=plot)) + 
  geom_smooth(method = "loess", size=2, alpha=0.15) +
  theme_classic()

ggplot(global_data, aes(x=year, y = detritusAW, colour = plot, group=plot)) + 
  geom_smooth(method = "loess", size=2, alpha=0.15) +
  theme_classic()

ggplot(global_data, aes(x=year, y = totalVegCover, colour = plot, group=plot)) + 
  geom_smooth(method = "loess", size=2, alpha=0.15) +
  theme_classic()

ggplot(global_data, aes(x=year, y = aquatics, colour = plot, group=plot)) + 
  geom_smooth(method = "loess", size=2, alpha=0.15) +
  theme_classic()

#RIP XXXX-2016
ggplot(global_data, aes(x=year, y = woody, colour = plot, group=plot)) + 
  geom_smooth(method = "loess", size=2, alpha=0.15) +
  theme_classic()
