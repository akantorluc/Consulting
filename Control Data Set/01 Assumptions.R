attach(Ohsowski_Control_Data)

hist(detriusAW16)

# Water
hist(waterDepth15)
hist(waterDepth16)
hist(waterDepth17)
water <- c(waterDepth15, waterDepth16, waterDepth17, waterDepth18, waterDepth19)
hist(water)

# Typha
hist(typhaBM15)
hist(typhaBM16)
hist(typhaBM17)
typ <- c(typhaBM15, typhaBM16, typhaBM17, typhaBM18)
hist(typ)

# Veg Cover
hist(totalVegCover15)
hist(totalVegCover16)
veg <- c(totalVegCover15, totalVegCover16, totalVegCover18, totalVegCover19)
hist(veg)

# Aquatics
hist(aquatics15)
hist(aquatics16)
aqu <- c(aquatics15, aquatics16, aquatics17, aquatics18)
hist(aqu)

attach(newData)
par(mfrow = c(1,1))
hist(organic) #slight right skewed, pretty normal
hist(H) #very right skewed/ zero inflated
hist(richness) #count data, right skewed?
hist(typhaDens) #normal
hist(NO3_N) #very right skewed
hist(NH4_N, breaks = 15) #slight right skewed with 1 very high outlier
hist(sedges) #really bad zero inflated but maybe useful
hist(rushes) #exclude
hist(grasses) #exclude
hist(graminoids) #normal
hist(forbs) #zero inflated (9/25)
hist(woody) #only 1 tree so exclude
hist(typCover) #normal
hist(typhaBM) #right skewed
hist(detritusAW) #right skewed with lots of zeros
hist(waterDepth) #right skewed
hist(totalVegCover) #slight right skew
hist(aquatics) #zero inflated and awful
