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

