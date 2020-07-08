require(raster)
require(rgdal)
require(viridis)
require(tidyverse)
require(pwr)

#
df.0426 <- raster("D:/pace_uav/crop20180426.tif")
df.0503 <- raster("D:/pace_uav/crop20180503.tif")
df.0507 <- raster("D:/pace_uav/crop20180507.tif")
df.0521 <- raster("D:/pace_uav/crop20180521.tif")
df.0604 <- raster("D:/pace_uav/crop20180604.tif")

# change raster resolution
res(df.0426)


# change raster resolution
df.0426_20m <- aggregate(df.0426, fact = 666.667)
df.0503_20m <- aggregate(df.0503, fact = 500)
df.0507_20m <- aggregate(df.0507, fact = 500)
df.0521_20m <- aggregate(df.0521, fact = 500)
df.0604_20m <- aggregate(df.0604, fact = 500)

####### POWER ANALYSIS
x.0426 <- as.data.frame(df.0426_20m)

x.0426 <- na.omit(x.0426)
# 
mean(x.0426$crop20180426)
sd(x.0426$crop20180426)
dim(x.0426)

pwr.t.test(n = , d = , sig.level = , power = , type = c("two.sample", "one.sample", "paired"))














# now let's model this thing
y <- 65354.32 * exp(-21.55 * x)
y2 <- 65354.32 * exp(-21.55 * z) 

x11(width = 8, height = 3.73)
plot(y, col = (viridis(48)), zlim = c(0,50))

x11(width = 8, height = 3.73)
plot(y2, col = (viridis(48)), zlim = c(0,50))

# resize
##Resample r1 to extent/resolution of r2
y1 <- resample(y, y2, resample='bilinear')

##Create stack 'r1r2stk'
w <- stack(y1, y2)
# set colors
cols <- colorRampPalette(brewer.pal(9,"BuPu"))

# use gsub to modify label names.that we'll use for the plot 
rasterNames.gf  <- names(w)

# view Names
rasterNames.gf <- gsub("crop", "", rasterNames.gf)

x11(height = 4)
levelplot(w, col.regions = cols,
          main="Gap Fraction Modelled from UAV GCC",
          names.attr= c("April 24, 2018", "June 04, 2018"))

################### LAI MODEL
################### 
# Call:
#   lm(formula = lai ~ gcc, data = bob.15)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.35288 -0.20035 -0.04912  0.37737  1.10196 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -7.7121     0.9477  -8.138 4.39e-09 ***
#   gcc          25.7696     2.2103  11.659 1.14e-12 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.538 on 30 degrees of freedom
# Multiple R-squared:  0.8192,	Adjusted R-squared:  0.8132 
# F-statistic: 135.9 on 1 and 30 DF,  p-value: 1.143e-12
j <- aggregate(df.0426, fact = 667)
k <- aggregate(df.0604, fact = 667)
res(x)

# now let's model this thing
m <- (25.7696 * j) - 7.7121
n <- (25.7696 * k) - 7.7121 

##Resample r1 to extent/resolution of r2
o <- resample(m, n, resample='bilinear')

##Create stack 'r1r2stk'
q <- stack(o, n)
require(rasterVis)

# format plots

# set colors
cols <- colorRampPalette(brewer.pal(9,"Oranges"))

# use gsub to modify label names.that we'll use for the plot 
rasterNames  <- names(q)

# view Names
rasterNames <- gsub("crop", "", rasterNames)

x11(height = 4)
levelplot(q, col.regions = cols,
          main="LAI Modelled from UAV GCC",
          names.attr= c("April 24, 2018", "June 04, 2018"))

x11(width = 8, height = 3.73)
plot(m, col = rev(viridis(48)), zlim = c(0,5))

x11(width = 8, height = 3.73)
plot(n, col = rev(viridis(48)), zlim = c(0,5))

