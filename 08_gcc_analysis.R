require(raster)
library(rgdal)
library(sp)
library(sf)
library(maptools)
library(tidyverse)

###
a <- raster("D:/pace_uav/crop20180426.tif")
b <- raster("D:/pace_uav/crop20180503.tif")
c <- raster("D:/pace_uav/crop20180507.tif")
d <- raster("D:/pace_uav/crop20180521.tif")
e <- raster("D:/pace_uav/crop20180604.tif")

a <- rasterToPoints(c)

j <- a[3,3]
a[a == j] <- NA

mean(a[,3], na.rm = TRUE)
sd(a[,3], na.rm = TRUE)

#comparing
df <- read.csv("./data/pace_drone.csv")

df %>%
  filter(buffer == 20) %>%
  group_by(date) %>%
  summarize(plot.mean = mean(mean), plot.sd= sd(sd)) -> big.boi

scene.mean <- c(0.356, 0.430, 0.438, 0.452, 0.449)
scene.sd <- c(0.0266, 0.066, 0.0708, 0.076, 0.067)
doy <- c(116, 123, 127, 141, 155)



x <- data.frame(scene.mean, scene.sd, doy)

big.boi <- cbind(big.boi, x)

require(ggplot2)

x11()
ggplot(big.boi, aes(x = plot.mean, y = scene.mean))+
  geom_point(size = 3)+
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1.5)

lm.gcc <- lm(scene.mean ~ plot.mean, data = big.boi)
rsq_label.gcc <- paste('R^2 == ', round(summary(lm.gcc)$r.squared, 2))


# Function for Root Mean Squared Error
RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(lm.gcc$residuals)

# If you want, say, MAE, you can do the following:

# Function for Mean Absolute Error
mae <- function(error) { mean(abs(error)) }
mae(fit$residuals)

x11(width = 4, height = 4)
ggplot(big.boi, aes(x = plot.mean, y = scene.mean))+
  geom_errorbar(aes(ymin = scene.mean - scene.sd, ymax = scene.mean + scene.sd),
                position=position_dodge(.9))+
  geom_errorbarh(aes(xmin = plot.mean - plot.sd, xmax = plot.mean + plot.sd),
                position=position_dodge(.9))+
  geom_point(shape = 21, fill = "purple", size = 3, alpha = 1)+
  theme_bw()+
  ylab(expression(GCC[UAV]~"(Full Scene)"))+
  xlab(expression(GCC[UAV]~"(Plot Estimates at 20 m)"))+
  theme(legend.position = "none")+
  xlim(c(0.3, 0.55))+
  ylim(c(0.3, 0.55))+
  geom_abline(slope = 1, intercept = 0, color = "black", size = 1)+
  annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 2, label= "RMSE = 0.007", size = 6)

#### segmented analysis
require(segmented)

# bob <- cam
par(mar=c(5.1,4.1,4.1,2.1))
# greenness
a <- data.frame(scene.mean, scene.sd, doy)

x <- a$doy
y <- a$scene.mean
lm.green <- lm(y ~ x)

seg.green <- segmented(lm.green, seg.Z = ~x, psi = 125)
bp <-  round(seg.green$psi[2], 2)
bp.se <- round(seg.green$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = expression(paste("GCC"["UAV"])),
     xlab ="Julian Day",
     bg = "yellow",
     pch = 21,
     cex = 1.25)
plot(seg.green, add=T)
text(x = 140, y = 0.38, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 0.37, label = "Estimated Breakpoint w/ SE")


