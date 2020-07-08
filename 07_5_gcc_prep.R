require(raster)
library(rgdal)
library(sp)
library(sf)
library(maptools)
library(tidyverse)

###
# band 1 - red
# band 2 - green
# band 3 - blue
# band 4 - IR

df.0426 <- stack("D:/pace_uav/20180426.tif")
df.0503 <- stack("D:/pace_uav/20180503.tif")
df.0507 <- stack("D:/pace_uav/20180507.tif")
df.0521 <- stack("D:/pace_uav/20180521.tif")
df.0604 <- stack("D:/pace_uav/20180604.tif")

# tim <- raster("D:/pace_uav/20180426.tif")

# extent format (xmin,xmax,ymin,ymax)
new.extent <- extent(-78.276, -78.271, 37.9225, 37.9235)
#new.extent.0507 <- extent(-78.276, -78.274, 37.9225, 37.9235)
new.extent
class(new.extent)
##### check area
spy_prj <- rasterToPolygons(df.0426)

## calculate polygon area 
library(rgeos)

gArea(spy_prj[1, ])


x11()
plotRGB(df.0507, r = 1, g = 2, b = 3)

# crop to extent
df.0426 <- crop(x = df.0426, y = new.extent)
df.0503 <- crop(x = df.0503, y = new.extent)
df.0507 <- crop(x = df.0507, y = new.extent)
df.0521 <- crop(x = df.0521, y = new.extent)
df.0604 <- crop(x = df.0604, y = new.extent)

#### GCc
x <- df.0507
y <- df.0604
df <- x[[2]] / (x[[1]] + x[[2]] + x[[3]])
df2 <- y[[2]] / (y[[1]] + y[[2]] + y[[3]])



# Define the Proj.4 spatial reference 
# http://spatialreference.org/ref/epsg/26915/proj4/
# project raster
new.crs <- "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84" 

# Project Raster
utm.tim <- projectRaster(tim, crs = new.crs)

utm.df <- projectRaster(df, crs = new.crs)
writeRaster(utm.df, "crop20180507.tif")

utm.0503 <- projectRaster(df.0503, crs = new.crs)
utm.0507 <- projectRaster(df.0507, crs = new.crs)
utm.0521 <- projectRaster(df.0521, crs = new.crs)
utm.0604 <- projectRaster(df.0604, crs = new.crs)


x11()
raster::plot(df)

x11()
x.0507 <- as.data.frame(df.0507)
x.0507 %>%
  filter(X20180507 < 254) -> x.0507

x.0426 <- as.data.frame(df.0426)
x.0503 <- as.data.frame(df.0503)
x.0521 <- as.data.frame(df.0521)
x.0604 <- as.data.frame(df.0604)

# filter down
x.0426 %>%
  filter(X20180426 < 254) -> x.0426

mean(x.0604$X20180604)
x.0503 %>%
  filter(X20180503 < 254) -> x.0503

mean(x.0503$X20180503)
sd(x.0503$X20180503)



x11()
hist(x.0507$X20180507)

mean(df.0426)
sd(df.0426)
mean(x.0507$X20180507)
sd(x.0507$X20180507)

x11()
ggplot(x.0507, aes(x = X20180507)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  xlab("Gcc")
#### BRING IN PLOT Points

# Define the Proj.4 spatial reference 
# http://spatialreference.org/ref/epsg/26915/proj4/
# project raster
new.crs <- "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84" 

# Project Raster
utm.0426 <- projectRaster(df.0426, crs = new.crs)
utm.0503 <- projectRaster(df.0503, crs = new.crs)
utm.0507 <- projectRaster(df.0507, crs = new.crs)
utm.0521 <- projectRaster(df.0521, crs = new.crs)
utm.0604 <- projectRaster(df.0604, crs = new.crs)