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

df.0426 <- raster("D:/pace_uav/20180426.tif", band = 2)
df.0503 <- raster("D:/pace_uav/20180503.tif", band = 2)
df.0507 <- raster("D:/pace_uav/20180507.tif", band = 2)
df.0521 <- raster("D:/pace_uav/20180521.tif", band = 2)
df.0604 <- raster("D:/pace_uav/20180604.tif", band = 2)


# extent format (xmin,xmax,ymin,ymax)
new.extent <- extent(-78.276, -78.271, 37.9225, 37.9235)
#new.extent.0507 <- extent(-78.276, -78.274, 37.9225, 37.9235)
new.extent
class(new.extent)

# bring in plot centers
pts <- readOGR("./data/gis/TLS_plots.shp")

pts.short <- data.frame(pts)

pts.short %>%
        filter(Id < 10) %>%
        data.frame() -> pts.labels

m.pts <- st_as_sf(x = pts.labels, 
                  coords = c("coords.x1", "coords.x2"),
                  crs = "+proj=utm +datum=WGS84")


df.0426 <- crop(x = df.0426, y = new.extent)
df.0503 <- crop(x = df.0503, y = new.extent)
df.0507 <- crop(x = df.0507, y = new.extent)
df.0521 <- crop(x = df.0521, y = new.extent)
df.0604 <- crop(x = df.0604, y = new.extent)

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
mean(x.0426$X20180426)
sd(x.0426$X20180426)
dim(x.0426)

mean(x.0604$X20180604)
x.0503 %>%
        filter(X20180503 < 254) -> x.0503

mean(x.0503$X20180503)
sd(x.0503$X20180503)
dim(x.0503)


#  05-07
x.0507 %>%
        filter(X20180507 < 254) -> x.0507
mean(x.0507$X20180507)
sd(x.0507$X20180507)
dim(x.0507)

x11()
ggplot(x.0507, aes(x = X20180507)) +
        geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
        theme_minimal()+
        xlab("Gcc")

# 05 21
x.0521 %>%
        filter(X20180521 < 254) -> x.0521
mean(x.0521$X20180521)
sd(x.0521$X20180521)
dim(x.0521)

# 6 / 04
x.0604 %>%
        filter(X20180604 < 254) -> x.0604
mean(x.0604$X20180604)
sd(x.0604$X20180604)
dim(x.0604)
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



# Plot UAV 0426 DOY126
x11(width = 8, height = 3.43)
raster::plot(utm.0426,
             main = "DOY 117",
             legend.args = list(text = 'Gcc', side = 4, 
                                font = 2, line = 2.5, cex = 0.8))
plot(m.pts,
     pch = 19,
     col = "black",
     # size the points according to the temp value
     add = TRUE)
text(m.pts$Id,  pos = 4, offset = 0.7)

# Plot UAV 0503 DOY124
x11(width = 8, height = 3.43)
raster::plot(utm.0503,
             main = "DOY 124",
             legend.args = list(text = 'Gcc', side = 4, 
                                font = 2, line = 2.5, cex = 0.8))
plot(m.pts,
     pch = 19,
     col = "black",
     # size the points according to the temp value
     add = TRUE)
text(m.pts$Id,  pos = 4, offset = 0.7)

# Plot UAV 0507
x11(width = 8, height = 3.43)
raster::plot(utm.0507,
             main = "DOY 128",
             legend.args = list(text = 'Gcc', side = 4, 
                                font = 2, line = 2.5, cex = 0.8))
plot(m.pts,
     pch = 19,
     col = "black",
     # size the points according to the temp value
     add = TRUE)
text(m.pts$Id,  pos = 4, offset = 0.7)

# Plot UAV 0521
x11(width = 8, height = 3.43)
raster::plot(utm.0521,
             main = "DOY 142",
             legend.args = list(text = 'Gcc', side = 4, 
                                font = 2, line = 2.5, cex = 0.8))
plot(m.pts,
     pch = 19,
     col = "black",
     # size the points according to the temp value
     add = TRUE)
text(m.pts$Id,  pos = 4, offset = 0.7)

# Plot UAV 0604 DOY 156
x11(width = 8, height = 3.43)
raster::plot(utm.0604,
             main = "DOY 156",
             legend.args = list(text = 'Gcc', side = 4, 
                                font = 2, line = 2.5, cex = 0.8))
plot(m.pts,
     pch = 19,
     col = "black",
     # size the points according to the temp value
     add = TRUE)
text(m.pts$Id,  pos = 4, offset = 0.7)






####################### bufferss
df <- utm.0604
ya.boi <- as.Date("2018-06-04")
# 20 meters!
x <- raster::extract(df, 
                     pts, 
                     buffer = 20,
                     fun = mean,
                     df = TRUE)

xx <- data.frame(ID = pts$Id, x)

names(xx) <- c("id", "ID", "mean")
xx$sd <- raster::extract(df, 
                         pts, 
                         buffer = 20,
                         fun = sd)
xx$date <- ya.boi
xx$buffer <- 20

xxx <- xx[c("id", "date", "buffer", "mean", "sd")]

# 10 meters!
y <- raster::extract(df, 
                     pts, 
                     buffer = 10,
                     fun = mean,
                     df = TRUE)
yy <- data.frame(ID = pts$Id, y)

names(yy) <- c("id", "ID", "mean")
yy$sd <- raster::extract(df, 
                         pts, 
                         buffer = 10,
                         fun = sd)
yy$date <- ya.boi
yy$buffer <- 10

yyy <- yy[c("id", "date", "buffer", "mean", "sd")]

# 5 meters   
z <- raster::extract(df, 
                     pts, 
                     buffer = 5,
                     fun = mean,
                     na.rm = TRUE,
                     df = TRUE)

zz <- data.frame(ID = pts$Id, z)

names(zz) <- c("id", "ID", "mean")
zz$sd <- raster::extract(df, 
                         pts, 
                         buffer = 5,
                         fun = sd)

zz$date <- ya.boi
zz$buffer <- 5

zzz <- zz[c("id", "date", "buffer", "mean", "sd")]

# bring them all together
al <- rbind(zzz, yyy, xxx)

# only the ids we care about and have camera data
al %>%
        filter(id < 10) %>%
        data.frame() -> bob.20180604

#####
uav.green <- rbind(bob.20180426, bob.20180503, bob.20180507, bob.20180521, bob.20180604)












####
write.csv(uav.green, "./data/pace_drone.csv")
x <- as.data.frame(new.df)
colnames(x) <- ("green")
x$green <- x$green/255

mean(x$green)
sd(x$green)

# for note all but 2018-05-07 is based on 66258000 while 05/07 is 26503200
x.0426 <- as.data.frame(utm.0426)
mean.green <- c(0.628, 0.554, 0.477, 0.502, 0.529)
sd.green <- c(0.182, 0.232, 0.214, 0.222, 0.261)
dates <- c("2018-04-26", "2018-05-03", "2018-05-07", "2018-05-21", "2018-06-04")
              
uav <- data.frame(mean.green, sd.green, dates)

uav$dates <- as.Date(uav$dates)
uav$jd <- as.integer(format(uav$dates, "%j"))


#####   BREAKPOINT
require(segmented)
x <- uav$jd
y <- uav$mean.green
lm.uav <- lm(y ~ x)

seg.uav <- segmented(lm.uav, seg.Z = ~x, psi = 125)
bp <-  round(seg.uav$psi[2], 2) 
bp.se <- round(seg.uav$psi[3], 2) 

x11(width = 4, height = 4)
plot(x,y,
     ylab = "UAV Stand Greenness",
     xlab ="Julian Day",
     col = "#66a61e",
     pch = 19,
     cex = 2)
plot(seg.uav, add=T)
text(x = 140, y = 0.55, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 0.58, label = "Estimated Breakpoint w/ SE")



#####   BREAKPOINT
require(segmented)
# with the ndvi now
x <- uav$jd
y <- c(0.057, 0.587, 0.728, 0.770, 0.790)
lm.ndvi <- lm(y ~ x)

seg.uav <- segmented(lm.ndvi, seg.Z = ~x, psi = 125)
bp <-  round(seg.uav$psi[2], 2) 
bp.se <- round(seg.uav$psi[3], 2) 

x11(width = 4, height = 4)
plot(x,y,
     ylab = "NDVI",
     xlab ="Julian Day",
     col = "dodger blue",
     pch = 19,
     cex = 2)
plot(seg.uav, add=T)
text(x = 140, y = 0.4, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 0.48, label = "Estimated Breakpoint w/ SE")
