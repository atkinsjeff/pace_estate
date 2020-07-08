# bring in plot centers
pts <- readOGR("./data/gis/TLS_plots.shp")

pts.short <- data.frame(pts)

pts.short %>%
  filter(Id < 10) %>%
  data.frame() -> pts.labels

# new.pts <- pts.labels[, c(1, 4, 5)]
# 
# names(new.pts)[2] <- "X"
# names(new.pts)[3] <- "Y"
# 
# coordinates(new.pts) <- c("X", "Y")
# CRS(new.pts) <-  "+proj=utm +zone=17 +datum=WGS84"
# proj4string(new.pts) <- CRS("+proj=longlat +datum=WGS84")  ## for example
# 
# res <- spTransform(new.pts, CRS("+proj=utm +zone=17 ellps=WGS84"))

m.pts <- st_as_sf(x = pts, 
                  coords = c("coords.x1", "coords.x2"),
                  crs = "+proj=utm +datum=WGS84")

library(sp)
library(rgdal)

xy <- data.frame(ID = 1:2, X = c(118, 119), Y = c(10, 50))
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example

res <- spTransform(, CRS("+proj=utm +zone=51 ellps=WGS84"))
res

df <- RASTER

####################### bufferss
df <- raster("D:/pace_uav/crop20180604.tif")

joe <- data.frame(m.pts$Id)
k = 2

for(j in 5:50){
# 20 meters!
x <- raster::extract(df, 
                     m.pts, 
                     buffer = j,
                     fun = mean,
                     df = TRUE)
  # add to data frame
  joe[, k] <- x[,2]
  k = k + 1
  print(k)
}
  
  joe$scene <- "x20180604"
  
 write.csv(joe, "x20180604_buffers.csv")
  # counter
 

