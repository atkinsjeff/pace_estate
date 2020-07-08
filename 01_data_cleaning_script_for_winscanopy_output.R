# Data munging script

# this will need to be converted to a function at some point.
require(plyr)
require(dplyr)
require(tidyverse)

# 0426
x <- read.csv("./data/0426_pace_estate_ndvi_camera_analysis.CSV",
              skip = 5)

df <- x[,c(2,34, 60:61, 62:67, 75)]

names(df)[1] <- "filename"
names(df)[2] <- "ndvi"
names(df)[3] <- "gap_fraction"
names(df)[4] <- "openness"
names(df)[5] <- "lai1"
names(df)[6] <- "lai2"
names(df)[7] <- "lai3"
names(df)[8] <- "lai4"
names(df)[9] <- "lai5"
names(df)[10] <- "leaf_angle"
names(df)[11] <- "ci"


df$plot <- as.factor(substr(df$filename, 5, 9))
df$lai <- rowMeans(df[,5:9])
df$subplot <- as.factor(substr(df$filename, 10, 10))

pace0426 <- df
pace0426$date <- as.factor("2018-04-26")

# 0503
x <- read.csv("./data/0503_pace_estate_ndvi_camera_analysis.CSV",
              skip = 5)

df <- x[,c(2,34, 60:61, 62:67, 75)]

names(df)[1] <- "filename"
names(df)[2] <- "ndvi"
names(df)[3] <- "gap_fraction"
names(df)[4] <- "openness"
names(df)[5] <- "lai1"
names(df)[6] <- "lai2"
names(df)[7] <- "lai3"
names(df)[8] <- "lai4"
names(df)[9] <- "lai5"
names(df)[10] <- "leaf_angle"
names(df)[11] <- "ci"


df$plot <- as.factor(substr(df$filename, 5, 9))
df$lai <- rowMeans(df[,5:9])
df$subplot <- as.factor(substr(df$filename, 10, 10))

pace0503 <- df
pace0503$date <- as.factor("2018-05-03")

#######
x <- read.csv("./data/0510_pace_estate_ndvi_camera_analysis.csv",
              skip = 5)

df <- x[,c(2,34, 60:61, 62:67, 75)]

names(df)[1] <- "filename"
names(df)[2] <- "ndvi"
names(df)[3] <- "gap_fraction"
names(df)[4] <- "openness"
names(df)[5] <- "lai1"
names(df)[6] <- "lai2"
names(df)[7] <- "lai3"
names(df)[8] <- "lai4"
names(df)[9] <- "lai5"
names(df)[10] <- "leaf_angle"
names(df)[11] <- "ci"


df$plot <- as.factor(substr(df$filename, 5, 9))
df$lai <- rowMeans(df[,5:9])

pace0507<- df
pace0507$subplot <- as.factor("c")
pace0507$date <- as.factor("2018-05-07")

# 0521
x <- read.csv("./data/0521_pace_estate_ndvi_camera_analysis.CSV",
              skip = 5)

df <- x[,c(2,34, 60:61, 62:67, 75)]

names(df)[1] <- "filename"
names(df)[2] <- "ndvi"
names(df)[3] <- "gap_fraction"
names(df)[4] <- "openness"
names(df)[5] <- "lai1"
names(df)[6] <- "lai2"
names(df)[7] <- "lai3"
names(df)[8] <- "lai4"
names(df)[9] <- "lai5"
names(df)[10] <- "leaf_angle"
names(df)[11] <- "ci"


df$plot <- as.factor(substr(df$filename, 5, 9))
df$lai <- rowMeans(df[,5:9])
df$subplot <- as.factor(substr(df$filename, 10, 10))

pace0521 <- df
pace0521$date <- as.factor("2018-05-21")


# 0606
x <- read.csv("./data/0606_pace_estate_ndvi_camera_analysis.CSV",
               skip = 5)

df <- x[,c(2,34, 60:61, 62:67, 75)]

names(df)[1] <- "filename"
names(df)[2] <- "ndvi"
names(df)[3] <- "gap_fraction"
names(df)[4] <- "openness"
names(df)[5] <- "lai1"
names(df)[6] <- "lai2"
names(df)[7] <- "lai3"
names(df)[8] <- "lai4"
names(df)[9] <- "lai5"
names(df)[10] <- "leaf_angle"
names(df)[11] <- "ci"


df$plot <- as.factor(substr(df$filename, 9, 13))
df$lai <- rowMeans(df[,5:9])

pace0604 <- df
pace0604$subplot <- as.factor("c")
pace0604$date <- as.factor("2018-06-04")

# bring it together
pace <- rbind(pace0426, pace0503, pace0507, pace0521, pace0604)


#pace <- read.csv("./data/pace_lai_through_0521.csv")

z <- pace[, c(1:4, 10:15)]


write.csv(z, "./data/pace_lai_through_0604.csv")