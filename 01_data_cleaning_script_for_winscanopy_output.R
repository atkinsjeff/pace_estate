# Data munging script

# this will need to be converted to a function at some point.
require(plyr)
require(dplyr)
require(tidyverse)

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

df %>% group_by(plot) %>%
  summarize(lai= mean(lai),
            ndvi = mean(ndvi),
            gap_fraction = mean(gap_fraction),
            theta = mean(leaf_angle),
            ci = mean(ci)) -> y

pace0606 <- data.frame(y)
pace0606$date <- as.factor("2018-06-06")
pace0606$data <- as.fa

pace <- read.csv("./data/pace_lai_through_0521.csv")

z <- pace[,2:8]


b <- rbind(z, pace0606)

write.csv(b, "./data/pace_lai_through_0606.csv")