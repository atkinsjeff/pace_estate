require(pwr)

### bring in camera data
cam <- read.csv("./data/pace_lai_through_0604.csv")
cam$date <- as.Date(cam$date)
# the camera date has been changed to match the drone date closest
# camera date 2018-05-10 has been changed to 2018-05-07
# camera date 2018-06-06 has been changed to 2018-06-04

# remove shitty column
cam$X <- NULL

# cam sort to center
cam %>%
  filter(subplot == "c") -> cam.center

############


require(raster)
require(rgdal)
require(viridis)
require(tidyverse)
require(pwr)
require(boot)
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

# filter camera
cam.center %>%
  dplyr::filter(date == "2018-04-26") %>% 
  dplyr::select(ndvi) -> ndvi.0426

cam.center %>%
  dplyr::filter(date == "2018-05-03") %>% 
  dplyr::select(ndvi) -> ndvi.0503

cam.center %>%
  dplyr::filter(date == "2018-05-07") %>% 
  dplyr::select(ndvi) -> ndvi.0507

cam.center %>%
  dplyr::filter(date == "2018-05-21") %>% 
  dplyr::select(ndvi) -> ndvi.0521

cam.center %>%
  dplyr::filter(date == "2018-06-04") %>% 
  dplyr::select(ndvi) -> ndvi.0604


#model the population using 2700 uniformly drawn integers
samp1 <- list()
samp2 <- list()
samp3 <- list()
samp4 <- list()
samp5 <- list()

samples <- list()
mean.ndvi0426 <- numeric()
mean.ndvi0503 <- numeric()
mean.ndvi0507 <- numeric()
mean.ndvi0521 <- numeric()
mean.ndvi0604 <- numeric()
conf <- list()

calc_mean <- function(x) {
  mean(x)
}

#Calculate SEM for samples ranging in size from 1 to 100 observations
for (i in 1:100) {
  samp1[[i]] <- sample(ndvi.0426, i, replace = TRUE)

  samp2[[i]] <- sample(ndvi.0503, i, replace = TRUE)
  samp3[[i]] <- sample(ndvi.0507, i, replace = TRUE)

  samp4[[i]] <- sample(ndvi.0521, i, replace = TRUE)

  samp5[[i]] <- sample(ndvi.0604, i, replace = TRUE)
}

for (i in 1:100) {
  mean.ndvi0426[[i]] <- calc_mean(samp1[[i]])
  
  mean.ndvi0503[[i]] <- calc_mean(samp2[[i]])
  
  mean.ndvi0507[[i]] <- calc_mean(samp3[[i]])
  
  mean.ndvi0521[[i]] <- calc_mean(samp4[[i]])
  
  mean.ndvi0604[[i]] <- calc_mean(samp5[[i]])
}
#creating a DF and adding a col for the sample size
mean.ndvi_df <- data.frame(mean.ndvi0426, mean.ndvi0503, mean.ndvi0507, mean.ndvi0521, mean.ndvi0604)
mean.ndvi_df["no_plots"]<-c(1:100)

#plot the SEM
#
#
#
#
set.seed(2000)

#model the population using 2700 uniformly drawn integers
population <- ceiling(runif(2700, 150, 250))
samp <- list()
samples <- list()
sem <- numeric()
conf <- list()

#function to compute the SEM
calc_sem <- function(x) {
  sd(x)/sqrt(length(x))
}

#Calculate SEM for samples ranging in size from 1 to 100 observations
for (i in 1:100) {
  samp[[i]]<-sample(population, i)
  sem[[i]]<-calc_sem(samp[[i]])
}

#creating a DF and adding a col for the sample size
sem_df<-data.frame(sem)
sem_df["size"]<-c(1:100)

#plot the SEM
ggplot(sem_df,aes(x = size, y = sem))+geom_point()+xlab("Sample Size")+ylab("Standard Error of the Mean")
ggplot(mean.ndvi_df, aes(x = no_plots, y = means))+
  geom_point()+
  xlab("Sample Size")+
  ylab("Standard Error of the Mean")