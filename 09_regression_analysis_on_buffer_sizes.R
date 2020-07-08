require(tidyverse)
require(ggplot2)
require(gridExtra)
require(viridis)
require(RColorBrewer)

a <- read.csv("x20180426_buffers.csv")
b <- read.csv("x20180503_buffers.csv")
#c <- read.csv("x20180507_buffers.csv")
d <- read.csv("x20180521_buffers.csv")
e <- read.csv("x20180604_buffers.csv")

a$date <- as.Date("2018-04-26")
b$date <- as.Date("2018-05-03")
#c$date <- as.Date("2018-05-07")# remove bad rows
d$date <- as.Date("2018-05-21")
e$date <- as.Date("2018-06-04")

#df <- rbind(a, b, c, d, e)
df <- rbind(a, b, d, e)

df <- na.omit(df)
df <- na.omit(df)
# add matching id column in df
df$plot <- as.factor(paste("pace", df$m.pts.Id, sep = ""))


df <- df[, c(51, 49, 50, 3:48) ]

# make tidy
x <- gather(df, key = buffer.size, value = gcc, V2:V47)

x$buffer.size <- as.numeric(substr(x$buffer.size, 2, nchar(x$buffer.size)))

x$buffer.size <- x$buffer.size + 3

### bring in camera data
cam <- read.csv("./data/pace_lai_through_0604.csv")
cam$date <- as.Date(cam$date)
# the camera date has been changed to match the drone date closest
# camera date 2018-05-10 has been changed to 2018-05-07
# camera date 2018-06-06 has been changed to 2018-06-04

cam$X <- NULL

# make wide
bob <- merge(x, cam,  by = c("plot", "date"))

bob$buffer.size <- as.factor(bob$buffer.size)

bob %>%
  dplyr::select(plot, subplot, date, ndvi, lai, gap_fraction, buffer.size, gcc) -> y

x11()
ggplot(y, aes(x = gcc, y = ndvi, color = as.factor(date) ))+
  geom_point()


# Function for Root Mean Squared Error
RMSE <- function(error) { sqrt(mean(error^2)) }

##########################
##########################
###   BUFFER SIZE












###### NDVI

# center points only
y %>%
  filter(subplot == "c") %>%
  data.frame() -> y.center
  
y.center %>%
  group_by(buffer.size) %>%
  do(fit = lm(ndvi ~ gcc, data = .)) -> z

# get the coefficients by group in a tidy data_frame
carl <- broom::glance(z, fit)
dan <- data.frame(carl)

# get slopes
earl <- data.frame(broom::tidy(z, fit))

earl %>% 
  filter(term == "gcc") %>%
  data.frame() -> earl

# make RMSE data frame
emily<- data.frame(carl$buffer.size)

for(i in 1:nrow(z)){
emily$rmse[i] <- RMSE(z$fit[[i]]$residuals)
}

# Now the averages
y %>%
  dplyr::group_by(plot, date) %>%
  dplyr::summarize(ndvi = mean(ndvi), lai = mean(lai), gap_fraction = mean(gap_fraction), gcc = gcc, buffer.size = buffer.size) %>%
  data.frame() -> y.all

y.all <- distinct(y.all)


y.all %>%
  group_by(buffer.size) %>%
  do(fit = lm(ndvi ~ gcc, data = .)) -> z2

# get the coefficients by group in a tidy data_frame
carl2 <- broom::glance(z2, fit)
dan2 <- data.frame(carl2)

# get slopes
earl2 <- data.frame(broom::tidy(z2, fit))

earl2 %>% 
  filter(term == "gcc") %>%
  data.frame() -> earl2

# make RMSE data frame
emily2 <- data.frame(carl2$buffer.size)

for(i in 1:nrow(z)){
  
  emily2$rmse[i] <- RMSE(z2$fit[[i]]$residuals)
  
}

##### bootstrapping for R Squared!
# bootstrapping with 1000 replications
# data frame of ci's for R squared values

kim <- data.frame(buffers = range.buffers,
                  low.ci.CENTER = NA,
                  up.ci.CENTER = NA,
                  low.ci.COMP = NA,
                  up.ci.COMP = NA)
j = 1

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
results <- boot(data=jeff, statistic=rsq,
                R=1000, formula = ndvi ~ gcc)
results2 <- boot(data=jeff2, statistic=rsq,
                R=1000, formula = ndvi ~ gcc)

# get 95% confidence interval
cis <- boot.ci(results, type="bca")
cis2 <- boot.ci(results2, type="bca")

kim[j, 2] <- cis[[4]][4]
kim[j, 3] <- cis[[4]][5]
kim[j, 4] <- cis2[[4]][4]
kim[j, 5] <- cis2[[4]][5]

j = j + 1
print(j)
}

dan <- cbind(dan, kim)


############ 
############ 
kate <- data.frame(buffers = range.buffers,
                  low.ci.CENTER = NA,
                  up.ci.CENTER = NA,
                  low.ci.COMP = NA,
                  up.ci.COMP = NA)
j = 1

# 
# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit)[2])
}

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=bs,
                  R=1000, formula = ndvi ~ gcc)
  results2 <- boot(data=jeff2, statistic=bs,
                   R=1000, formula = ndvi ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kate[j, 2] <- cis[[4]][4]
  kate[j, 3] <- cis[[4]][5]
  kate[j, 4] <- cis2[[4]][4]
  kate[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

earl <- cbind(earl, kate)


###################################### BOOTSTRAPPING FOR RMSE

kelly <- data.frame(buffers = range.buffers,
                   low.ci.CENTER = NA,
                   up.ci.CENTER = NA,
                   low.ci.COMP = NA,
                   up.ci.COMP = NA)
j = 1

# 
# function to obtain regression weights
rmse.boot <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(RMSE(fit$residuals))
}

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=rmse.boot,
                  R=1000, formula = ndvi ~ gcc)
  results2 <- boot(data=jeff2, statistic=rmse.boot,
                   R=1000, formula = ndvi ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kelly[j, 2] <- cis[[4]][4]
  kelly[j, 3] <- cis[[4]][5]
  kelly[j, 4] <- cis2[[4]][4]
  kelly[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

emily <- cbind(emily, kelly)

####################################
# PLOTS
x11()
p.r.sq <- ggplot(dan, aes(x = as.numeric(buffer.size), y = adj.r.squared))+
  geom_errorbar(aes(ymin = dan$low.ci.CENTER, ymax = dan$up.ci.CENTER))+
  geom_errorbar(aes(ymin = dan$low.ci.COMP, ymax = dan$up.ci.COMP))+
  geom_point(shape = 21, fill = "#1B9E77", size = 3, alpha = 1)+
  geom_point(data = dan2, aes(x = as.numeric(buffer.size), y = adj.r.squared), 
             shape = 21, fill = "#093847", size = 3, alpha = 1)+
  theme_bw()+
  xlab("Buffer Size (m)")+
  ylab(expression(R^2))


x11()
p.rmse <- ggplot(emily, aes(x = as.numeric(carl.buffer.size), y = rmse))+
  geom_errorbar(aes(ymin = emily$low.ci.CENTER, ymax = emily$up.ci.CENTER))+
  geom_errorbar(aes(ymin = emily$low.ci.COMP, ymax = emily$up.ci.COMP))+
  geom_point(shape = 21, fill = "#1B9E77", size = 3, alpha = 1)+
  geom_point(data = emily2, aes(x = as.numeric(carl2.buffer.size), y = rmse), 
             shape = 21, fill = "#093847", size = 3, alpha = 1)+
  theme_bw()+
  xlab("Buffer Size (m)")+
  ylab("RMSE")

x11()
p.slope <- ggplot(earl, aes(x = as.numeric(buffer.size), y = estimate,))+
  geom_errorbar(aes(ymin = earl$low.ci.CENTER, ymax = earl$up.ci.CENTER))+
  geom_errorbar(aes(ymin = earl$low.ci.COMP, ymax = earl$up.ci.COMP))+
  geom_point(shape = 21, fill = "#1B9E77", size = 3, alpha = 1)+
  geom_point(data = earl2, aes(x = as.numeric(buffer.size), y = estimate), 
             shape = 21, fill = "#093847", size = 3, alpha = 1)+
  theme_bw()+
  xlab("Buffer Size (m)")+
  ylab("Slope")

x11(width = 8, height = 2.5)
grid.arrange(p.r.sq, p.rmse,  p.slope, nrow = 1, left = "NDVI")



############################ ARE THESE DIFFERENT

kevin <- dan[, c(1, 3)]
kevin[,3] <- dan2[, 3]
names(kevin)[2] <- "center"
names(kevin)[3] <- "composite"
t.test(kevin$center, kevin$composite, paired = TRUE)

# model
# x11()
# ggplot(x, aes(x = buffer.size, y = gcc, fill = buffer.size))+
#   geom_boxplot()+
#   facet_grid(.~date)
#   theme_bw()+
#   xlab("Buffer Size (m)")+
#   ylab(expression(R^2))








##############################################
############### LAI
# center points only
y %>%
  filter(subplot == "c") %>%
  data.frame() -> y.center

y.center %>%
  group_by(buffer.size) %>%
  do(fit = lm(lai ~ gcc, data = .)) -> z

# get the coefficients by group in a tidy data_frame
carl <- broom::glance(z, fit)
dan <- data.frame(carl)

# get slopes
earl <- data.frame(broom::tidy(z, fit))

earl %>% 
  filter(term == "gcc") %>%
  data.frame() -> earl

# make RMSE data frame
emily<- data.frame(carl$buffer.size)

for(i in 1:nrow(z)){
  
  emily$rmse[i] <- RMSE(z$fit[[i]]$residuals)
  
}

# Now the averages
y.all %>%
  group_by(buffer.size) %>%
  do(fit = lm(lai ~ gcc, data = .)) -> z2

# get the coefficients by group in a tidy data_frame
carl2 <- broom::glance(z2, fit)
dan2 <- data.frame(carl2)

# get slopes
earl2 <- data.frame(broom::tidy(z2, fit))

earl2 %>% 
  filter(term == "gcc") %>%
  data.frame() -> earl2

# make RMSE data frame
emily2 <- data.frame(carl2$buffer.size)

for(i in 1:nrow(z)){
  
  emily2$rmse[i] <- RMSE(z2$fit[[i]]$residuals)
  
}
##############################################
##### bootstrapping for R Squared!
# bootstrapping with 1000 replications
# data frame of ci's for R squared values

kim <- data.frame(buffers = range.buffers,
                  low.ci.CENTER = NA,
                  up.ci.CENTER = NA,
                  low.ci.COMP = NA,
                  up.ci.COMP = NA)
j = 1

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=rsq,
                  R=1000, formula = lai ~ gcc)
  results2 <- boot(data=jeff2, statistic=rsq,
                   R=1000, formula = lai ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kim[j, 2] <- cis[[4]][4]
  kim[j, 3] <- cis[[4]][5]
  kim[j, 4] <- cis2[[4]][4]
  kim[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

dan <- cbind(dan, kim)


############ 
############ 
kate <- data.frame(buffers = range.buffers,
                   low.ci.CENTER = NA,
                   up.ci.CENTER = NA,
                   low.ci.COMP = NA,
                   up.ci.COMP = NA)
j = 1

# 
# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit)[2])
}

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=bs,
                  R=1000, formula = lai ~ gcc)
  results2 <- boot(data=jeff2, statistic=bs,
                   R=1000, formula = lai ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kate[j, 2] <- cis[[4]][4]
  kate[j, 3] <- cis[[4]][5]
  kate[j, 4] <- cis2[[4]][4]
  kate[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

earl <- cbind(earl, kate)


###################################### BOOTSTRAPPING FOR RMSE

kelly <- data.frame(buffers = range.buffers,
                    low.ci.CENTER = NA,
                    up.ci.CENTER = NA,
                    low.ci.COMP = NA,
                    up.ci.COMP = NA)
j = 1

# 
# function to obtain regression weights
rmse.boot <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(RMSE(fit$residuals))
}

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=rmse.boot,
                  R=1000, formula = lai ~ gcc)
  results2 <- boot(data=jeff2, statistic=rmse.boot,
                   R=1000, formula = lai ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kelly[j, 2] <- cis[[4]][4]
  kelly[j, 3] <- cis[[4]][5]
  kelly[j, 4] <- cis2[[4]][4]
  kelly[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

emily <- cbind(emily, kelly)
#### sorting to find best?
head(dan2[order(dan2$AIC),])
head(dan2[order(-dan2$adj.r.squared),])

  x11()
  p.r.sq <- ggplot(dan, aes(x = as.numeric(buffer.size), y = adj.r.squared))+
    geom_errorbar(aes(ymin = dan$low.ci.CENTER, ymax = dan$up.ci.CENTER))+
    geom_errorbar(aes(ymin = dan$low.ci.COMP, ymax = dan$up.ci.COMP))+
    geom_point(shape = 21, fill = "#D95F02", size = 3, alpha = 1)+
    geom_point(data = dan2, aes(x = as.numeric(buffer.size), y = adj.r.squared), 
               shape = 21, fill = "#852A04", size = 3, alpha = 1)+
    theme_bw()+
    xlab("Buffer Size (m)")+
    ylab(expression(R^2))
  
  
  x11()
  p.rmse <- ggplot(emily, aes(x = as.numeric(carl.buffer.size), y = rmse))+
    geom_errorbar(aes(ymin = emily$low.ci.CENTER, ymax = emily$up.ci.CENTER))+
    geom_errorbar(aes(ymin = emily$low.ci.COMP, ymax = emily$up.ci.COMP))+
      geom_point(shape = 21, fill = "#D95F02", size = 3, alpha = 1)+
    geom_point(data = emily2, aes(x = as.numeric(carl2.buffer.size), y = rmse), 
               shape = 21, fill = "#852A04", size = 3, alpha = 1)+
    theme_bw()+
    xlab("Buffer Size (m)")+
    ylab("RMSE")
  
  x11()
  p.slope <- ggplot(earl, aes(x = as.numeric(buffer.size), y = estimate,))+
    geom_errorbar(aes(ymin = earl$low.ci.CENTER, ymax = earl$up.ci.CENTER))+
    geom_errorbar(aes(ymin = earl$low.ci.COMP, ymax = earl$up.ci.COMP))+
    geom_point(shape = 21, fill = "#D95F02", size = 3, alpha = 1)+
    geom_point(data = earl2, aes(x = as.numeric(buffer.size), y = estimate), 
               shape = 21, fill = "#852A04", size = 3, alpha = 1)+
    theme_bw()+
    xlab("Buffer Size (m)")+
    ylab("Slope")
  
  x11(width = 8, height = 2.5)
  grid.arrange(p.r.sq, p.rmse, p.slope, nrow = 1, left = "LAI")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############# ARE THEY DIFFERENT???
  ############# 
  kevin <- dan[, c(1, 3)]
  kevin[,3] <- dan2[, 3]
  names(kevin)[2] <- "center"
  names(kevin)[3] <- "composite"
  t.test(kevin$center, kevin$composite, paired = TRUE)
###############################################
  #### gap fraction
  
  # center points only
  y.center %>%
    group_by(buffer.size) %>%
    do(fit = lm(gap_fraction ~ gcc, data = .)) -> z
  
  # get the coefficients by group in a tidy data_frame
  carl <- broom::glance(z, fit)
  dan <- data.frame(carl)
  
  # get slopes
  earl <- data.frame(broom::tidy(z, fit))
  
  earl %>% 
    filter(term == "gcc") %>%
    data.frame() -> earl
  
  # make RMSE data frame
  emily<- data.frame(carl$buffer.size)
  
  for(i in 1:nrow(z)){
    
    emily$rmse[i] <- RMSE(z$fit[[i]]$residuals)
    
  }
  
  # Now the averages
  y.all %>%
    group_by(buffer.size) %>%
    do(fit = lm(gap_fraction ~ gcc, data = .)) -> z2
  
  # get the coefficients by group in a tidy data_frame
  carl2 <- broom::glance(z2, fit)
  dan2 <- data.frame(carl2)
  
  # get slopes
  earl2 <- data.frame(broom::tidy(z2, fit))
  
  earl2 %>% 
    filter(term == "gcc") %>%
    data.frame() -> earl2
  
  # make RMSE data frame
  emily2 <- data.frame(carl2$buffer.size)
  
  for(i in 1:nrow(z)){
    
    emily2$rmse[i] <- RMSE(z2$fit[[i]]$residuals)
    
  }
  
  }
##############################################
##### bootstrapping for R Squared!
# bootstrapping with 1000 replications
# data frame of ci's for R squared values

kim <- data.frame(buffers = range.buffers,
                  low.ci.CENTER = NA,
                  up.ci.CENTER = NA,
                  low.ci.COMP = NA,
                  up.ci.COMP = NA)
j = 1

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=rsq,
                  R=1000, formula = gap_fraction ~ gcc)
  results2 <- boot(data=jeff2, statistic=rsq,
                   R=1000, formula = gap_fraction ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kim[j, 2] <- cis[[4]][4]
  kim[j, 3] <- cis[[4]][5]
  kim[j, 4] <- cis2[[4]][4]
  kim[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

dan <- cbind(dan, kim)


############ 
############ 
kate <- data.frame(buffers = range.buffers,
                   low.ci.CENTER = NA,
                   up.ci.CENTER = NA,
                   low.ci.COMP = NA,
                   up.ci.COMP = NA)
j = 1

# 
# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit)[2])
}

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=bs,
                  R=1000, formula = gap_fraction ~ gcc)
  results2 <- boot(data=jeff2, statistic=bs,
                   R=1000, formula = gap_fraction ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kate[j, 2] <- cis[[4]][4]
  kate[j, 3] <- cis[[4]][5]
  kate[j, 4] <- cis2[[4]][4]
  kate[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

earl <- cbind(earl, kate)


###################################### BOOTSTRAPPING FOR RMSE

kelly <- data.frame(buffers = range.buffers,
                    low.ci.CENTER = NA,
                    up.ci.CENTER = NA,
                    low.ci.COMP = NA,
                    up.ci.COMP = NA)
j = 1

# 
# function to obtain regression weights
rmse.boot <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(RMSE(fit$residuals))
}

for(i in 5:50){
  
  # sort to buffer
  y.center %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff
  
  y.all %>%
    filter(buffer.size == i) %>%
    data.frame() -> jeff2
  
  
  results <- boot(data=jeff, statistic=rmse.boot,
                  R=1000, formula = gap_fraction ~ gcc)
  results2 <- boot(data=jeff2, statistic=rmse.boot,
                   R=1000, formula = gap_fraction ~ gcc)
  
  # get 95% confidence interval
  cis <- boot.ci(results, type="bca")
  cis2 <- boot.ci(results2, type="bca")
  
  kelly[j, 2] <- cis[[4]][4]
  kelly[j, 3] <- cis[[4]][5]
  kelly[j, 4] <- cis2[[4]][4]
  kelly[j, 5] <- cis2[[4]][5]
  
  j = j + 1
  print(j)
}

emily <- cbind(emily, kelly)

  x11()
  p.r.sq <- ggplot(dan, aes(x = as.numeric(buffer.size), y = adj.r.squared))+
    geom_errorbar(aes(ymin = dan$low.ci.CENTER, ymax = dan$up.ci.CENTER))+
    geom_errorbar(aes(ymin = dan$low.ci.COMP, ymax = dan$up.ci.COMP))+
    geom_point(shape = 21, fill = "#7570B3", size = 3, alpha = 1)+
    geom_point(data = dan2, aes(x = as.numeric(buffer.size), y = adj.r.squared), 
               shape = 21, fill = "#353A4D", size = 3, alpha = 1)+
    theme_bw()+
    xlab("Buffer Size (m)")+
    ylab(expression(R^2))
  
  
  x11()
  p.rmse <- ggplot(emily, aes(x = as.numeric(carl.buffer.size), y = rmse))+
    geom_errorbar(aes(ymin = emily$low.ci.CENTER, ymax = emily$up.ci.CENTER))+
    geom_errorbar(aes(ymin = emily$low.ci.COMP, ymax = emily$up.ci.COMP))+
    
    geom_point(shape = 21, fill = "#7570B3", size = 3, alpha = 1)+
    geom_point(data = emily2, aes(x = as.numeric(carl2.buffer.size), y = rmse), 
               shape = 21, fill = "#353A4D", size = 3, alpha = 1)+
    theme_bw()+
    xlab("Buffer Size (m)")+
    ylab("RMSE")
  
  x11()
  p.slope <- ggplot(earl, aes(x = as.numeric(buffer.size), y = estimate,))+
    geom_errorbar(aes(ymin = earl$low.ci.CENTER, ymax = earl$up.ci.CENTER))+
    geom_errorbar(aes(ymin = earl$low.ci.COMP, ymax = earl$up.ci.COMP))+
      geom_point(shape = 21, fill = "#7570B3", size = 3, alpha = 1)+
    geom_point(data = earl2, aes(x = as.numeric(buffer.size), y = estimate), 
               shape = 21, fill = "#353A4D", size = 3, alpha = 1)+
    theme_bw()+
    xlab("Buffer Size (m)")+
    ylab("Slope")
  
  x11(width = 8, height = 2.5)
  grid.arrange(p.r.sq, p.rmse, p.slope, nrow = 1, left = "Gap Fraction")
  
  
  
####################### additional plots and analysis

# we have identified 15 m radius as appropriate for LAI  
y.all %>%
  filter(buffer.size == 15) %>%
  data.frame() -> bob.15

lai.model <- lm(lai ~ gcc, data = bob.15)
x11()
plot(y = bob.15$lai, x = bob.15$gcc)

  
x11(width = 4, height = 4)
  p.lai <-  ggplot(bob.15, aes(x = gcc, y = lai))+
    geom_point(shape = 21, fill = "#D95F02", size = 3, alpha = 0.7)+
    theme_bw()+
    theme(legend.position = "none")+
    xlab(expression(GCC[UAV]~"(15 m)"))+
    ylab("Camera LAI")+
    geom_smooth(method = "lm", se = FALSE, color = "black")

##### Gap Fraction
y.all %>%
  filter(buffer.size == 20) %>%
  data.frame() -> bob.20

x11()
plot(y = bob.20$gap_fraction, x = bob.20$gcc)

gf.model <- nls(gap_fraction ~ a * exp(-b * gcc), data = bob.20, start = list(a = 1, b = 1))
  

  bob.20$predictedgf <- predict(gf.model)

  #####
  x11(width = 4, height = 4)
  p.gf <-  ggplot(bob.20, aes(x = gcc, y = gap_fraction))+
    geom_point(shape = 21, fill = "#7570B3", size = 3, alpha = 1)+
    #geom_line(aes(y = predictedgf), size = 1, color = "dark blue")+
    theme_bw()+
    theme(legend.position = "none")+
    xlab(expression(GCC[UAV]~"(20 m)"))+
    ylab("Gap Fraction")+
    geom_smooth(method="nls", 
                formula = (y ~ a * exp(-b * x)),
                method.args = list(start = c(a = 1, b = 1)),
                se = FALSE,
                color = "black") # this is an argument to stat_smooth and 
  # switches off drawing confidence intervals

  
  # make big panel
  x11(width = 4, height = 8)
  grid.arrange(p.lai, p.gf, nrow = 2)
  
  
  
#   ######################################
#   # making a by plot look at NDVI
#   
#   ###### NDVI
#   y %>%
#     group_by(buffer.size, plot) %>%
#     do(fit = lm(ndvi ~ gcc, data = .)) -> z
#   
#   # get the coefficients by group in a tidy data_frame
#   carl <- broom::glance(z, fit)
#   dan <- data.frame(carl)
#   
#   # get slopes
#   earl <- data.frame(broom::tidy(z, fit))
#   
#   earl %>% 
#     filter(term == "gcc") %>%
#     data.frame() -> earl
#   
#   # make RMSE data frame
#   emily<- data.frame(carl$buffer.size)
#   emily[2] <- carl$plot
#   names(emily)[names(emily) == "V2"] <- "plot"
#   for(i in 1:nrow(z)){
#     
#     emily$rmse[i] <- RMSE(z$fit[[i]]$residuals)
#     
#   }
#   
#   p.r.sq <- ggplot(dan, aes(x = as.numeric(buffer.size), 
#                             y = adj.r.squared, fill = plot))+
#     geom_point(shape = 21, size = 3, alpha = 1)+
#       theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab(expression(R^2))+ 
#     theme(legend.position = "right")
#   
#   
#   p.rmse <- ggplot(emily, aes(x = as.numeric(carl.buffer.size), 
#                               y = rmse, fill = plot))+
#     geom_point(shape = 21, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab("RMSE")+ 
#     theme(legend.position = "none")
#   
#   p.slope <- ggplot(earl, aes(x = as.numeric(buffer.size), 
#                               y = estimate, fill = plot))+
#     geom_point(shape = 21, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab("Slope")+ 
#     theme(legend.position = "none")
#   
#   x11(width = 8, height = 2.5)
#   grid.arrange(p.r.sq, p.rmse, p.slope, nrow = 1, left = "NDVI by Plot")
#   
#   
# #######################################
#   # making a by plot look at LAI
#   
#   ###### NDVI
#   y %>%
#     group_by(buffer.size, plot) %>%
#     do(fit = lm(lai ~ gcc, data = .)) -> z
#   
#   # get the coefficients by group in a tidy data_frame
#   carl <- broom::glance(z, fit)
#   dan <- data.frame(carl)
#   
#   # get slopes
#   earl <- data.frame(broom::tidy(z, fit))
#   
#   earl %>% 
#     filter(term == "gcc") %>%
#     data.frame() -> earl
#   
#   # make RMSE data frame
#   emily<- data.frame(carl$buffer.size)
#   emily[2] <- carl$plot
#   names(emily)[names(emily) == "V2"] <- "plot"
#   for(i in 1:nrow(z)){
#     
#     emily$rmse[i] <- RMSE(z$fit[[i]]$residuals)
#     
#   }
#   
#   p.r.sq <- ggplot(dan, aes(x = as.numeric(buffer.size), 
#                             y = adj.r.squared, fill = plot))+
#     geom_point(shape = 23, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab(expression(R^2))+ 
#     theme(legend.position = "none")
#   
#   
#   p.rmse <- ggplot(emily, aes(x = as.numeric(carl.buffer.size), 
#                               y = rmse, fill = plot))+
#     geom_point(shape = 23, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab("RMSE")+ 
#     theme(legend.position = "none")
#   
#   p.slope <- ggplot(earl, aes(x = as.numeric(buffer.size), 
#                               y = estimate, fill = plot))+
#     geom_point(shape = 23, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab("Slope")+ 
#     theme(legend.position = "none")
#   
#   x11(width = 8, height = 2.5)
#   grid.arrange(p.r.sq, p.rmse, p.slope, nrow = 1, left = "LAI by Plot")
#   
#   
#   #######################################
#   # making a by plot look at gap fraction
#   
#   ###### NDVI
#   y %>%
#     group_by(buffer.size, plot) %>%
#     do(fit = lm(gap_fraction ~ gcc, data = .)) -> z
#   
#   # get the coefficients by group in a tidy data_frame
#   carl <- broom::glance(z, fit)
#   dan <- data.frame(carl)
#   
#   # get slopes
#   earl <- data.frame(broom::tidy(z, fit))
#   
#   earl %>% 
#     filter(term == "gcc") %>%
#     data.frame() -> earl
#   
#   # make RMSE data frame
#   emily<- data.frame(carl$buffer.size)
#   emily[2] <- carl$plot
#   names(emily)[names(emily) == "V2"] <- "plot"
#   for(i in 1:nrow(z)){
#     
#     emily$rmse[i] <- RMSE(z$fit[[i]]$residuals)
#     
#   }
#   
#   p.r.sq <- ggplot(dan, aes(x = as.numeric(buffer.size), 
#                             y = adj.r.squared, fill = plot))+
#     geom_point(shape = 22, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab(expression(R^2))+ 
#     theme(legend.position = "none")
#   
#   
#   p.rmse <- ggplot(emily, aes(x = as.numeric(carl.buffer.size), 
#                               y = rmse, fill = plot))+
#     geom_point(shape = 22, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab("RMSE")+ 
#     theme(legend.position = "none")
#   
#   p.slope <- ggplot(earl, aes(x = as.numeric(buffer.size), 
#                               y = estimate, fill = plot))+
#     geom_point(shape = 22, size = 3, alpha = 1)+
#     theme_bw()+
#     scale_fill_brewer(palette = 3, type = "qual" )+
#     xlab("Buffer Size (m)")+
#     ylab("Slope")+ 
#     theme(legend.position = "none")
#   
#   x11(width = 8, height = 2.5)
#   grid.arrange(p.r.sq, p.rmse, p.slope, nrow = 1, left = "Gap Fraction by Plot")
# ##########
# 
#   
#   
#   
#   
#   
#   
#   
#   ###################### NDVI by date.
#   ###### NDVI by date
#   y %>%
#     group_by(buffer.size, date) %>%
#     do(fit = lm(ndvi ~ gcc, data = .)) -> zz
#   
#   # get the coefficients by group in a tidy data_frame
#   gary <- broom::glance(zz, fit)
#   jim <- broom::tidy(zz, fit)
#   henry <- data.frame(gary)
#   hank <- data.frame(jim)
#   
#   hank %>%
#     filter(term == "gcc") %>%
#     data.frame() -> jody
# 
# 
#   
# 
#  p.slope <- ggplot(jody, aes(x = as.numeric(buffer.size), y = estimate, color = as.factor(date)))+
#     geom_point(size = 2)+
#     theme_bw()+
#     xlab("Buffer Size (m)")+
#     ylab("Slope")+
#     scale_colour_brewer(
#       type = "qual", palette = 3)
#  
# 
#  p.pval <- ggplot(jody, aes(x = as.numeric(buffer.size), y = p.value, color = as.factor(date)))+
#    geom_point(size = 2)+
#    theme_bw()+
#    xlab("Buffer Size (m)")+
#    ylab("p value")+
#    scale_colour_brewer(
#      type = "qual", palette = 3)+
#    geom_hline(yintercept = 0.05, color = "black", size = 1.5)
# 
#  x11(width = 8, height = 3)
#  grid.arrange(p.slope, p.pval, nrow = 1)
#  
#  
#  
#  td <- broom::tidy(zz$fit, conf.int = TRUE)
#  
#  ggplot(td, aes(estimate, term, color = term)) +
#    geom_point() +
#    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
#    geom_vline()