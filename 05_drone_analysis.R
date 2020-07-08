require(ggplot2)
require(viridis)
require(tidyverse)
require(gridExtra)
require(randomForest)
require(bcp)
# reading in data
df <- read.csv("./data/pace_drone.csv")

df$date <- as.Date(df$date)

df %>%
  filter(!date == "2018-05-07" ) %>%
  data.frame() -> df

# # df %>%
# #   filter(mean < 254) %>%
# #   data.frame() -> df
# #
# x11(width = 6, height = 4)
# ggplot(df, aes(x = as.factor(date), y = mean, fill = as.factor(buffer)))+
#   geom_boxplot()+
#   theme_bw()+
#   scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
#                     name="Plot Buffer Size [m]",
#                     breaks=c(5, 10, 20),
#                     labels=c(5, 10, 20))+
#   xlab("")+
#   ylab(expression(GCC[UAV]))+
#   theme(legend.position="bottom")

# bringin in the camera data
cam <- read.csv("./data/pace_lai_through_0604.csv")
cam$date <- as.Date(cam$date)
# the camera date has been changed to match the drone date closest
# camera date 2018-05-10 has been changed to 2018-05-07
# # camera date 2018-06-06 has been changed to 2018-06-04
# cam$date[cam$date == "2018-05-10"] <- "2018-05-07"
# cam$date[cam$date == "2018-06-06"] <- "2018-06-04"


# the camera date has been changed to match the drone date closest
# camera date 2018-05-10 has been changed to 2018-05-07
# camera date 2018-06-06 has been changed to 2018-06-04
#cam$date[cam$date == "2018-05-10"] <- "2018-05-07"
#cam$date[cam$date == "2018-06-06"] <- "2018-06-04"

# add matching id column in df
df$plot <- as.factor(paste("pace", df$id, sep = ""))

# change column names for df
colnames(df) <- c("X", "id", "date", "buffer", "mean.green", "sd.green", "plot")



# sort down
df2 <- df[c("plot", "date", "buffer", "mean.green", "sd.green")]

# break apart
df2 %>%
  pivot_wider(names_from = buffer,
              values_from = c(mean.green, sd.green)) %>%
  data.frame() -> df3

# adjust greennness values to 0 to 1




# make wide
bill <- merge(cam, df3,  by = c("plot", "date"))

# sort to center only
bill %>%
  filter(subplot == "c") %>%
  data.frame() -> bob
######

# BOB STATS

bob %>%
  dplyr::group_by(date) %>%
  dplyr::summarize_at(vars(lai:mean.green_20), mean, na.rm = TRUE) %>%
  data.frame() -> x

bob %>%
  dplyr::group_by(date) %>%
  dplyr::summarize_at(vars(lai:mean.green_20), sd, na.rm = TRUE) %>%
  data.frame() -> y

### STATS

# 5

lai.fit <- lm(lai ~ mean.green_10, data = bob)
summary(lm(mean.green_5 ~ lai, data = bob))
summary(lm(lai ~ mean.green_10, data = bob))
summary(lm(mean.green_20 ~ lai, data = bob))

bob$doy <- as.integer(strftime(bob$date, format = "%j"))

bob %>%
  filter(doy < 128) -> carl

summary(lm(ndvi ~ doy, data = carl))
summary(lm(mean.green_20 ~ doy, data = carl))

AIC(lm(mean.green_5 ~ lai, data = bob))
AIC(lm(mean.green_10 ~ lai, data = bob))
AIC(lm(mean.green_20 ~ lai, data = bob))

lm.5 <- lm(mean.green_5 ~ ndvi, data = bob)
summary(lm(mean.green_10 ~ ndvi, data = bob))
summary(lm(mean.green_20 ~ ndvi, data = bob))

summary(lm(mean.green_5 ~ gap_fraction, data = bob))
summary(lm(mean.green_10 ~ gap_fraction, data = bob))
summary(lm(mean.green_20 ~ gap_fraction, data = bob))

AIC(lm(mean.green_5 ~ gap_fraction, data = bob))
AIC(lm(mean.green_10 ~ gap_fraction, data = bob))
AIC(lm(mean.green_20 ~ gap_fraction, data = bob))

rsq_label.5 <- paste('R^2 == ', round(summary(lm.5)$r.squared, 2))
# big time plots
x11(width = 4, height = 4)
p.5 <- ggplot(bob, aes(x = ndvi, y = mean.green_5))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(.9))+
  geom_point(shape = 21, fill = "dark blue", size = 3, alpha = 1)+
  theme_bw()+
  ylab(expression(GCC[UAV]~"(5 m)"))+
  xlab("NDVI")+
  theme(legend.position = "none")+
  ylim(c(0.3, 0.6))+
  geom_smooth(method = "lm", se = FALSE)+
  annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 2, label = rsq_label.5, parse = TRUE, size = 6)

lm.10 <- lm(mean.green_10 ~ ndvi, data = bob)
rsq_label.10 <- paste('R^2 == ', round(summary(lm.10)$r.squared, 2))

x11(width = 4, height = 4)
p.10 <- ggplot(bob, aes(x = ndvi, y = mean.green_10))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(.9))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  ylab(expression(GCC[UAV]~"(10 m)"))+
  xlab("NDVI")+
  theme(legend.position = "none")+
  ylim(c(0.3, 0.6))+
  geom_smooth(method = "lm", se = FALSE)+
  annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 2, label = rsq_label.10, parse = TRUE, size = 6)

lm.20 <- lm(ndvi ~ mean.green_20 + plot, data = bob)
rsq_label.20 <- paste('R^2 == ', round(summary(lm.20)$r.squared, 2))

x11(width = 4, height = 4)
p.20 <- ggplot(bob, aes(x = ndvi, y = mean.green_20))+
  geom_errorbar(aes(ymin = mean.green_20 - sd.green_20, ymax = mean.green_20 + sd.green_20),
                 position=position_dodge(.9))+
  geom_point(shape = 21, fill = "dark orange", size = 3, alpha = 1)+
  theme_bw()+
  ylab(expression(GCC[UAV]~"(20 m)"))+
  xlab("NDVI")+
  theme(legend.position = "none")+
  ylim(c(0.3, 0.6))+
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 2, label = rsq_label.20, parse = TRUE, size = 6)

#### ranking
AIC(lm.5)
AIC(lm.10)
AIC(lm.20)

# make big panel
x11()
grid.arrange(p.5, p.10, p.20, nrow = 1,
             top = "GCC (UAV) and Camera NDVI at 5, 10, 20 m radius buffers")
# 
# ################ NDVI Plots
# p5 <- ggplot(bob, aes(x = ndvi, y = mean.green_5))+
#   geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
#                 position=position_dodge(.9))+
#   geom_point(shape = 21, fill = "dark blue", size = 3, alpha = 1)+
#   theme_bw()+
#   ylab("UAV Greenness (5 m)")+
#   geom_abline(slope = 1, intercept = 0)+
#   ylim(c(0,.9))+
#   xlim(c(0,.9))+
#   theme(legend.position = "none",
#         panel.grid = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank())+
#   facet_grid(.~date)
# 
# p10 <- ggplot(bob, aes(x = ndvi, y = mean.green_10))+
#   geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
#                 position=position_dodge(2))+
#   geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
#   theme_bw()+
#   theme(legend.position = "none",
#         panel.grid = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank())+
#   ylab("UAV Greenness (10 m)")+
#   xlab("")+
#   geom_abline(slope = 1, intercept = 0)+
#   ylim(c(0,.9))+
#   xlim(c(0,.9))+
#   facet_grid(.~date)+theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )
# 
# 
# p20 <- ggplot(bob, aes(x = ndvi, y = mean.green_5))+
#   geom_errorbar(aes(ymin = mean.green_20 - sd.green_20, ymax = mean.green_20 + sd.green_20),
#                 position=position_dodge(.9))+
#   geom_point(shape = 21, fill = "dark orange", size = 3, alpha = 1)+
#   theme_bw()+
#   theme(legend.position = "none")+
#   ylab("UAV Greenness (20 m)")+
#   xlab("Camera NDVI")+
#   geom_abline(slope = 1, intercept = 0)+
#   ylim(c(0,.9))+
#   xlim(c(0,.9))+
#   facet_grid(.~date)+theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank())
# 
# # make big panel
# x11()
# grid.arrange(p5, p10, p20, nrow = 3,
#              top = "UAV Greenness and Camera NDVI at 5, 10, 20 m radius buffers")
# mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
#                 position=position_dodge(.9))+
#   geom_point(shape = 21, fill = "dark orange", size = 3, alpha = 1)+
#   theme_bw()+
#   ylab("UAV Greenness (5 m)")+
#   xlab("Camera Gap Fraction")+
#   theme(legend.position = "none")+
#   geom_smooth(method = "lm", SE = FALSE)
# 
# lm.lai.gf <- lm(mean.green_10 ~ gap_fraction, data = bob)
# ################################
# ################################



#### LAIplots
p.lai5 <- ggplot(bob, aes(x = lai, y = mean.green_5))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(.9))+
  geom_point(shape = 21, fill = "dark blue", size = 3, alpha = 1)+
  theme_bw()+
  ylab("UAV Greenness (5 m)")+
  geom_abline(slope = 0.125, intercept = 0)+
  ylim(c(0,.9))+
  xlim(c(0, 5.5))+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  facet_grid(.~date)

p.lai10 <- ggplot(bob, aes(x = lai, y = mean.green_10))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(2))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("UAV Greenness (10 m)")+
  xlab("")+
  geom_abline(slope = 0.125, intercept = 0)+
  ylim(c(0,.9))+
  xlim(c(0, 5.5))+
  facet_grid(.~date)+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )




  nls(lm(mean.green_20 ~ lai, data = bob))
  model <- nls(mean.green_20 ~ a/(1 + exp(-b * (lai - c))), data = bob, 
               start=list(a= -1, b=.5,c = 1))

  bob$predicted <- predict(model)
  x11(width = 4, height = 4)
p.lai <-  ggplot(bob, aes(x = lai, y = mean.green_20))+
  geom_errorbar(aes(ymin = mean.green_20 - sd.green_20, ymax = mean.green_20 + sd.green_20),
                position=position_dodge(.9))+
  geom_point(shape = 21, fill = "dark orange", size = 3, alpha = 1)+
    geom_line(aes(y = predicted), size = 1, color = "dark blue")+
  theme_bw()+
  theme(legend.position = "none")+
  ylab(expression(GCC[UAV]~"(20 m)"))+
  xlab("Camera LAI")
  #######3
  gf.model <- nls(mean.green_20 ~ a + (b - c) * exp(-b * gap_fraction), data = bob, start = list(a = 0.5, b = 0.1, c = 1))
  
  nls(y ~ yf + (y0 - yf) * exp(-alpha * t), 
      data = sensor1,
      start = list(y0 = 54, yf = 25, alpha = 1))
  
  bob$predictedgf <- predict(gf.model)
  
gf.log <- lm(log(mean.green_20) ~ gap_fraction, data = bob)

bob$predictedgflog <- predict(gf.log)
  #####
  x11(width = 4, height = 4)
p.gf <-  ggplot(bob, aes(x = gap_fraction, y = mean.green_20))+
    geom_errorbar(aes(ymin = mean.green_20 - sd.green_20, ymax = mean.green_20 + sd.green_20),
                  position=position_dodge(.9))+
    geom_point(shape = 21, fill = "dark red", size = 3, alpha = 1)+
    geom_line(aes(y = predictedgf), size = 1, color = "dark blue")+
    theme_bw()+
    theme(legend.position = "none")+
    ylab(expression(GCC[UAV]~"(20 m)"))+
    xlab("Camera Gap Fraction")
  +
    stat_smooth(method="nls", 
                formula = y ~ 0.4633/(1/ exp(-0.7153 * (x + 0.4473))),
                se=FALSE) # this is an argument to stat_smooth and 
  # switches off drawing confidence intervals
  
  x11(width = 4, height = 4)
  ggplot(bob, aes(x = gap_fraction, y = mean.green_20))+
    geom_point(shape = 21, fill = "dark orange", size = 3, alpha = 1)+
    theme_bw()+
    theme(legend.position = "none")+
    ylab("UAV Greenness (20 m)")+
    xlab("Camera Gap Fraction")+
    geom_smooth(method = "loess", se = FALSE)
+
  ylim(c(0,.9))+
  xlim(c(0, 5.5))+
  facet_grid(.~date)+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()  )

# make big panel
x11()
grid.arrange(p.lai, p.gf, nrow = 2)


##### RANDOM FOREST TO PICK THE BEST

set.seed(420)
uav.rf <- randomForest(mean.green_20 ~ lai + ndvi + gap_fraction + theta + ci,
                       data = bob,
                       proximity = TRUE,
                       importance = TRUE)

print(uav.rf)
varImpPlot(uav.rf)

set.seed(420)
uav.rf.lite <- randomForest(mean.green_10 ~ lai + ndvi + gap_fraction,
                       data = bob,
                       proximity = TRUE,
                       importance = TRUE)

print(uav.rf.lite)
varImpPlot(uav.rf.lite)
## Look at variable importance:
round(importance(uav.rf), 2)
## Do MDS on 1 - proximity:
uav.mds <- cmdscale(1 - uav.rf$proximity, eig=TRUE)

x11()
op <- par(pty="s")

pairs(cbind(bob[,4:8], uav.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue", "black", "gray")[as.numeric(bob$mean.green_10)],
      main="UAV Greenness: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(uav.mds$GOF)



  ##### let's look at all of em over time
p.ndvi10 <- ggplot(bob, aes(x = ndvi, y = mean.green_10))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(2))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("UAV Greenness (10 m)")+
  xlab("NDVI")+
  geom_abline(slope = 1, intercept = 0)+
  ylim(c(0,.9))+
  xlim(c(0,.9))+
  facet_grid(.~date)+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
p.lai10 <- ggplot(bob, aes(x = lai, y = mean.green_10))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(2))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("UAV Greenness (10 m)")+
  xlab("Leaf Area Index (LAI)")+
  geom_abline(slope = 0.125, intercept = 0)+
  ylim(c(0,.9))+
  xlim(c(0, 5.5))+
  facet_grid(.~date)+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

p.ci10 <- ggplot(bob, aes(x = ci, y = mean.green_10))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(2))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("UAV Greenness (10 m)")+
  xlab("Clumping Index")+
  geom_abline(slope = 1, intercept = 0)+
  ylim(c(0,.9))+
  facet_grid(.~date)+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# p.theta10 <- ggplot(bob, aes(x = theta, y = mean.green_10))+
#   geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
#                 position=position_dodge(2))+
#   geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
#   theme_bw()+
#   theme(legend.position = "none")+
#   ylab("UAV Greenness (10 m)")+
#   xlab("Mean Leaf Angle")+
#   geom_abline(slope = 1, intercept = 0)+
#   ylim(c(0,.9))+
#   facet_grid(.~date)+theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )

p.gap10 <- ggplot(bob, aes(x = (gap_fraction/100), y = mean.green_10))+
  geom_errorbar(aes(ymin = mean.green_10 - sd.green_10, ymax = mean.green_10 + sd.green_10),
                position=position_dodge(2))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("UAV Greenness (10 m)")+
  xlab("Gap Fraction")+
  geom_abline(slope = 1, intercept = 0)+
  ylim(c(0,.9))+
  facet_grid(.~date)+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

x11()
grid.arrange(p.ndvi10, p.lai10, p.gap10, p.ci10, nrow = 4,
             top = "UAV Greenness at 10 m plot radius")

# when does gap fraction stablize
x11()
ggplot(bob, aes(x = date, y =  (gap_fraction/100)))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Gap Fraction")+
  ylim(c(0,.9))

x11()
ggplot(bob, aes(x = date, y =  lai))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("LAI")

x11()
ggplot(bob, aes(x = date, y =  ci))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Clumping Index")

x11()
ggplot(bob, aes(x = date, y =  ndvi))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("NDVI")


x11()
ggplot(bob, aes(x = date, y =  mean.green_10))+
  geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("UAV Greenness")+
  geom_smooth(method = "loess")

# ggplot(bob, aes(x = date, y =  mean.green_10))+
#   geom_point(shape = 21, fill = "dark green", size = 3, alpha = 1)+
#   theme_bw()+
#   theme(legend.position = "none")+
#   ylab("UAV Greeness")+
#   geom_smooth(method = "loess")+
#   facet_grid(.~plot)

#### segmented analysis
require(segmented)

# bob <- cam
bob$jd <- as.integer(format(bob$date, "%j"))

par(mar=c(5.1,4.1,4.1,2.1))
# greenness
x <- bob$jd
y <- bob$mean.green_20
lm.green <- lm(y ~ x)

seg.green <- segmented(lm.green, seg.Z = ~x, psi = 125)
bp <-  round(seg.green$psi[2], 2)
bp.se <- round(seg.green$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = expression(paste("GCC"["UAV"])),
     xlab ="Julian Day",
     bg = "#E7298A",
     pch = 21,
     cex = 1.25)
plot(seg.green, add=T)
text(x = 140, y = 0.37, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 0.35, label = "Estimated Breakpoint w/ SE")

# NDVI
x <- bob$jd
y <- bob$ndvi
lm.ndvi <- lm(y ~ x)

seg.ndvi <- segmented(lm.ndvi, seg.Z = ~x, psi = 125)
bp <-  round(seg.ndvi$psi[2], 2)
bp.se <- round(seg.ndvi$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "Camera NDVI",
     xlab ="Julian Day",
     bg = "#1B9E77",
     pch = 21,
     cex = 1.25)
plot(seg.ndvi, add=T)
text(x = 140, y = 0.33, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 0.42, label = "Estimated Breakpoint w/ SE")

# LAI
x <- bob$jd
y <- bob$lai
lm.lai <- lm(y ~ x)

seg.lai <- segmented(lm.lai, seg.Z = ~x, psi = 125)
bp <-  round(seg.lai$psi[2], 2)
bp.se <- round(seg.lai$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "Camera LAI",
     xlab ="Julian Day",
     bg = "#D95F02",
     pch = 21,
     cex = 1.25)
plot(seg.lai, add=T)
text(x = 140, y = 1.7, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 2.1, label = "Estimated Breakpoint w/ SE")

# GAP FRACTION
x <- bob$jd
y <- bob$gap_fraction
lm.gap_fraction <- lm(y ~ x)

seg.gap_fraction <- segmented(lm.gap_fraction, seg.Z = ~x, psi = 125)
bp <-  round(seg.gap_fraction$psi[2], 2)
bp.se <- round(seg.gap_fraction$psi[3], 2)

x11(width = 4, height = 4)
plot(x,y,
     ylab = "Camera Gap Fraction",
     xlab ="Julian Day",
     bg = "#7570B3",
     pch = 21,
     cex = 1.25)
plot(seg.gap_fraction, add=T)
text(x = 140, y = 32, label = paste(bp, bp.se, sep = "+/-"))
text(x = 140, y = 37, label = "Estimated Breakpoint w/ SE")









