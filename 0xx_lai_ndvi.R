#04 looking at lai to ndvi

pace <- read.csv("./data/pace_lai_through_0606.csv")


require(ggplot2)
require(ggridges)
require(viridis)
require(viridisLite)

#plot of lai
ggplot(pace, aes(x = lai, y = date, group = date, fill = as.factor(date)))+
  geom_density_ridges(scale = 3, size = 0.25, alpha = 0.6, color = "white") +
  theme_ridges()+
  scale_x_continuous(limits=c(0, 6), expand = c(0.01, 0))+
  theme(legend.position="none")+
  xlab("LAI")+
  ylab("Date")

#plot of lai
ggplot(pace, aes(x = ndvi, y = date, group = date, fill = date))+
  geom_density_ridges(scale = 3, size = 0.25, alpha = 0.6, color = "white") +
  theme_ridges()+
  scale_x_continuous(limits=c(-0.25, 1), expand = c(0.01, 0))+
  theme(legend.position="none")+
  xlab("NDVI")+
  ylab("Date")
  
ggplot(pace, aes(x = ndvi, y = lai, color = plot))+
  geom_point(size = 5)+
  theme_classic()+
  xlab("NDVI")+
  ylab("LAI")
  
summary(lm(lai ~ ndvi, data = pace))


### line plot by plot
x11()
ggplot(pace, aes(x = date, y = lai, group = plot, color = plot))+
  geom_line(size=1.5) + 
  geom_point(size=3, fill="white") +
  scale_color_viridis(discrete=TRUE)+
  theme_classic()+
  scale_shape_manual(values=c(22,21))+
  xlab("Date")+
  ylab("LAI")

### line plot by plot
x11()
ggplot(pace, aes(x = date, y = ndvi, group = plot, color = plot))+
  geom_line(size=1.5) + 
  geom_point(size=3, fill="white") +
  scale_shape_manual(values=c(22,21))+
  scale_color_viridis(discrete=TRUE)+
  theme_classic()+
  xlab("Date")+
  ylab("NDVI")