#04 looking at lai to ndvi

pace <- read.csv("./data/pace_lai_through_0606.csv")


require(ggplot2)
require(ggridges)
require(viridis)
require(viridisLite)

#plot of lai
x11()
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
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(16), colour="black"),
        axis.title.x= element_text(size = (20), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("NDVI")+
  ylab("Date")
  
ggplot(pace, aes(x = ndvi, y = lai, color = plot))+
  geom_point(size = 5)+
  theme_classic()+
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(16), colour="black"),
        axis.title.x= element_text(size = (20), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("NDVI")+
  ylab("LAI")
  
summary(lm(lai ~ ndvi, data = pace))


### line plot by plot
x11()
ggplot(pace, aes(x = date, y = lai, group = plot, color = plot))+
  geom_line(size=1.5) + 
  geom_point(size=3, fill="white") +
  scale_color_viridis(discrete=TRUE)+
  theme_bw()+
  theme(legend.position="none")+
  scale_shape_manual(values=c(22,21))+
  theme(axis.text.x = element_text(size=(12), colour="black"))+
  theme(axis.title.y= element_text(size=16), 
        axis.text.y = element_text(size=(12), colour="black"),
        axis.title.x= element_text(size = (16), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("LAI")

### line plot by plot
x11(width = 6, height = 3)
ggplot(pace, aes(x = date, y = ndvi, group = plot, color = plot))+
  geom_line(size=1.5) + 
  geom_point(size=3, fill="white") +
  scale_shape_manual(values=c(22,21))+
  scale_color_viridis(discrete=TRUE)+
  theme_bw()+
  theme(legend.position="none")+
  scale_shape_manual(values=c(22,21))+
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(16), colour="black"),
        axis.title.x= element_text(size = (20), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("NDVI")

x11(width = 4, height = 4)
ggplot(pace, aes(x = ndvi, y = lai))+
  geom_point(size=3, fill="white") +
  scale_shape_manual(values=c(22,21))+
  scale_color_viridis(discrete=TRUE)+
  theme_bw()+
  theme(legend.position="none")+
  scale_shape_manual(values=c(22,21))+
  theme(axis.text.x = element_text(size=(16), colour="black"))+
  theme(axis.title.y= element_text(size=20), 
        axis.text.y = element_text(size=(16), colour="black"),
        axis.title.x= element_text(size = (20), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("NDVI")+
  ylab("LAI")

summary(lm(lai ~ ndvi, data = pace))

