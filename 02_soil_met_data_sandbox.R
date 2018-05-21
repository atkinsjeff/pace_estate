#
#

require(ggplot2)

soil.met <- read.csv("./data/pace_soil_met_data.csv")

soil.met$date <-as.Date(soil.met$date)

# vwc mean by plot
soil.met %>%
  group_by(plot)%>%
  summarize(vwc.mean = mean(vwc), vwc.sd = sd(vwc)) -> vwc
  
# comparison
y <- merge(pace, vwc)
y <- merge(y, fpar)

ggplot(y, aes(x = gap_fraction, y = vwc.sd))+
  geom_point()

ggplot(y, aes(x = gap_fraction, y = fpar))+
  geom_point(size = 2)

ggplot(y, aes(x = lai, y = lai.lp80))+
  geom_point(size = 3)+
  geom_text(aes(label=y$plot),hjust=0, vjust=0)+
  xlab("CAMERA LAI")+
  ylab("LP-80 LAI")