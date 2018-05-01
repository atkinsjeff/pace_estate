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

ggplot(y, aes(x = gap_fraction, y = vwc.sd))+
  geom_point()