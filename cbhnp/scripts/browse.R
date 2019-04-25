# task: browse condition for french mountain and north mountain - cape breton highlands national park 
# author: ricky weng

# distribution

# library
library(tidyverse) # data preparation 
library(FSA) # dunnTest
library(gmodels) # ci
library(hrbrthemes) # theme

# read data
d <- read.csv("Data_browse.csv", sep = ",")

d$Year <- factor(d$Year)

# Plot
ggplot(d, aes(x = X..AvailableTwigs..ungulate..30.cm., fill = Location))+
  geom_histogram(binwidth = 5)+
  facet_grid(cols = vars(Year), rows = vars(Location))+
  scale_fill_manual(values = c("#377EB8", "#E41A1C"))+
  theme_ipsum()+
  theme(legend.position = "none")+
  xlab("% Available Browse")+
  ylab("Count")
ggsave("histogram-browse.png", width = 8, height = 5, dpi = 300)


ggplot(d, aes(x = Year, y = X..AvailableTwigs..ungulate..30.cm., group = Year, color = Location))+
  stat_boxplot(geom = 'errorbar', width = 0.25)+
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.alpha = 0.5)+
  stat_summary(fun.y = mean, geom = "point", shape = "+", size = 5)+
  scale_color_manual(values = c("#377EB8", "#E41A1C"))+
  facet_wrap(~Location)+
  theme_ipsum()+
  theme(legend.position = "none")+
  ylab("% Available Browse")
ggsave("boxplot-browse.png", width = 8, height = 5, dpi = 300)

# Kruskal test
kruskal.test(X..AvailableTwigs..ungulate..30.cm.[Location == "French Mountain"]~Year[Location == "French Mountain"], data = d)
kruskal.test(X..AvailableTwigs..ungulate..30.cm.[Location == "North Mountain"]~Year[Location == "North Mountain"], data = d)

# Dunn's test
dunnTest(X..AvailableTwigs..ungulate..30.cm.[Location == "French Mountain"]~Year[Location == "French Mountain"], data = d)
dunnTest(X..AvailableTwigs..ungulate..30.cm.[Location == "North Mountain"]~Year[Location == "North Mountain"], data = d)

## Confidence Interval

# Calculate
d <- d%>%
  select(Year, Location, Species, X..AvailableTwigs..ungulate..30.cm.)%>%
  group_by(Year, Location)%>%
  summarize(mean = ci(X..AvailableTwigs..ungulate..30.cm., confidence = 0.95, na.rm = TRUE)[1],
            lower_CI = ci(X..AvailableTwigs..ungulate..30.cm., confidence=0.95, na.rm = TRUE)[2],
            upper_CI = ci(X..AvailableTwigs..ungulate..30.cm., confidence=0.95, na.rm = TRUE)[3])

# Plot
ggplot(d, aes(x = Year, y = mean, colour = Location, group = Location)) + 
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), colour = "#999999", width = .1) +
  geom_line() +
  geom_point(size = 3)+
  scale_color_manual(values = c("#377EB8", "#E41A1C"))+
  ylab("% Available Browse")+
  theme_ipsum()+
  theme(legend.title = element_blank())
ggsave("browse-CI.png",width = 8, height = 5, dpi = 300)
