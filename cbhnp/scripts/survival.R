# task: survival of white spruce and balsam fir using different planting methods
# author: ricky weng

# species
# libary
library(tidyverse) # dplyr, ggplot2 

# read data
d2 <- read.csv("Seedling_Survival.csv", sep = ",", na.strings = "na")
d2$Treatment <- str_replace(d2$Treatment, "Holes", "Hand")

# get mean values
d2 <- d2%>%
  select(Year, Treatment,X..survival, surviving.stems.ha)%>%
  filter(Year %in% c(2016, 2017), X..survival != "NA")%>%
  group_by(Year, Treatment)%>%
  summarize(survival_rate = mean(X..survival, na.rm = TRUE), stems_ha = mean(surviving.stems.ha, na.rm = TRUE))

# order
d2%>% 
  arrange(survival_rate)
d2$n <- as.numeric(factor(d2$Year))
d2$Treatment <- as.character(factor(d2$Treatment))
library(plyr)
d2 <- ddply(d2, .(Year,Treatment), transform, x = paste(c(rep(' ', n-1), Treatment), collapse = ''))

d2$x <- factor(d2$x, levels = d2[order(d2$survival_rate), 'x'])
d2$Year <- factor(d2$Year)
levels(d2$Year) <- c("White Spruce", "Balsam Fir")

# Plot
d2%>%
  ggplot(aes(x = x, y = survival_rate, color = Year))+
  geom_segment(aes(x = x , xend = x, y = 70, yend = survival_rate), color = "grey")+
  geom_point(size = 3)+
  facet_wrap(~Year, nrow = 2, scale = "free_y")+
  scale_color_manual(values = c("#377EB8", "#E41A1C"))+
  ylim(70, 100)+
  ylab("% Survival")+
  xlab("Treatment")+
  coord_flip()+
  theme_ipsum()+
  theme(axis.text.y = element_text(size = 10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10)
  )+
  theme(legend.title = element_blank(), plot.title = element_text(size = 16))
ggsave("survival-species.png", width = 8, height = 5, dpi = 300)

detach(package:plyr)

# combined species

# read data
d3 <- read.csv("Seedling_Survival.csv", sep=",", na.strings = c("na"))
d3$Treatment <- str_replace(d3$Treatment, "Holes", "Hand")

# get mean values
d3 <- d3%>%
  select(Year, Treatment,X..survival, surviving.stems.ha)%>%
  filter(Year %in% c(2016, 2017))%>%
  group_by(Treatment)%>%
  summarize(survival_rate = mean(X..survival, na.rm=TRUE), stems_ha = mean(surviving.stems.ha, na.rm = TRUE))%>%
  arrange(survival_rate)%>%
  mutate(category = factor(Treatment, Treatment))

# plot
d3 %>%
  ggplot(aes(x = category, y = survival_rate))+
  geom_segment(aes(x = category , xend = category, y = 75, yend = survival_rate), color = "grey")+
  geom_point(size = 3, color = "#377EB8")+
  ylab("% Survival")+
  xlab("Treatment")+
  ylim(75, 100)+
  coord_flip()+
  theme_ipsum()+
  theme(axis.text.y = element_text(size = 10))+
  theme(legend.title = element_blank())
ggsave("survival-combined.png", width = 8, height = 5, dpi = 300)
