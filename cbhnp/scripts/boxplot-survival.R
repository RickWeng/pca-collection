# task: boxplot of % survival for white spruce and balsam fir
# author: ricky weng

## by species

# library
library(tidyverse) #dplyr, ggplot2

# data preparation
d2 <- read.csv("Seedling_Survival.csv", sep=",", na.strings = "na")
d2$Treatment <- str_replace(d2$Treatment, "Holes", "Hand")
d3 <- d2%>%
  select(Year, Treatment, X..survival, surviving.stems.ha)%>%
  filter(Year %in% c(2016, 2017), X..survival != "NA")

d3$Year <- factor(d3$Year, levels = c(2016, 2017), labels = c("White Spruce", "Balsam Fir"))
d3$Treatment <- factor(d3$Treatment, levels = c("Plow", "Hand", "Mow", "Till", "Brush blanket", "None", "Control"), labels = c("Plow", "Hand", "Mow", "Till", "Brush Blanket", "None", "Control"))

# plot
ggplot(d3, aes(x = Treatment, y = X..survival, group = Treatment, color = Year))+
  stat_boxplot(geom = 'errorbar', width = 0.25)+
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.alpha = 0.5)+
  stat_summary(fun.y = mean, geom = "point", shape = "+", size = 5)+
  scale_color_manual(values = c("#377EB8", "#E41A1C"))+
  facet_wrap(~Year, scales = "free_x")+
  theme_ipsum()+
  ylab("% Survival")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))
ggsave("boxplot-survial-species.png", width = 8, height = 5, dpi = 300)

## combined
d3$Treatment <- factor(d3$Treatment, levels = c("Plow", "Mow", "Hand", "Till", "Brush Blanket", "None", "Control"), labels = c("Plow", "Hand", "Mow", "Till", "Brush Blanket", "None", "Control"))
ggplot(d3, aes(x = Treatment, y = X..survival, group = Treatment))+
  stat_boxplot(geom = 'errorbar', width = 0.25)+
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.alpha = 0.5)+
  stat_summary(fun.y = mean, geom = "point", shape = "+", size=5)+
  scale_color_manual(values = c("#377EB8", "#E41A1C"))+
  theme_ipsum()+
  ylab("% Survival")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))
ggsave("boxplot-survial-combined.png", width = 8, height = 5, dpi = 300)
