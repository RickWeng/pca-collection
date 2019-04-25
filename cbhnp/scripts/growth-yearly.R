# task: measurement of white spruce and balsam fir using different planting methods since 2016
# author: ricky weng

library(tidyverse) # dplyr, ggplot2
library(viridis) # color
library(hrbrthemes) # theme

# set workspace
setwd("~/CBHNP/analysis")

# read data
d1 <- read.csv("Seedling_Data.csv", sep = ",", na.strings = c("M", "missing", "null", "NULL", "n/a", "", "na", " "))
d2 <- read.csv("Seedling_Survival.csv", sep = ",")

# data preparation
d1$Planted. <- str_replace(d1$Planted., "y`|Y |y ", "Y")
d1$Species <- str_replace(d1$Species, "WS ", "WS")

d2 <- d2%>%
  select(Plot, Treatment)

# white spruce - add baseline
d1 <- d1%>%
  select(Day : Planted.)%>%
  filter(Planted. == "Y", Height..cm. != "D", Species == "WS")%>%
  mutate(Category = ifelse(Month %in% c(4, 5) & Year == 2016, "Baseline",
                         ifelse(Month == 10 & Year == 2016, "2016",
                                ifelse(Month %in% c(9, 10) & Year == 2017, "2017",
                                       ifelse(Month %in% c(10, 11) & Year == 2018, "2018", NA)))))

# join treatment to data
new_d <- left_join(d1, d2)

new_d$Height..cm. <- as.numeric(as.character(new_d$Height..cm.))
new_d$Basal.Diameter..mm. <- as.numeric(as.character(new_d$Basal.Diameter..mm.))

# get mean values
new_d <- new_d%>%
  group_by(Category, Treatment)%>%
  summarize(Mean_height = mean(Height..cm., na.rm = TRUE),
            Mean_basal = mean(Basal.Diameter..mm., na.rm = TRUE),
            Mean_twigs = mean(X.Twigs, na.rm = TRUE))

# wide to long format
new_d <- gather(new_d, Variables, Value, Mean_height : Mean_twigs, factor_key = TRUE)
new_d2 <- new_d%>%
  filter(Category %in% c("2018", "Baseline"))%>%
  group_by(Treatment, Variables)%>%
  summarize(Difference = Value[Category == "2018"] - Value[Category == "Baseline"])

new_d$Category <- factor(new_d$Category, levels = c("2018", "2017", "2016", "Baseline"))
new_d$Treatment <- factor(new_d$Treatment, levels = c("Plow", "Hand", "Till", "Mow", "Brush blanket", "None"), labels = c("Plow", "Hand", "Till", "Mow", "Brush Blanket", "No Treatment"))
new_d$Variables <- factor(new_d$Variables, levels = c("Mean_height", "Mean_basal", "Mean_twigs"), labels = c("Height (cm)", "Basal Diameter (mm)", "Twigs (#)"))

new_d2$Treatment <- factor(new_d2$Treatment, levels = c("Plow","Hand", "Till", "Mow", "Brush blanket", "None"), labels = c("Plow", "Hand", "Till", "Mow", "Brush Blanket", "No Treatment"))
new_d2$Variables <- factor(new_d2$Variables, levels = c("Mean_height", "Mean_basal", "Mean_twigs"), labels = c("Height (cm)", "Basal Diameter (mm)", "Twigs (#)"))

# plot
new_d %>%
  ggplot(aes(y = Value, x = Variables, group = Category, color = Category))+
  geom_line(size = 0.5)+
  geom_point(size = 2)+
  facet_wrap(~Treatment)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("White Spruce")+
  theme_ipsum()+
  theme(plot.title = element_text(size = 12),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))
ggsave("growth-WS.png", width = 12, height = 8, dpi = 300)

new_d2%>%
  ggplot(aes(x = Variables, y = Difference, group = Treatment, color = Treatment))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_color_brewer(palette = "Set1") +
  ggtitle("White Spruce")+
  ylab("Growth")+
  theme_ipsum()+
  theme(plot.title = element_text(size = 12),
        legend.title = element_blank())+
  ggsave("difference-WS.png", width = 8, height = 5, dpi = 300)

# read data
d0 <- read.csv("Exclosure.csv", sep = ",")
d1 <- read.csv("Seedling_Data.csv", sep = ",", na.strings = c("M", "missing", "null", "NULL", "n/a", "", "na", " "))

# data preparation
d1$Planted. <- str_replace(d1$Planted., "y`|Y |y ", "Y")

# balsam Fir - add baseline
d1 <- d1%>%
  select(Day : Planted.)%>%
  filter(Planted. == "Y", Height..cm. != "D", Species == "BF")%>%
  mutate(Category = ifelse(Month %in% c(6, 7) & Year == 2016, "Baseline",
                         ifelse(Month == 10 & Year == 2016, "2016",
                                ifelse(Year == 2017, "2017",
                                       ifelse(Year == 2018, "2018", NA)))))

# join treatment to data
new_d <- left_join(d1, d0)
new_d$Height..cm. <- as.numeric(as.character(new_d$Height..cm.))
new_d$Basal.Diameter..mm. <- as.numeric(as.character(new_d$Basal.Diameter..mm.))

# get mean values
new_d <- new_d%>%
  filter(Treatment != "NA")%>%
  group_by(Category, Treatment)%>%
  summarize(Mean_height = mean(Height..cm., na.rm = TRUE),
            Mean_basal = mean(Basal.Diameter..mm., na.rm = TRUE),
            Mean_twigs = mean(X.Twigs, na.rm = TRUE))

# wide to long format
new_d <- gather(new_d, Variables, Value, Mean_height : Mean_twigs, factor_key = TRUE)

new_d2 <- new_d%>%
  filter(Category %in% c("2018", "Baseline"))%>%
  group_by(Treatment, Variables)%>%
  summarize(Difference = Value[Category == "2018"] - Value[Category == "Baseline"])

new_d$Category <- factor(new_d$Category, levels = c("2018", "2017", "2016", "Baseline"))
new_d$Treatment <- factor(new_d$Treatment, levels = c("Holes", "Brush blanket", "No Treatment", "Control"), labels = c("Hand", "Brush Blanket", "No Treatment", "Control"))
new_d$Variables <- factor(new_d$Variables, levels = c("Mean_height", "Mean_basal", "Mean_twigs"),labels = c("Height (cm)", "Basal Diameter (mm)", "Twigs (#)"))

new_d2$Treatment <- factor(new_d2$Treatment, levels = c("Holes", "Brush blanket", "No Treatment", "Control"), labels = c("Hand", "Brush Blanket", "No Treatment", "Control"))
new_d2$Variables <- factor(new_d2$Variables, levels = c("Mean_height", "Mean_basal", "Mean_twigs"), labels = c("Height (cm)", "Basal Diameter (mm)", "Twigs (#)"))

# plot
new_d%>%
  ggplot(aes(x = Variables, y = Value, group = Category, color = Category))+
  geom_line(size = 0.5)+
  geom_point(size = 2)+
  facet_wrap(~Treatment)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Balsam Fir")+
  theme_ipsum()+
  theme(plot.title = element_text(size = 12),
        legend.title = element_blank())
ggsave("growth-BF.png", width = 12, height = 8, dpi = 300)

new_d2%>%
  ggplot(aes(x = Variables, y = Difference, group = Treatment, color = Treatment))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_color_brewer(palette = "Set1") +
  ggtitle("Balsam Fir")+
  ylab("Growth")+
  theme_ipsum()+
  theme(plot.title = element_text(size = 12),
        legend.title = element_blank())
ggsave("difference-BF.png", width = 8, height = 5, dpi = 300)
