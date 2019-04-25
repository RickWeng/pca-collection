# task: growth of white spruce and balsam fir using different planting methods
# author: ricky weng

# library
library(tidyverse) # data preparation
library(hrbrthemes) # theme
library(RColorBrewer) # color

# Set workspace
setwd("~/CBHNP/analysis")

# read data
d0 <- read.csv("Exclosure.csv", sep=",")
d1 <- read.csv("Seedling_Data.csv", sep=",", na.strings = c("M", "missing", "null", "NULL", "n/a", "", "na", " "))

# Data preparation
d1$Planted. <- str_replace(d1$Planted., "y`|Y |y ", "Y")
d1$Plot <- str_replace(d1$Plot, "E1 ", "E1" )

# Balsam Fir - add baseline
d1 <- d1%>%
  select(Day : Planted.)%>%
  filter(Planted. == "Y", Height..cm. != "D", Species == "BF")%>%
  mutate(Category = ifelse(Month %in% c(6, 7) & Year == 2016, "Baseline",
                         ifelse(Month == 10 & Year == 2016, "2016",
                                ifelse(Year == 2017, "2017",
                                       ifelse(Year == 2018, "2018", NA)))))

# Join treatment to data
new_d <- left_join(d1, d0)
new_d$Height..cm. <- as.numeric(as.character(new_d$Height..cm.))
new_d$Basal.Diameter..mm. <- as.numeric(as.character(new_d$Basal.Diameter..mm.))

# Get mean values
new_d <- new_d%>%
  filter(Treatment != "NA")%>%
  group_by(Category, Plot, Treatment)%>%
  summarize(Mean_height = mean(Height..cm., na.rm = TRUE),
            Mean_basal = mean(Basal.Diameter..mm., na.rm = TRUE),
            Mean_twigs = mean(X.Twigs, na.rm = TRUE))

# Wide to long format
new_d <- gather(new_d, Variables, Value, Mean_height : Mean_twigs, factor_key = TRUE)
# Calculate growth
new_d2 <- new_d%>%
  filter(Category %in% c("2018", "Baseline"), Plot != "L11")%>%
  group_by(Plot, Treatment, Variables)%>%
  summarize(Difference = (Value[Category == "2018"] - Value[Category == "Baseline"]))

new_d2$Treatment <- factor(new_d2$Treatment, levels=c("Holes", "Brush blanket", "No Treatment", "Control"), labels = c("Hand", "Brush Blanket", "No Treatment", "Control"))
new_d2$Variables <- factor(new_d2$Variables, levels=c("Mean_height", "Mean_basal", "Mean_twigs"), labels = c("Height (cm)", "Basal Diameter (mm)", "Twigs (#)"))

# Plot
new_d2%>%
  ggplot(aes(x = Treatment, y = Difference, group = Treatment, fill = Treatment))+
  stat_boxplot(geom = 'errorbar', width = 0.25)+
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.alpha = 0.5)+
  stat_summary(fun.y = mean, geom = "point", shape = "+", size = 5)+
  facet_wrap(~Variables, scale ="free_y", nrow = 1)+
  theme_ipsum()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom"
  )+
  scale_fill_brewer(palette = "Set1")+
  ylab("Growth")
ggsave("Box-difference-BF.png", width = 8, height = 5, dpi = 300)

# White Spruce
# Read data
d1 <- read.csv("Seedling_Data.csv", sep = ",", na.strings = c("M", "missing", "null", "NULL", "n/a", "", "na", " "))
d2 <- read.csv("Seedling_Survival.csv", sep = ",")

# Data preparation
d1$Planted. <- str_replace(d1$Planted., "y`|Y |y ", "Y")
d1$Species <- str_replace(d1$Species, "WS ", "WS")

d2 <- d2%>%
  select(Plot, Treatment)

# White spruce - add baseline
d1 <- d1%>%
  select(Day : Planted.)%>%
  filter(Planted. == "Y", Height..cm. != "D", Species == "WS")%>%
  mutate(Category = ifelse(Month %in% c(4, 5) & Year == 2016, "Baseline",
                         ifelse(Month == 10 & Year == 2016, "2016",
                                ifelse(Month %in% c(9, 10) & Year == 2017, "2017",
                                       ifelse(Month %in% c(10, 11) & Year == 2018, "2018", NA)))))

# Join treatment to data
new_d <- left_join(d1, d2)

new_d$Height..cm. <- as.numeric(as.character(new_d$Height..cm.))
new_d$Basal.Diameter..mm. <- as.numeric(as.character(new_d$Basal.Diameter..mm.))

# Get mean values
new_d <- new_d%>%
  group_by(Plot, Category, Treatment)%>%
  summarize(Mean_height = mean(Height..cm., na.rm = TRUE),
            Mean_basal = mean(Basal.Diameter..mm., na.rm = TRUE),
            Mean_twigs = mean(X.Twigs, na.rm = TRUE))
# Wide to long format
new_d <- gather(new_d, Variables, Value, Mean_height : Mean_twigs, factor_key = TRUE)

# Calculate growth
new_d1 <- new_d%>%
  filter(Category == "2018")
new_d2 <- new_d%>%
  filter(Category %in% c("2018", "Baseline"), Plot %in% new_d1$Plot)%>%
  group_by(Plot, Treatment, Variables)%>%
  summarize(Difference = Value[Category == "2018"] - Value[Category == "Baseline"])

new_d2$Treatment <- factor(new_d2$Treatment, levels = c("Plow", "Hand", "Till", "Mow", "Brush blanket", "None"),labels = c("Plow", "Hand", "Till", "Mow", "Brush Blanket", "No Treatment"))
new_d2$Variables <- factor(new_d2$Variables, levels = c("Mean_height", "Mean_basal", "Mean_twigs"),labels = c("Height (cm)", "Basal Diameter (mm)", "Twigs (#)"))

# Plot
new_d2%>%
  ggplot(aes(x = Treatment, y = Difference, group = Treatment, fill = Treatment))+
  stat_boxplot(geom = 'errorbar', width = 0.25)+
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.alpha = 0.5)+
  stat_summary(fun.y = mean, geom = "point", shape = "+", size = 5)+
  facet_wrap(~Variables, scale = "free_y", nrow = 1)+
  theme_ipsum()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Set1")+
  ylab("Growth")
ggsave("Box-difference-WS.png", width = 8, height = 5, dpi = 300)



