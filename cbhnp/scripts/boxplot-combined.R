# task: planting method best promotes growth 
# author: ricky weng

# Library 
library(tidyverse) # dplyr, ggplot2
library(RColorBrewer) # color

# Set workspace
setwd("~/CBHNP/analysis")

# Read data
d0 <- read.csv("Exclosure.csv", sep = ",")
d1 <- read.csv("Seedling_Data.csv", sep = ",", na.strings = c("M", "missing", "null", "NULL", "n/a", "", "na", " "))
d2 <- read.csv("Seedling_Survival.csv", sep = ",")

# Data preparation
d1$Planted. <- str_replace(d1$Planted., "y`|Y |y ", "Y")
d1$Species <- str_replace(d1$Species, "WS ", "WS")
d1$Plot <- str_replace(d1$Plot, "E1 ", "E1")

# Balsam Fir - add baseline
d3 <- d1%>%
  select(Day : Planted.)%>%
  filter(Planted. == "Y", Height..cm. != "D", Species == "BF")%>%
  mutate(Category = ifelse(Month %in% c(6,7) & Year == 2016, "Baseline",
                         ifelse(Month == 10 & Year == 2016, "2016",
                                ifelse(Year == 2017, "2017",
                                       ifelse(Year == 2018, "2018", NA)))))
# Join treatment to data
new_d1 <- left_join(d3, d0)
new_d1$Height..cm. <- as.numeric(as.character(new_d1$Height..cm.))
new_d1$Basal.Diameter..mm. <- as.numeric(as.character(new_d1$Basal.Diameter..mm.))
new_d1$Treatment <- str_replace(new_d1$Treatment, "Holes", "Hand")

d2 <- d2%>%
  select(Plot, Treatment)

# White spruce - add baseline
d4 <- d1%>%
  select(Day : Planted.)%>%
  filter(Planted. == "Y", Height..cm.!="D", Species == "WS")%>%
  mutate(Category = ifelse(Month %in% c(4, 5) & Year == 2016, "Baseline",
                         ifelse(Month == 10 & Year == 2016, "2016",
                                ifelse(Month %in% c(9, 10) & Year == 2017, "2017",
                                       ifelse(Month %in% c(10, 11) & Year == 2018, "2018", NA)))))
# Join treatment to data
new_d2 <- left_join(d4,d2)

new_d2$Height..cm. <- as.numeric(as.character(new_d2$Height..cm.))
new_d2$Basal.Diameter..mm. <- as.numeric(as.character(new_d2$Basal.Diameter..mm.))
new_d2$Treatment <- str_replace(new_d2$Treatment, "None", "No Treatment")

new_d3 <- bind_rows(new_d1, new_d2)


# Get mean values
new_d3 <- new_d3%>%
  filter(Treatment != "NA")%>%
  group_by(Category, Plot, Treatment)%>%
  summarize(Mean_height = mean(Height..cm., na.rm=TRUE),
            Mean_basal = mean(Basal.Diameter..mm., na.rm=TRUE),
            Mean_twigs = mean(X.Twigs, na.rm = TRUE))
new_d3 <- gather(new_d3, Variables, Value, Mean_height : Mean_twigs, factor_key = TRUE)
new_d0 <- new_d3%>%
  filter(Category == "2018")
new_d3 <- new_d3%>%
  filter(Category %in% c("2018", "Baseline"), Plot %in% new_d0$Plot)%>%
  group_by(Plot, Treatment, Variables)%>%
  summarize(Difference = (Value[Category == "2018"] - Value[Category == "Baseline"]))

new_d3$Treatment <- factor(new_d3$Treatment, levels = c("Plow", "Hand", "Till", "Mow", "Brush blanket", "No Treatment", "Control"))
new_d3$Variables <- factor(new_d3$Variables, levels = c("Mean_height", "Mean_basal", "Mean_twigs"), labels = c("Height (cm)", "Basal Diameter (mm)", "Twigs (#)"))

kruskal.test(new_d3$Difference[new_d3$Variables == "Height (cm)"]~new_d3$Treatment[new_d3$Variables == "Height (cm)"])
kruskal.test(new_d3$Difference[new_d3$Variables == "Basal Diameter (mm)"]~new_d3$Treatment[new_d3$Variables == "Basal Diameter (mm)"])
kruskal.test(new_d3$Difference[new_d3$Variables == "Twigs (#)"]~new_d3$Treatment[new_d3$Variables == "Twigs (#)"])

dunnTest(new_d3$Difference[new_d3$Variables == "Height (cm)"]~new_d3$Treatment[new_d3$Variables == "Height (cm)"])
dunnTest(new_d3$Difference[new_d3$Variables == "Basal Diameter (mm)"]~new_d3$Treatment[new_d3$Variables == "Basal Diameter (mm)"])
dunnTest(new_d3$Difference[new_d3$Variables == "Twigs (#)"]~new_d3$Treatment[new_d3$Variables == "Twigs (#)"])

# Plot
new_d3%>%
  ggplot(aes(x = Treatment, y = Difference, group = Treatment, fill = Treatment))+
  stat_boxplot(geom = 'errorbar', width = 0.25)+
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.alpha = 0.5)+
  stat_summary(fun.y = mean, geom = "point", shape = "+", size=5)+
  facet_wrap(~Variables, scale = "free_y", nrow = 1)+
  theme_ipsum()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Set1")+
  ylab("Growth")
ggsave("box-difference-combined.png", width = 8, height = 5, dpi = 300)