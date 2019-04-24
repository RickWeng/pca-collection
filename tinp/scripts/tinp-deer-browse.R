# task: deer browse analysis for thousand islands national park
# author: ricky

# library
library(tidyverse) # dplyr, ggplot2
library(gmodels) # confidence interval
library(hrbrthemes) # theme
library(viridis) # color
library(ggthemes) # theme
library(scales) # percent transformation

# set working directory
setwd("~/")

# read data
s <- read.csv("Thousand_Islands_NP_DeerBrowseSpring_2011-2018_Data.csv")
sw <- read.csv("Thousand_Islands_NP_DeerBrowseSummerWood_2011-2018_Data.csv")

# preprocessing
## delete French header 
s <- s[-1, ]
sw <- sw[-1, ]
## change multiple columns to numeric
s[ , c(1, 11:15)]<-apply(s[ , c(1, 11:15)], 2, function(x) as.numeric(as.character(x)))
sw[ , c(1, 14:15)]<-apply(sw[ , c(1, 14:15)], 2, function(x) as.numeric(as.character(x)))
## change header name
names(sw)[14] <- "seedlings"
names(sw)[15] <- "saplings"

# spring stems at different sites
## calculate
sum_browse <- s%>%
  filter(Plot.Name%in%c("HillDBA", "HillDBB"))%>%
  gather(key = "percent", value = "number", 11:15)%>%
  group_by(Year, Plot.Name)%>%
  summarize(number = sum(number))
## plot
theme_format <- theme(axis.title.x = element_text(size=10),
                      axis.title.y = element_text(size=10),
                      axis.text.x = element_text(size=10),
                      axis.text.y = element_text(size=10))

ggplot(sum_browse, aes(x = Year, y = number))+
  geom_point(size = 2)+
  geom_line()+
  facet_wrap(~Plot.Name)+
  ylab("Number of Stems")+
  theme_ipsum()+
  theme_format
## save
ggsave("tinp-spring-sum.png", width = 8, height = 5, dpi = 300)

# spring stems at different browse level
## calculate
browse <- s%>%
  filter(Plot.Name%in%c("HillDBA", "HillDBB"))%>%
  gather(key = "percent", value = "number", 11:15)%>%
  group_by(Year, Plot.Name, percent)%>%
  summarize(number = sum(number))%>%
  ungroup()%>%
  group_by(Year, Plot.Name)%>%
  mutate(prop = number/sum(number))

browse$percent <- factor(browse$percent, levels = c("No.Deer.Browse", "X25.Percent.Deer.Browse", "X50.Percent.Deer.Browse", "X75.Percent.Deer.Browse", "X100.Percent.Deer.Browse"), labels = c("0%", "25%", "50%", "75%", "100%"))
browse$Year <- factor(browse$Year)
## heatmap
ggplot(browse, aes(x=Year, y= percent, fill = prop))+
  geom_tile(color="white", size=0.1)+
  scale_fill_viridis(name="Stems", labels = percent)+
  coord_equal()+
  facet_wrap(~Plot.Name)+
  labs(x = NULL, y = "Browse Level", title=NULL)+
  theme_tufte(base_family="Helvetica")+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=10))+
  theme(panel.border=element_blank())+
  theme(plot.title=element_text(hjust=0))+
  theme(strip.text=element_text(hjust=0))+
  theme(panel.margin.x=unit(0.5, "cm"))+
  theme(panel.margin.y=unit(0.5, "cm"))+
  theme(legend.title=element_text(size=8))+
  theme(legend.title.align=1)+
  theme(legend.text=element_text(size=8))+
  theme(legend.position="bottom")+
  theme(legend.key.size=unit(0.5, "cm"))+
  theme(legend.key.width=unit(1.5, "cm"))
## save
ggsave("tinp-spring-prop.png", width = 8, height = 5, dpi = 300)

# summer woods at different sites
## select plot hilldba and hilldbb
hill_sw <- sw%>%
  filter(Plot.Name%in%c("HillDBA", "HillDBB"))
## descriptive statistics of seedlings 
hill_seedling <- hill_sw%>%
  group_by(Year, Plot.Name)%>%
  summarize(mean = ci(seedlings, confidence = 0.95, na.rm = TRUE)[1],
            lower_CI = ci(seedlings, confidence = 0.95, na.rm = TRUE)[2],
            upper_CI = ci(seedlings, confidence = 0.95, na.rm = TRUE)[3])
## plot
ggplot(hill_seedling, aes(x = Year, y = mean))+
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), colour="#999999", width=.3)+
  geom_line()+
  geom_point(size = 2)+
  facet_wrap(~Plot.Name)+
  ylab("Number of Seedlings (5-30cm)")+
  theme_ipsum()+
  theme_format
## save
ggsave("tinp-sw-seedling.png", width = 8, height = 5, dpi = 300)

## descriptive statistics of saplings
hill_sapling <- hill_sw%>%
  group_by(Year, Plot.Name)%>%
  summarize(sum = sum(saplings, na.rm = TRUE))
## plot
ggplot(hill_sapling, aes(x = Year, y = sum))+
  geom_line()+
  geom_point(size = 2)+
  facet_wrap(~Plot.Name)+
  ylab("Number of Saplings (30-200cm)")+
  theme_ipsum()+
  theme_format
## save
ggsave("tinp-sw-sapling.png", width = 8, height = 5, dpi = 300)

# summer woods by species
## calculate total seedlings for each specie
hill_species <- hill_sw%>%
  group_by(Year, Plot.Name, Species.Common.Name)%>%
  summarize(sum_sp = sum(seedlings, na.rm = TRUE))
## top 5 species in 2018 at hilldba
dba_5 <- hill_species%>%
  filter(Year == 2018, Plot.Name == "HillDBA")%>%
  top_n(5, sum_sp)%>%
  pull(Species.Common.Name)
## top 5 species in 2018 at hilldbb
dbb_5 <- hill_species%>%
  filter(Year == 2018, Plot.Name == "HillDBB")%>%
  top_n(5, sum_sp)%>%
  pull(Species.Common.Name)
## create a new dataframe for hilldba
year = rep(c(2010, 2012, 2014, 2016, 2017, 2018), each = 32)
species = rep(levels(dba$Species.Common.Name), times = 6)
df = data.frame("Year" = year, "Species.Common.Name" = species)
## get zeros at hilldba
dba <- hill_species%>%
  filter(Plot.Name == "HillDBA")
dba_truezero <- df%>%
  left_join(dba, by = c("Year", "Species.Common.Name"))
dba_truezero$sum_sp=replace(dba_truezero$sum_sp, is.na(dba_truezero$sum_sp), 0)
## add categories 
dba_truezero <- dba_truezero%>%
  mutate(cate = ifelse(Species.Common.Name %in% dba_5, as.character(Species.Common.Name), "Others"))
dba_truezero$cate = factor(dba_truezero$cate, levels = c("Sugar Maple", "Red Maple", "American Beech", "White Ash", "Poplar sp.", "Others"))
## plot
ggplot(dba_truezero, aes(x = Year, y = sum_sp, group = Species.Common.Name, color = cate))+
  geom_line(size = 1, alpha=0.5)+
  geom_point(size = 2)+
  scale_color_manual(name = "Top 5 Species", values = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF", "grey"))+
  ylab("Number of Seedlings (5-30cm)")+
  theme_ipsum()+
  theme_format+
  theme(legend.title = element_text(size=10))
## save
ggsave("tinp-dba-sp.png", width = 8, height = 5, dpi = 300)
## create a new dataframe for hilldbb
year = rep(c(2010, 2012, 2014, 2016, 2017, 2018), each = 32)
species = rep(levels(dbb$Species.Common.Name), times = 6)
df = data.frame("Year" = year, "Species.Common.Name" = species)
## get zeros at hilldbb
dbb <- hill_species%>%
  filter(Plot.Name == "HillDBB")
dbb_truezero <- df%>%
  left_join(dbb, by = c("Year", "Species.Common.Name"))
dbb_truezero$sum_sp=replace(dbb_truezero$sum_sp, is.na(dbb_truezero$sum_sp), 0)
## add categories 
dbb_truezero <- dbb_truezero%>%
  mutate(cate = ifelse(Species.Common.Name %in% dbb_5, as.character(Species.Common.Name), "Others"))
dbb_truezero$cate = factor(dbb_truezero$cate, levels = c("Red Maple", "White Ash", "Bitternut Hickory", "Ironwood", "Blue Beech", "Others"))
## plot
ggplot(dbb_truezero, aes(x = Year, y = sum_sp, group = Species.Common.Name, color = cate))+
  geom_line(size = 1, alpha=0.5)+
  geom_point(size = 2)+
  scale_color_manual(name = "Top 5 Species", values = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF", "grey"))+
  ylab("Number of Seedlings (5-30cm)")+
  theme_ipsum()+
  theme_format+
  theme(legend.title = element_text(size=10))
## save
ggsave("tinp-dbb-sp.png", width = 8, height = 5, dpi = 300)