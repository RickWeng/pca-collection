# task: descriptive statistics of survey data for gros morne national park
# author: ricky weng

# library
library(tidyverse) # dplyr, ggplot2
library(gmodels) # confidence interval
library(colorspace) # color
library(hrbrthemes) # theme

# set working directory
setwd("U:/GMNP")

# read data
l_19 <- read.csv("Lowlands 2019.csv")
h_19 <- read.csv("Highlands 2019.csv")
l_15 <- read.csv("Lowlands 2015.csv")
h_15 <- read.csv("Highlands 2015.csv")

# preprocessing
l_15 <- l_15%>%
  mutate(Density = Total_moose/Area)
h_15 <- h_15%>%
  mutate(Density = Total_moose/Area)

# define function of summarizing statistics and adding year and ecoregion
ci_summary <- function(x, y, z) {
  x%>%
    summarize(mean = ci(Density, confidence = 0.95, na.rm = TRUE)[1],
              lower_CI = ci(Density, confidence = 0.95, na.rm = TRUE)[2],
              upper_CI = ci(Density, confidence = 0.95, na.rm = TRUE)[3])%>%
    mutate(year = y, ecoregion = z)
}

# apply function
l_15 <- ci_summary(l_15, 2015, 'low')
h_15 <- ci_summary(h_15, 2015, 'high')
l_19 <- ci_summary(l_19, 2019, 'low')
h_19 <- ci_summary(h_19, 2019, 'high')

# merge to one data frame
moose_pop <- rbind(l_15, h_15, l_19, h_19)
moose_pop$year <- factor(moose_pop$year)
moose_pop$ecoregion <- factor(moose_pop$ecoregion, levels = c("low", "high"), labels = c("Lowlands", "Highlands"))

# plot
ggplot(moose_pop, aes(x = year, y = mean, colour = ecoregion, group = ecoregion)) + 
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), colour="#999999", width=.1) +
  geom_line(size=1) +
  geom_point(size=3)+
  scale_color_manual(values=c("#0072B2",lighten("#0072B2",0.8)))+
  xlab("Year")+
  ylab("Density of Moose (#/km2)")+
  theme_ipsum()+
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

# save
ggsave("gmnp-moosepop.png", width = 8, height = 5, dpi = 300)
