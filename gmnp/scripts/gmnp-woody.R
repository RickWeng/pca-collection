# task: gros morne national park core project 
# author: ricky weng

# library
library(tidyverse) 
library(hrbrthemes)
library(colorspace)
library(scales)

# working directory 
setwd("~/GMNP")

# preprocessing
df <- read.csv("Understory Woody Plant CoRe Survey Data_All Vegetation_2009_2010_2014_2018.csv")
str(df)
head(df)
summary(df)
## before < after 1 < after 2
df$Before_After <- factor(df$Before_After, levels = c("B", "A1", "A2"))

# analysis
## calculation
### sum of heights
growth <- df%>%
  filter(Species%in%c("BaFi", "WhBi", "MoAs", "MoMa", "ChPe"))%>%
  group_by(Plot_ID, Before_After, Manage_Type, Plot_Type)%>%
  summarize(Sum_Height = sum(Height))%>%
  ungroup()
### browse rates
browse <- df%>%
  filter(Species%in%c("BaFi", "WhBi", "MoAs", "MoMa", "ChPe"))%>%
  group_by(Plot_ID, Before_After, Manage_Type, Plot_Type, Browse)%>%
  summarize(Number = n())%>%
  ungroup()%>%
  group_by(Plot_ID, Before_After, Manage_Type, Plot_Type)%>%
  summarize(Browse_Rate = Number[Browse == 1]/sum(Number))
### density
density <- df%>%
  filter(Species%in%c("BaFi", "WhBi", "MoAs", "MoMa", "ChPe"))%>%
  group_by(Plot_ID, Before_After, Manage_Type, Plot_Type)%>%
  summarize(Density = n())

## target checking
### growth target
growth_target <- growth%>%
  group_by(Before_After)%>%
  summarize(Total_Height = sum(Sum_Height))
gt_a2 <- growth_target$Total_Height[growth_target$Before_After == "A2"]
gt_b <- growth_target$Total_Height[growth_target$Before_After == "B"]
gt_achieve <- (gt_a2 - gt_b)/gt_b
gt_achieve
### browse target
browse_target <- df%>%
  filter(Species%in%c("BaFi", "WhBi", "MoAs", "MoMa", "ChPe"))%>%
  group_by(Plot_ID, Before_After, Manage_Type, Plot_Type, Browse)%>%
  summarize(Number = n())%>%
  ungroup()%>%
  group_by(Before_After)%>%
  summarize(Browse_Rate = sum(Number[Browse == 1])/sum(Number))
bt_a2 <- browse_target$Browse_Rate[browse_target$Before_After == "A2"]
bt_b <- browse_target$Browse_Rate[browse_target$Before_After == "B"]
bt_achieve <- bt_b - bt_a2
bt_achieve

# plot
## preparation 
new_labels <- c("Before Hunting\n(2009 & 2010)", "Close After Hunting\n(2014)", "Years After Hunting\n(2018)")
pd <- position_dodge(0.5)
cust_theme <- theme(axis.title.x = element_blank(),
               legend.title = element_blank(),
               axis.title.y=element_text(size=12),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               legend.text = element_text(size=12))
## boxplot
### sum of heights plot
growth%>%
  ggplot(aes(x = Before_After, y = Sum_Height, group = interaction(Plot_Type, Before_After), fill = Plot_Type))+
  stat_boxplot(geom='errorbar',width=0.25,position = pd)+
  geom_boxplot(width = 0.45, outlier.shape = 1, outlier.alpha = 0.5, color = "#2B4635", position = pd)+
  stat_summary(fun.y = mean,geom="point",shape="+",size=5, position = pd)+
  scale_x_discrete(labels = new_labels)+
  scale_fill_manual(values = c(lighten("#0072B2", 0.8), "#0072B2"))+
  ylab("Sum of Heights (cm)")+
  theme_ipsum()+
  cust_theme
### save plot
ggsave("gmnp-woody-growth.png", width = 8, height=6, dpi=300)

### browse rate plot
browse%>%
  ggplot(aes(x = Before_After, y = Browse_Rate, group = interaction(Plot_Type, Before_After), fill = Plot_Type))+
  stat_boxplot(geom='errorbar',width=0.25,position = pd)+
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.alpha = 0.5, color = "#2B4635", position = pd)+
  stat_summary(fun.y = mean,geom="point",shape="+",size=5, position = pd)+
  scale_x_discrete(labels = new_labels)+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  scale_fill_manual(values = c(lighten("#0072B2", 0.8), "#0072B2"))+
  ylab("Browse Rate")+
  theme_ipsum()+
  cust_theme
### save plot
ggsave("gmnp-woody-browse.png", width = 8, height = 6, dpi = 300)

### number of stems plot
density%>%
  ggplot(aes(x = Before_After, y = Density, group = interaction(Plot_Type, Before_After), fill = Plot_Type))+
  stat_boxplot(geom='errorbar',width=0.25,position = pd)+
  geom_boxplot(width = 0.45, outlier.shape = 1, outlier.alpha = 0.5, color = "#2B4635", position = pd)+
  stat_summary(fun.y = mean,geom="point",shape="+",size=5, position = pd)+
  scale_x_discrete(labels = new_labels)+
  scale_fill_manual(values = c(lighten("#0072B2", 0.8), "#0072B2"))+
  ylab("Number of Stems Per Plot")+
  theme_ipsum()+
  cust_theme
### save plot
ggsave("gmnp-woody-density.png", width = 8, height = 6, dpi = 300)
