### Task: Question 4 and 6
### Author: Ricky Weng

# Libraries
library(tidyverse)
library(hrbrthemes)
library(GGally)
library(viridis)
library(RColorBrewer)

# Set workspace
setwd("~/PPNP data/analysis")

# Read csv data file

data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",")

p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Scientific.Name
           )%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"))
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
x <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control,High.or.Low.Density.Shrub.Plot) %>%
  summarize(biodiversity=length(unique(Scientific.Name)))%>%
  mutate(Restoration=ifelse(Site.Name.Macroplot=="DeLaurier North" & Year<=2012,"Before",
                          ifelse(Site.Name.Macroplot=="DeLaurier North" & Year>2012,"After",
                                 ifelse(Site.Name.Macroplot=="Sanctuary" & Year<=2014,"Before",
                                        ifelse(Site.Name.Macroplot=="Sanctuary" & Year>2014,"After",
                                               ifelse(Site.Name.Macroplot=="Sleepy Hollow" & Year<=2014,"Before",
                                                      ifelse(Site.Name.Macroplot=="Sleepy Hollow" & Year>2014,"After",
                                                             ifelse(Site.Name.Macroplot=="Sparrow Field" & Year<=2009,"Before",
                                                                    ifelse(Site.Name.Macroplot=="Sparrow Field" & Year>2009,"After",
                                                                           ifelse(Site.Name.Macroplot=="West Beach 3 (South)" & Year<=2009,"Before",
                                                                                  ifelse(Site.Name.Macroplot=="West Beach 3 (South)" & Year>2009,"After","Unkown")))))))))))
x

x$Restoration=factor(x$Restoration,levels=c("Before","After"))
x$High.or.Low.Density.Shrub.Plot=factor(x$High.or.Low.Density.Shrub.Plot,levels=c("L","H"))
x$Treatment.or.Control=factor(x$Treatment.or.Control,levels=c("T","C"))
x$Year=as.factor(x$Year)
ggplot(x,aes(x=Year,y=biodiversity,color=Treatment.or.Control, group = interaction(Year, Treatment.or.Control)))+
  geom_point(aes(shape=Restoration),size=3)+
  stat_summary(fun.y=mean,geom = "line",aes(group=Treatment.or.Control))+
  facet_grid(High.or.Low.Density.Shrub.Plot~Site.Name.Macroplot)+
  theme(panel.spacing = unit(0,"lines"))+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Species Richness")+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  guides(color=guide_legend(title="Group"))
ggsave("Question4.png",dpi=300,width=14,height=7)
  
ggplot(x,aes(x=Restoration,y=biodiversity,color=Treatment.or.Control,group=interaction(Restoration,Treatment.or.Control)))+
  
  geom_boxplot(width=0.5,outlier.shape =1,outlier.alpha = 0.5)+
  facet_grid(High.or.Low.Density.Shrub.Plot~Site.Name.Macroplot)+
  
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  ylab("Species Richness")+
  theme_ipsum()+
  guides(color=guide_legend(title="Group"))
ggsave("Question42.png",dpi=300,width=14,height=7)  

### Question 6
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",")

p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control,Scientific.Name
)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
x <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control) %>%
  summarize(biodiversity=length(unique(Scientific.Name)))%>%
  mutate(Restoration=ifelse(Site.Name.Macroplot=="DeLaurier North" & Year<=2012,"Before",
                            ifelse(Site.Name.Macroplot=="DeLaurier North" & Year>2012,"After",
                                   ifelse(Site.Name.Macroplot=="Sanctuary" & Year<=2014,"Before",
                                          ifelse(Site.Name.Macroplot=="Sanctuary" & Year>2014,"After",
                                                 ifelse(Site.Name.Macroplot=="Sleepy Hollow" & Year<=2014,"Before",
                                                        ifelse(Site.Name.Macroplot=="Sleepy Hollow" & Year>2014,"After",
                                                               ifelse(Site.Name.Macroplot=="Sparrow Field" & Year<=2009,"Before",
                                                                      ifelse(Site.Name.Macroplot=="Sparrow Field" & Year>2009,"After",
                                                                             ifelse(Site.Name.Macroplot=="West Beach 3 (South)" & Year<=2009,"Before",
                                                                                    ifelse(Site.Name.Macroplot=="West Beach 3 (South)" & Year>2009,"After","Unkown")))))))))))
x

x$Restoration=factor(x$Restoration,levels=c("Before","After"))
x$Treatment.or.Control=factor(x$Treatment.or.Control,levels=c("T","C"))
x$Year=as.factor(x$Year)
ggplot(x,aes(x=Year,y=biodiversity,color=Treatment.or.Control, group = interaction(Year, Treatment.or.Control)))+
  geom_point(aes(shape=Restoration),size=3)+
  stat_summary(fun.y=mean,geom = "line",aes(group=Treatment.or.Control))+
  facet_grid(cols=vars(Site.Name.Macroplot))+
  theme(panel.spacing = unit(0,"lines"))+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Species Richness")+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  guides(color=guide_legend(title="Group"))
ggsave("Question4Q6.png",dpi=300,width=14,height=7)

ggplot(x,aes(x=Restoration,y=biodiversity,color=Treatment.or.Control,group=interaction(Restoration,Treatment.or.Control)))+
  
  geom_boxplot(width=0.5,outlier.shape =1,outlier.alpha = 0.5)+
  facet_grid(cols=vars(Site.Name.Macroplot))+
  
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  ylab("Species Richness")+
  theme_ipsum()+
  guides(color=guide_legend(title="Group"))
ggsave("Question42Q6.png",dpi=300,width=14,height=7)  

# Increase?
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",")

p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Scientific.Name
)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"))

p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
x <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control,High.or.Low.Density.Shrub.Plot) %>%
  summarize(biodiversity=length(unique(Scientific.Name)))%>%
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot,Treatment.or.Control))
  

x
x$category=factor(x$category,levels=c("LT","LC","HT","HC"))
levels(x$category)

DN=subset(x,Site.Name.Macroplot=="DeLaurier North" & Year%in%c(2012,2018))
S=subset(x,Site.Name.Macroplot=="Sanctuary" & Year%in%c(2014,2018))
SH=subset(x,Site.Name.Macroplot=="Sleepy Hollow" & Year%in%c(2014,2018))
SF=subset(x,Site.Name.Macroplot=="Sparrow Field" & Year%in%c(2009,2018))
WB=subset(x,Site.Name.Macroplot=="West Beach 3 (South)" & Year%in%c(2009,2018))
x=rbind(DN,S,SH,SF,WB)
x

x = x%>%
  group_by(Site.Name.Macroplot, Treatment.or.Control,category)%>%
  summarize(difference=biodiversity[which.max(Year)]-biodiversity[which.min(Year)])

x$category=factor(x$category,levels=c("LT","LC","HT","HC"))

ggplot(x, aes(x=category,y=difference)) +
  geom_bar(stat="identity",fill="#66C2A5") +
  scale_x_discrete(limits = rev(levels(x$category)))+
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10)
  ) +
  xlab("Plots")+
  ylab("Change of Species Richness")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("IncreaseQ4.png",dpi=300,width=10,height=8)

### Increase? Q6
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",")

p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control,Scientific.Name
)

p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
x <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control) %>%
  summarize(biodiversity=length(unique(Scientific.Name)))%>%
  mutate(category=paste0(Treatment.or.Control))


x
x$category=factor(x$category,levels=c("T","C"))
levels(x$category)

DN=subset(x,Site.Name.Macroplot=="DeLaurier North" & Year%in%c(2012,2018))
S=subset(x,Site.Name.Macroplot=="Sanctuary" & Year%in%c(2014,2018))
SH=subset(x,Site.Name.Macroplot=="Sleepy Hollow" & Year%in%c(2014,2018))
SF=subset(x,Site.Name.Macroplot=="Sparrow Field" & Year%in%c(2009,2018))
WB=subset(x,Site.Name.Macroplot=="West Beach 3 (South)" & Year%in%c(2009,2018))
x=rbind(DN,S,SH,SF,WB)
x

x = x%>%
  group_by(Site.Name.Macroplot, Treatment.or.Control,category)%>%
  summarize(difference=biodiversity[which.max(Year)]-biodiversity[which.min(Year)])

x$category=factor(x$category,levels=c("T","C"))

ggplot(x, aes(x=category,y=difference)) +
  geom_bar(stat="identity",fill="#66C2A5") +
  scale_x_discrete(limits = rev(levels(x$category)))+
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10)
  ) +
  xlab("Plots")+
  ylab("Change of Species Richness")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("IncreaseQ4Q6.png",dpi=300,width=10,height=8)


