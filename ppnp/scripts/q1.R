### Task: Question 1 and 6
### Author: Ricky Weng

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

### Native species

# Set workspace
setwd("~/PPNP data/analysis")


data <- read.csv("LESSS_analysis.csv",header=TRUE,sep=",")
p <- select(data,Year,Site.Name.Macroplot, Micro.plot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot
           ,Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.,X..Cover)%>%
  filter(Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.=="K",High.or.Low.Density.Shrub.Plot%in%c("H","L"))
p <- p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
p <- p %>% 
  # Compute the mean cover, sd
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Micro.plot) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE))%>%
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot)%>%
  summarize(sd=sd(mean_cover),mean_cover=sum(mean_cover)/9)%>%
  # Add restoration status
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

p
p$Restoration=factor(p$Restoration,levels=c("Before","After"))
p$High.or.Low.Density.Shrub.Plot=factor(p$High.or.Low.Density.Shrub.Plot,levels=c("L","H"))
p$Treatment.or.Control=factor(p$Treatment.or.Control,levels=c("T","C"))
p$Year=factor(p$Year)

# Plot figures
ggplot(p,aes(x=Year,y=mean_cover,color=Treatment.or.Control, group = interaction(Year, Treatment.or.Control)))+
  geom_point(aes(shape=Restoration),size=3)+
  theme(panel.spacing = unit(0,"lines"))+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(cols= vars(Site.Name.Macroplot),rows=vars(High.or.Low.Density.Shrub.Plot))+
  stat_summary(fun.y=mean,geom = "line",aes(group=Treatment.or.Control))+
  ylab("percent cover (%)")+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  guides(color=guide_legend(title="Group"))+
  geom_errorbar(aes(ymin=mean_cover-sd, ymax=mean_cover+sd), width=.2,
                position=position_dodge(0.05))
ggsave("SDplotQ1.png",width=14,height=7,dpi=300)

ggplot(p,aes(x=Year,y=mean_cover,color=Treatment.or.Control, group = interaction(Year, Treatment.or.Control)))+
  geom_point(aes(shape=Restoration),size=3)+
  theme(panel.spacing = unit(0,"lines"))+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(cols= vars(Site.Name.Macroplot),rows=vars(High.or.Low.Density.Shrub.Plot))+
  stat_summary(fun.y=mean,geom = "line",aes(group=Treatment.or.Control))+
  ylab("percent cover (%)")+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  guides(color=guide_legend(title="Group"))
ggsave("Question1.png",width=14,height=7,dpi=300)

ggplot(p,aes(x=Restoration,y=mean_cover,color=Treatment.or.Control,group=interaction(Restoration,Treatment.or.Control)))+
  geom_boxplot(width=0.5,outlier.shape =1,outlier.alpha = 0.5)+
  facet_grid(High.or.Low.Density.Shrub.Plot~Site.Name.Macroplot)+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  ylab("percent cover (%)")+
  theme_ipsum()+
  guides(color=guide_legend(title="Group"))
ggsave("Question1box.png",width=14,height=7,dpi=300)


### Question 6

data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",")
p = select(data,Year,Site.Name.Macroplot, Micro.plot,Treatment.or.Control
           ,Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.,X..Cover)%>%
  filter(Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.=="K")
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
p <- p %>% 
  # Compute the mean cover
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control,Micro.plot) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE))%>%
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control)%>%
  summarize(sd=sd(mean_cover),mean_cover=sum(mean_cover)/18)%>%
  # Add restoration status
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

p
p$Restoration=factor(p$Restoration,levels=c("Before","After"))
p$Treatment.or.Control=factor(p$Treatment.or.Control,levels=c("T","C"))
p$Year=factor(p$Year)

# Plot figures
ggplot(p,aes(x=Year,y=mean_cover,color=Treatment.or.Control, group = interaction(Year, Treatment.or.Control)))+
  geom_point(aes(shape=Restoration),size=3)+
  theme(panel.spacing = unit(0,"lines"))+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(cols= vars(Site.Name.Macroplot))+
  stat_summary(fun.y=mean,geom = "line",aes(group=Treatment.or.Control))+
  ylab("percent cover (%)")+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  guides(color=guide_legend(title="Group"))+
  geom_errorbar(aes(ymin=mean_cover-sd, ymax=mean_cover+sd), width=.2,
                position=position_dodge(0.05))
ggsave("SDplotQ1Q6.png",width=14,height=7,dpi=300)

ggplot(p,aes(x=Year,y=mean_cover,color=Treatment.or.Control, group = interaction(Year, Treatment.or.Control)))+
  geom_point(aes(shape=Restoration),size=3)+
  theme(panel.spacing = unit(0,"lines"))+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(cols= vars(Site.Name.Macroplot))+
  stat_summary(fun.y=mean,geom = "line",aes(group=Treatment.or.Control))+
  ylab("percent cover (%)")+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  guides(color=guide_legend(title="Group"))
ggsave("Question1Q6.png",width=14,height=7,dpi=300)

ggplot(p,aes(x=Restoration,y=mean_cover,color=Treatment.or.Control,group=interaction(Restoration,Treatment.or.Control)))+
  geom_boxplot(width=0.5,outlier.shape =1,outlier.alpha = 0.5)+
  facet_grid(cols=vars(Site.Name.Macroplot))+
  scale_color_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  ylab("percent cover (%)")+
  theme_ipsum()+
  guides(color=guide_legend(title="Group"))
ggsave("Question1boxQ6.png",width=14,height=7,dpi=300)


### 5% increase?

data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",")
p = select(data,Year,Site.Name.Macroplot, Micro.plot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot
           ,Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.,X..Cover)%>%
  filter(Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.=="K",High.or.Low.Density.Shrub.Plot%in%c("H","L"))
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
p <- p %>% 
  # Compute the mean
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Micro.plot) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE))%>%
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot)%>%
  summarize(sd=sd(mean_cover),mean_cover=sum(mean_cover)/9)%>%
  ungroup()%>%
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot,Treatment.or.Control))%>%
  # Add restoration status
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
p
p$category=factor(p$category,levels=c("LT","LC","HT","HC"))
levels(p$category)

DN=subset(p,Site.Name.Macroplot=="DeLaurier North" & Year%in%c(2012,2018))
S=subset(p,Site.Name.Macroplot=="Sanctuary" & Year%in%c(2014,2018))
SH=subset(p,Site.Name.Macroplot=="Sleepy Hollow" & Year%in%c(2014,2018))
SF=subset(p,Site.Name.Macroplot=="Sparrow Field" & Year%in%c(2009,2018))
WB=subset(p,Site.Name.Macroplot=="West Beach 3 (South)" & Year%in%c(2009,2018))
p=rbind(DN,S,SH,SF,WB)
p

p = p%>%
  group_by(Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,category)%>%
  summarize(difference=mean_cover[which.max(Year)]-mean_cover[which.min(Year)])%>%
  mutate(type=ifelse(difference < 5, "< 5%", ">= 5%"))


p$type=factor(p$type,levels=c(">= 5%","< 5%"))
p$category<-factor(x=p$category,levels=c("LT","LC","HT","HC"))

# Plot figures
ggplot(p, aes(x=category,y=difference,fill=type)) +
  geom_bar(stat="identity") +
  scale_fill_manual(labels=c(">= 5%","< 5%"),values=c("#66C2A5","#FC8D62"))+
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
  ylab("Change of Cover (%)")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))+
  scale_x_discrete(limits = rev(levels(p$category)))
ggsave("Percent5Q1.png",dpi=300,width=10,height=8)

ggplot(p, aes(x=category,y=difference,color=type)) +
  geom_segment( aes(x=category ,xend=category, y=0, yend=difference), color="grey") +
  geom_point(size=3) +
  scale_color_manual(labels=c(">= 5%","< 5%"),values=c("#66C2A5","#FC8D62"))+
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
  ylab("Change of Cover (%)")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))+
  scale_x_discrete(limits = rev(levels(p$category)))
ggsave("Percent5Q1.png",dpi=300,width=10,height=8)


# 5% increase? Q6
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",")
p = select(data,Year,Site.Name.Macroplot, Micro.plot,Treatment.or.Control
           ,Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.,X..Cover)%>%
  filter(Remove.all..Invasive.or.hyperabundant...Neutral..Native.but.not.what.we.are.trying.for...Keep..LESSS.desirable...Unknown..could.be.either.species.unknown.=="K")
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p
p <- p %>% 
  # Compute the mean
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control,Micro.plot) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE))%>%
  group_by(Year,Site.Name.Macroplot, Treatment.or.Control)%>%
  summarize(sd=sd(mean_cover),mean_cover=sum(mean_cover)/18)%>%
  ungroup()%>%
  mutate(category=paste0(Treatment.or.Control))%>% 
  # Add restoration status
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
p
p$category=factor(p$category,levels=c("T","C"))
levels(p$category)

DN=subset(p,Site.Name.Macroplot=="DeLaurier North" & Year%in%c(2012,2018))
S=subset(p,Site.Name.Macroplot=="Sanctuary" & Year%in%c(2014,2018))
SH=subset(p,Site.Name.Macroplot=="Sleepy Hollow" & Year%in%c(2014,2018))
SF=subset(p,Site.Name.Macroplot=="Sparrow Field" & Year%in%c(2009,2018))
WB=subset(p,Site.Name.Macroplot=="West Beach 3 (South)" & Year%in%c(2009,2018))
p=rbind(DN,S,SH,SF,WB)
p

p = p%>%
  group_by(Site.Name.Macroplot, Treatment.or.Control,category)%>%
  summarize(difference=mean_cover[which.max(Year)]-mean_cover[which.min(Year)])%>%
  mutate(type=ifelse(difference < 5, "< 5%", ">= 5%"))


p$type=factor(p$type,levels=c(">= 5%","< 5%"))
p$category=factor(p$category,levels=c("T","C"))

# Plot figures
ggplot(p, aes(x=category,y=difference,fill=type)) +
  geom_bar(stat="identity") +
  scale_fill_manual(labels=c(">= 5%","< 5%"),values=c("#66C2A5","#FC8D62"))+
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
  ylab("Change of Cover (%)")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))+
  scale_x_discrete(limits = rev(levels(p$category)))
ggsave("Percent5Q1Q6.png",dpi=300,width=10,height=8)

ggplot(p, aes(x=category,y=difference,color=type)) +
  geom_segment( aes(x=category ,xend=category, y=0, yend=difference), color="grey") +
  geom_point(size=3) +
  scale_color_manual(labels=c(">= 5%","< 5%"),values=c("#66C2A5","#FC8D62"))+
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
  ylab("Change of Cover (%)")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))+
  scale_x_discrete(limits = rev(levels(p$category)))
ggsave("Percent5Q1Q6.png",dpi=300,width=10,height=8)




