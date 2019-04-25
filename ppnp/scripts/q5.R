### Task: Question 5 and 6
### Author: Ricky Weng

# Libraries
library(RColorBrewer)
library(tidyverse)
library(hrbrthemes)
library(GGally)
library(viridis)

# Set workspace
setwd("~/PPNP data/analysis")

# Read csv data file

data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"),Native..Introduced=="I",Site.Name.Macroplot=="DeLaurier North")
p

z <- p %>% 
  # Compute the mean
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/9) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2012,2018))%>%
  summarize(difference= if (Year==2012) mean_cover[Year==2018]-mean_cover[Year==2012] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot, Treatment.or.Control))%>%
  group_by(High.or.Low.Density.Shrub.Plot, Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
# Library
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("LT","LC","HT","HC"))

# Plot figures           
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
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
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("DeLaurier North")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("DeLaurier North_species.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"),Native..Introduced=="I",Site.Name.Macroplot=="Sanctuary")
p

z <- p %>% 
  # Compute the mean
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/9) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2014,2018))%>%
  summarize(difference= if (Year==2014) mean_cover[Year==2018]-mean_cover[Year==2014] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot, Treatment.or.Control))%>%
  group_by(High.or.Low.Density.Shrub.Plot, Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
# Library
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("LT","LC","HT","HC"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
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
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("Sanctuary")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("Sanctuary_species.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"),Native..Introduced=="I",Site.Name.Macroplot=="Sleepy Hollow")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/9) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2014,2018))%>%
  summarize(difference= if (Year==2014) mean_cover[Year==2018]-mean_cover[Year==2014] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot, Treatment.or.Control))%>%
  group_by(High.or.Low.Density.Shrub.Plot, Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("LT","LC","HT","HC"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
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
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("Sleepy Hollow")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("Sleepy Hollow_species.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"),Native..Introduced=="I",Site.Name.Macroplot=="Sparrow Field")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/9) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2009,2018))%>%
  summarize(difference= if (Year==2009) mean_cover[Year==2018]-mean_cover[Year==2009] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot, Treatment.or.Control))%>%
  group_by(High.or.Low.Density.Shrub.Plot, Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("LT","LC","HT","HC"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
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
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("Sparrow Field")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("Sparrow Field_species.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"),Native..Introduced=="I",Site.Name.Macroplot=="West Beach 3 (South)")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/9) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2009,2018))%>%
  summarize(difference= if (Year==2009) mean_cover[Year==2018]-mean_cover[Year==2009] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot, Treatment.or.Control))%>%
  group_by(High.or.Low.Density.Shrub.Plot, Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("LT","LC","HT","HC"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
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
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("West Beach 3 (South)")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("West Beach 3 (South)_species.png",dpi=300,width=10,height=8)


### Question 6
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(Native..Introduced=="I",Site.Name.Macroplot=="DeLaurier North")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/18) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control, Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2012,2018))%>%
  summarize(difference= if (Year==2012) mean_cover[Year==2018]-mean_cover[Year==2012] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(Treatment.or.Control))%>%
  group_by(Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("T","C"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)
  ) +
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("DeLaurier North")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("DeLaurier North_speciesQ6.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(Native..Introduced=="I",Site.Name.Macroplot=="Sanctuary")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/18) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2014,2018))%>%
  summarize(difference= if (Year==2014) mean_cover[Year==2018]-mean_cover[Year==2014] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(Treatment.or.Control))%>%
  group_by(Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("T","C"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)
  ) +
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("Sanctuary")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("Sanctuary_speciesQ6.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(Native..Introduced=="I",Site.Name.Macroplot=="Sleepy Hollow")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/18) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2014,2018))%>%
  summarize(difference= if (Year==2014) mean_cover[Year==2018]-mean_cover[Year==2014] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(Treatment.or.Control))%>%
  group_by(Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("T","C"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)
  ) +
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("Sleepy Hollow")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("Sleepy Hollow_speciesQ6.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(Native..Introduced=="I",Site.Name.Macroplot=="Sparrow Field")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/18) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2009,2018))%>%
  summarize(difference= if (Year==2009) mean_cover[Year==2018]-mean_cover[Year==2009] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(Treatment.or.Control))%>%
  group_by(Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("T","C"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)
  ) +
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("Sparrow Field")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("Sparrow Field_speciesQ6.png",dpi=300,width=10,height=8)

###
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(Native..Introduced=="I",Site.Name.Macroplot=="West Beach 3 (South)")
p

z <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/18) %>%
  ungroup()%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control,Native..Introduced, Common.Name) %>%
  filter(Common.Name%in%Common.Name[Year==2018])%>%
  filter(Year%in%c(2009,2018))%>%
  summarize(difference= if (Year==2009) mean_cover[Year==2018]-mean_cover[Year==2009] else if_else(Year==2018,mean_cover[Year==2018],99))%>%
  mutate(change_type=ifelse(difference <= 0, "Decrease", "Increase"))%>%  
  mutate(category=paste0(Treatment.or.Control))%>%
  group_by(Treatment.or.Control)%>%
  arrange(difference,.by_group=TRUE)
warnings()
z$n = as.numeric(factor(z$category))
z
library(plyr)
z = ddply(z,.(category,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
z$x = factor(z$x, levels=z[order(z$difference), 'x'])
z
z$change_type=factor(z$change_type,levels=c("Increase","Decrease"))
z$category=factor(z$category,levels=c("T","C"))
ggplot(z, aes(x=x,y=difference,color=change_type)) +
  geom_segment( aes(x=x ,xend=x, y=0, yend=difference), color="grey") +
  geom_point(size=2) +
  scale_color_manual(labels=c("Increase","Decrease"),values=c("#66C2A5","#FC8D62"))+
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10))+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)
  ) +
  xlab("Invasive Species")+
  ylab("Change of Cover (%)")+
  ggtitle("West Beach 3 (South)")+
  facet_wrap(~category, ncol=1, scale="free_y")+
  theme(legend.title = element_blank(),plot.title = element_text(size=16))
ggsave("West Beach 3 (South)_speciesQ6.png",dpi=300,width=10,height=8)

