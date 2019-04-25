### Task: Question 5 (2) and 6
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

data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"),Native..Introduced=="I",Year==2018)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p=p%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced,Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/9) %>%
  mutate(category2=paste0(Site.Name.Macroplot,High.or.Low.Density.Shrub.Plot, Treatment.or.Control))%>%
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot, Treatment.or.Control))
z <- p %>% 
  # Compute the proportions:
  group_by(Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot,Native..Introduced)%>%
  summarize(mean_cover=max(mean_cover))%>%
  ungroup()
t=inner_join(p, z, by = c("Site.Name.Macroplot","Treatment.or.Control", "High.or.Low.Density.Shrub.Plot","mean_cover"))
t
t$n = as.numeric(factor(t$category2))
t

library(plyr)
t = ddply(t,.(category2,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
t$x = factor(t$x, levels=t[order(t$category2), 'x'])
t$category=factor(t$category, levels=c("LT","LC","HT","HC"))


ggplot(t, aes(x=x,y=mean_cover,fill= category) ) +
  geom_bar(stat="identity",position="dodge") +
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10),
        legend.title = element_blank())+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)
  ) +
  scale_fill_brewer(palette="Set3")+
  xlab("Invasive Species with the Highest Percent Cover")+
  ylab("Percent Cover (%)")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")
ggsave("Q52.png",dpi=300,width=10,height=8)

### Question 6
detach(package:plyr)
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)

p = select(data,Year,Site.Name.Macroplot, Treatment.or.Control, Native..Introduced,Common.Name
           ,X..Cover)%>%
  filter(Native..Introduced=="I",Year==2018)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p=p%>%
  group_by(Site.Name.Macroplot,Treatment.or.Control,Native..Introduced,Common.Name) %>%
  summarize(mean_cover=sum(X..Cover,na.rm=TRUE)/18) %>%
  mutate(category2=paste0(Site.Name.Macroplot,Treatment.or.Control))%>%
  mutate(category=paste0(Treatment.or.Control))
z <- p %>% 
  # Compute the proportions:
  group_by(Site.Name.Macroplot,Treatment.or.Control,Native..Introduced)%>%
  summarize(mean_cover=max(mean_cover))%>%
  ungroup()
t=inner_join(p, z, by = c("Site.Name.Macroplot","Treatment.or.Control", "mean_cover"))
t
t$n = as.numeric(factor(t$category2))
t

library(plyr)
t = ddply(t,.(category2,Common.Name),transform, x=paste(c(rep(' ',n-1), Common.Name), collapse=''))
t$x = factor(t$x, levels=t[order(t$category2), 'x'])
t$category=factor(t$category, levels=c("T","C"))


ggplot(t, aes(x=x,y=mean_cover,fill= category) ) +
  geom_bar(stat="identity",position="dodge") +
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.y = element_text(size=10),
        legend.title = element_blank())+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12)
  ) +
  scale_fill_manual(labels=c("Treatment","Control"),values=c("#66C2A5","#FC8D62"))+
  xlab("Invasive Species with the Highest Percent Cover")+
  ylab("Percent Cover (%)")+
  facet_wrap(~Site.Name.Macroplot, ncol=1, scale="free_y")
ggsave("Q52Q6.png",dpi=300,width=10,height=8)
