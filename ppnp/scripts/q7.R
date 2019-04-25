### Task: Question 7
### Author: Ricky Weng

# Libraries
library(dplyr)         # data.frame operations
library(tidyr)
library(purrr)         # list/vecgtor munging
library(ggplot2)       #
library(scales)        # working with ggplot2 for label formatting
library(gridExtra)     # working with ggplots for arranging plots
library(ggthemes)      # clean theme for ggplot2
library(viridis)       # color palette   

# Set workspace
setwd("~/PPNP data/analysis")

# Read csv data file
## NA were not recorded consistently (e.g. n/a, na...) in original data sheet, make it readable
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",na.strings = c("NA","na","n/a","N/A"))

# Data Preparation
p = select(data,Year,
          Site.Name.Macroplot, 
          Treatment.or.Control, 
          High.or.Low.Density.Shrub.Plot,
          X..Canopy.Cover)%>%
  filter(High.or.Low.Density.Shrub.Plot%in%c("H","L"))
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p$Year=as.factor(p$Year)
x <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control, High.or.Low.Density.Shrub.Plot) %>%
  summarize(average_canopycover=mean(X..Canopy.Cover,na.rm=TRUE)) %>%
  mutate(category=paste0(High.or.Low.Density.Shrub.Plot, Treatment.or.Control))
x
Heatmap_overall=ggplot(x, aes(x=Year, y=category, fill=average_canopycover))+
  geom_tile(color="white", size=0.1)+
  scale_fill_viridis(name="Average % Canopy Cover")+
  coord_equal()+
  facet_wrap(~Site.Name.Macroplot, ncol=2)+
  labs(x=NULL, y=NULL, title=NULL)+
  theme_tufte(base_family="Helvetica")+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=5))+
  theme(panel.border=element_blank())+
  theme(plot.title=element_text(hjust=0))+
  theme(strip.text=element_text(hjust=0))+
  theme(panel.margin.x=unit(0.5, "cm"))+
  theme(panel.margin.y=unit(0.5, "cm"))+
  theme(legend.title=element_text(size=6))+
  theme(legend.title.align=1)+
  theme(legend.text=element_text(size=6))+
  theme(legend.position="bottom")+
  theme(legend.key.size=unit(0.2, "cm"))+
  theme(legend.key.width=unit(1, "cm"))
Heatmap_overall
ggsave("Q7_overall.png",width=6,height=8,units="in",dpi=200,bg="white")

z=levels(as.factor(x$Site.Name.Macroplot))

Heatmap_individual=lapply(z,function(sn) {
  ggplot(filter(x,Site.Name.Macroplot==sn),aes(x=Year, y=category, fill=average_canopycover))+
    geom_tile(color="white", size=0.1)+
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))+
    scale_fill_viridis(name="Average % Canopy Cover")+
    coord_equal()+
    labs(x=NULL, y=NULL, title=sprintf("%s", sn))+
    theme_tufte(base_family="Helvetica")+
    theme(axis.ticks=element_blank())+
    theme(axis.text=element_text(size=5))+
    theme(panel.border=element_blank())+
    theme(plot.title=element_text(hjust=0))+
    theme(strip.text=element_text(hjust=0))+
    theme(panel.margin.x=unit(0.5, "cm"))+
    theme(panel.margin.y=unit(0.5, "cm"))+
    theme(legend.title=element_text(size=6))+
    theme(legend.title.align=1)+
    theme(legend.text=element_text(size=6))+
    theme(legend.position="bottom")+
    theme(legend.key.size=unit(0.2, "cm"))+
    theme(legend.key.width=unit(1, "cm"))
})
Heatmap_individual

Heatmap_individual[["ncol"]] <- 2
g= do.call(grid.arrange, Heatmap_individual)
ggsave("Q7_individual.png",g,width=8,height=8,units="in",dpi=200,bg="white")

### Regardless of shrub density
# Read csv data file
## NA were not recorded consistently (e.g. n/a, na...) in original data sheet, make it readable
data = read.csv("LESSS_analysis.csv",header=TRUE,sep=",",na.strings = c("NA","na","n/a","N/A"))

# Data Preparation
p = select(data,Year,
           Site.Name.Macroplot, 
           Treatment.or.Control, 
           X..Canopy.Cover)
p=p[!(p$Site.Name.Macroplot=="Sanctuary" & p$Year%in%c(2009,2012,2013)),]
p$Year=as.factor(p$Year)
x <- p %>% 
  # Compute the proportions:
  group_by(Year,Site.Name.Macroplot,Treatment.or.Control) %>%
  summarize(average_canopycover=mean(X..Canopy.Cover,na.rm=TRUE)) %>%
  mutate(category=paste0(Treatment.or.Control))
x
Heatmap_overall=ggplot(x, aes(x=Year, y=category, fill=average_canopycover))+
  geom_tile(color="white", size=0.1)+
  scale_fill_viridis(name="Average % Canopy Cover")+
  coord_equal()+
  facet_wrap(~Site.Name.Macroplot, ncol=2)+
  labs(x=NULL, y=NULL, title=NULL)+
  theme_tufte(base_family="Helvetica")+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=5))+
  theme(panel.border=element_blank())+
  theme(plot.title=element_text(hjust=0))+
  theme(strip.text=element_text(hjust=0))+
  theme(panel.margin.x=unit(0.5, "cm"))+
  theme(panel.margin.y=unit(0.5, "cm"))+
  theme(legend.title=element_text(size=6))+
  theme(legend.title.align=1)+
  theme(legend.text=element_text(size=6))+
  theme(legend.position="bottom")+
  theme(legend.key.size=unit(0.2, "cm"))+
  theme(legend.key.width=unit(1, "cm"))
Heatmap_overall
ggsave("Q7_combine_overall.png",width=8,height=6,units="in",dpi=200,bg="white")

z=levels(as.factor(x$Site.Name.Macroplot))

Heatmap_4=lapply(z,function(sn) {
  ggplot(filter(x,Site.Name.Macroplot==sn),aes(x=Year, y=category, fill=average_canopycover))+
    geom_tile(color="white", size=0.1)+
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))+
    scale_fill_viridis(name="Average % Canopy Cover")+
    coord_equal()+
    labs(x=NULL, y=NULL, title=sprintf("%s", sn))+
    theme_tufte(base_family="Helvetica")+
    theme(axis.ticks=element_blank())+
    theme(axis.text=element_text(size=5))+
    theme(panel.border=element_blank())+
    theme(plot.title=element_text(hjust=0))+
    theme(strip.text=element_text(hjust=0))+
    theme(panel.margin.x=unit(0.5, "cm"))+
    theme(panel.margin.y=unit(0.5, "cm"))+
    theme(legend.title=element_text(size=6))+
    theme(legend.title.align=1)+
    theme(legend.text=element_text(size=6))+
    theme(legend.position="bottom")+
    theme(legend.key.size=unit(0.2, "cm"))+
    theme(legend.key.width=unit(1, "cm"))
})
Heatmap_4

Heatmap_4[["ncol"]] <- 2
g= do.call(grid.arrange, Heatmap_4)
ggsave("Q7_combine_individual",g,width=8,height=8,units="in",dpi=200,bg="white")

