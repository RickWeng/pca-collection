# task: visualize scope of retrospective analysis
# author: ricky weng

# library
library(ggplot2) 
library(geosphere)
library(mapproj)
library(raster)
library(ggrepel)

# set working directory
setwd("U:/hyperabundant-report-data")

# read data
location <- read.csv("Location.csv")
# get rid of "National Park"
location$Park <- str_replace(location$Park, "National Park", "")

par(mar=c(0,0,0,0))

# get background map with boundary of provinces
Province <- getData(name = "GADM", country = "CAN", level = 1)
p <- map_data(Province)

# plot
ggplot()+
  geom_polygon(data = p, aes(x = long, y = lat, group = group), fill = "#ececec", color = "white")+
  coord_map("lambert", parameters = c(49, 77))+
  geom_point(data = location, aes(x = lon, y = lat), size = 2, color = "#009e73") +
  geom_text_repel(data = location, aes(x = lon, y = lat, label = Park),
                  size = 3,
                  fontface = "bold",
                  direction = "x",
                  nudge_y = 1.5,
                  segment.color = "grey50")+
  theme_light()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(
    panel.spacing = unit(c(0, 0, 0, 0), "null"),
    plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 12)
  ) 
  
ggsave("scopeofreview3.png", width = 8, height = 5, dpi = 300)
