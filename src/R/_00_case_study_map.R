
# ********************************************************************************** #
# Plot Case Study Site.
# Programmed By Pooya Shirazi, May, 2018.
# Ferdowsi University Of Mashhad, Mashhad, Iran.
# ********************************************************************************** #

# REQUIREMENT: --------------------------------------------------------------------------------

rm(list = ls())         # remove objects from a specified environment.
graphics.off()          # shuts down all open graphics devices.

library(tidyverse)      # r packages for data science.
library(rgdal)          # bindings for the 'geospatial' data abstraction library.
library(ggmap)          # spatial visualization with ggplot2.

# BODY CODE: ----------------------------------------------------------------------------------
# 01. load iran, sea and gorgan basin shapefile and converted to a dataframe:
iran <- rgdal::readOGR(dsn = "Articles/A01/data/ShapeFiles/Iran", layer = "Iran") %>% 
    ggplot2::fortify()

sea <- rgdal::readOGR(dsn = "Articles/A01/data/ShapeFiles/Sea", layer = "Sea") %>% 
    ggplot2::fortify()

khorasan <- rgdal::readOGR(dsn = "Articles/A01/data/ShapeFiles/Khorasan", layer = "Khorasan") %>% 
    ggplot2::fortify()

# 02. Load Point Data:
infoStations <- read.csv(file = "Articles/A01/data/Synoptic.csv", header = TRUE)

# 03. Plot Khorasan:
iran_map <- ggplot() + 
    theme_bw() +
    geom_polygon(data = khorasan,
                 mapping = aes(x = long, y = lat, group = group),
                 color = 'black', fill = 'gray75', size = .2) +
    geom_point(data = infoStations, 
               mapping = aes(x = Longitude, y = Latitude),
               size = 3) +
    geom_text(data = infoStations,
              mapping = aes(x = Longitude, y = Latitude, label = rownames(infoStations)),
              hjust = 0,
              nudge_x = 0.1,
              nudge_y = 0.1) +
    coord_map() +
    xlab(label = "") +
    ylab(label = "") +
    theme(text = element_text(size = 18))
    

ggsave(filename = "_01_Khorasan_Station.png", 
       plot = iran_map, 
       width = 6, 
       height = 6,
       units = "in",
       dpi = 1200,
       path = "./Articles/A01/result/")

# 04. Plot Iran:
iran_map <- ggplot() +
    theme_bw() + 
    geom_polygon(data = iran,
                 mapping = aes(x = long, y = lat, group = group),
                 color = 'black', fill = 'white', size = .2) +
    geom_polygon(data = sea,
                 mapping = aes(x = long, y = lat, group = group),
                 color = 'steelblue1', fill = 'steelblue1', size = .2) +
    geom_polygon(data = khorasan,
                 mapping = aes(x = long, y = lat, group = group),
                 color = 'black', fill = 'gray25', size = .2) +
    coord_map() +
    xlab(label = "") +
    ylab(label = "") +
    theme(text = element_text(size = 18)) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())

ggsave(filename = "_02_Iran_Map.png", 
       plot = iran_map, 
       width = 6, 
       height = 6,
       units = "in",
       dpi = 1200,
       path = "./Articles/A01/result/")

