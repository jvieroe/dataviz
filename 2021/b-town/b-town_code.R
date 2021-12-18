library(tidyverse)
library(sf)
library(osmdata)
library(magrittr)
library(cowplot)

temp_bbox <- c(11.38, 55.65,
               11.43, 55.68)

osm <- opq(bbox = temp_bbox)

osm_highways <- osm %>% 
  add_osm_feature(key = 'highway') %>%
  osmdata_sf ()

osm_lines <- osm_highways$osm_lines

ggp <- ggplot() +
  geom_sf(data = osm_lines,
          color = "dodgerblue4") +
  theme_void() +
  theme(plot.background = element_rect(color = "#EFEBDD", fill = "#EFEBDD"))
  
ggdraw(ggp) +
  draw_label(label = "Jyderup",
             x = 0.225, y = 0.95,
             size = 40,
             fontfamily = "Corinthia",
             color = "gray20") +
  draw_label(label = "55.66, 11.41",
             x = 0.225, y = 0.865,
             size = 25,
             fontfamily = "Corinthia",
             color = "gray20")




