library(tidyverse)
library(sf)
library(osmdata)
library(magrittr)
library(cowplot)
library(ggtext)

temp_bbox <- c(11.38, 55.65,
               11.43, 55.68)

osm <- opq(bbox = temp_bbox)

osm_highways <- osm %>% 
  add_osm_feature(key = 'highway') %>%
  osmdata_sf ()

osm_lines <- osm_highways$osm_lines

col_sand <- "#EFEBDD"
col_fg <- "dodgerblue4"
font <- "Oswald"

ggplot() +
  geom_sf(data = osm_lines,
          color = col_fg) +
  theme_void() +
  labs(caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> jvieroe<br>
                              <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> jvieroe",
       title = "Jyderup",
       subtitle = "55°39′36.14″ N, 11°23′55.03″ E") +
  theme(panel.background = element_rect(color = col_sand, fill = col_sand),
        plot.background = element_rect(color = col_sand, fill = col_sand),
        plot.caption = element_markdown(color = col_fg, family = font,
                                        margin = margin(t = 0.5, r = -0.5, b = 0.1, l = 0.0, unit = "cm")),
        plot.title = element_text(color = col_fg, family = font, size = 40,
                                  margin = margin(t = 0.5, r = 0.5, b = 0, l = 0.0, unit = "cm")),
        plot.subtitle = element_text(color = col_fg, family = font, size = 12.5))


ggsave(plot = last_plot(),
       filename = "2021/b-town/plot.png",
       dpi = 1000, scale = 1, width = 7, height = 7.5, units = c("in"))




