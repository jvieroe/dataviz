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

ggp <- ggplot() +
  geom_sf(data = osm_lines,
          color = col_fg) +
  theme_void() +
  labs(caption = "<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> jvieroe<br>
                              <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> jvieroe") +
  theme(panel.background = element_rect(color = col_sand, fill = col_sand),
        plot.background = element_rect(color = col_sand, fill = col_sand),
        plot.caption = element_markdown(color = col_fg))

ggp
  
ggdraw(ggp) +
  draw_label(label = "Jyderup",
             x = 0.190, y = 0.90,
             size = 40,
             fontfamily = font,
             color = col_fg) +
  draw_label(label = "55°39′36.14″ N, 11°23′55.03″ E",
             x = 0.190, y = 0.815,
             size = 12.5,
             fontfamily = font,
             color = col_fg)

ggsave(plot = last_plot(),
       filename = "2021/b-town/plot.png",
       dpi = 1000, scale = 1, width = 7, height = 7, units = c("in"))




# ggplot() +
#   geom_sf(data = osm_lines,
#           color = "dodgerblue4") +
#   labs(title = "This font is awesome:
#        <span style='font-family: \"Font Awesome 5 Free Solid\"'>&#62766; &#62650; &#62577;</span>",
#        subtitle = "This font is awesome:
#        <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#62766; &#62650; &#62577;</span>",
#        caption = "f099") +
#   theme_void() +
#   theme(plot.title = ggtext::element_markdown(),
#         plot.subtitle = ggtext::element_markdown(),
#         plot.caption = ggtext::element_markdown(family = "Font Awesome 5 Brands Regular"),
#         panel.background = element_rect(color = col_sand, fill = col_sand),
#         plot.background = element_rect(color = col_sand, fill = col_sand))
# 
# 
# ggplot(NULL, aes(0, 0)) +
#   geom_text(
#     aes(label = "clock"),
#     size = 50, family = "Font Awesome 5 Free"
#   )
# 
# 
# ggplot(NULL, aes(0, 0)) +
#   geom_text(
#     aes(label = "twitter"),
#     size = 50, family = "Font Awesome 5 Brands"
#   )
# 
# 
# 
# systemfonts::system_fonts() %>% 
#   filter(str_detect(family, "Font Awesome 5")) %>% 
#   transmute(
#     family, style,
#     file = stringr::str_extract(path, "[\\w-]+\\.ttf$")
#   )




