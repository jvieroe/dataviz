library(tidyverse)
library(sf)
library(tmap)
library(ggtext)

tmap_mode("view")


ming <- read_sf(dsn = "2022/ming_routes/data/Ming_Routes_2016",
                layer = "Ming_Routes_2016")

maps <- read_sf(dsn = "2022/ming_routes/data/ne_10m_admin_0_countries",
                layer = "ne_10m_admin_0_countries")

maps <- maps %>%  
  filter(SOVEREIGNT != "Antarctica")

latitude <- 32
longitude <- 90

ortho <- paste0('+proj=ortho +lat_0=', latitude, ' +lon_0=', longitude,
                ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')


maps_reproj <- maps %>% 
  st_cast('MULTIPOLYGON') %>%
  st_cast('POLYGON', do_split = TRUE) %>%
  st_transform(crs = ortho)

ming_reproj <- ming %>% 
  st_transform(crs = ortho)

maps_union <- maps_reproj %>% 
  st_union()

china <- maps_reproj %>% 
  filter(SOVEREIGNT == "China")

st_bbox(china)

ggplot() +
  geom_sf(data = maps_reproj, size = 0.2, color = "#06216E") +
  geom_sf(data = ming_reproj, size = 0.2, color = "#06216E") +
  coord_sf(xlim = c(-1253835, 4272399),
           ylim = c(-1760275, 2876833)) +
  theme_minimal()




ggplot() +
  geom_sf(data = maps, color = "white", fill = "gray15",
          size = .2) +
  coord_sf(crs = crs)

ggplot() +
  geom_sf(data = maps, color = "white", fill = "gray15",
          size = .2) +
  geom_sf(data = ming, color = "chartreuse2", size = 1.50, alpha = 0.10) +
  geom_sf(data = ming, color = "chartreuse2", size = 1.25, alpha = 0.25) +
  geom_sf(data = ming, color = "chartreuse2", size = 1.00, alpha = 0.50) +
  geom_sf(data = ming, color = "chartreuse2", size = 0.25, alpha = 0.75) +
  labs(title = "<span style ='font-family:Pacifico'> Ming Dynasty</span>  <span style='color:#C31F48;font-size:20px'>明朝</span>") +
  theme_void() +
  theme(panel.background = element_rect(fill = "gray10",
                                        color = "gray10"),
        plot.background = element_rect(fill = "gray10",
                                       color = "gray10"),
        plot.title = ggtext::element_markdown(color = "white",
                                              family = "Noto Sans TC"))

