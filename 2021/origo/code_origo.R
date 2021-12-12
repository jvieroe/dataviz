library(tidyverse)
library(magrittr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)


origo <- tribble(~lat, ~lon,
                 0, 0) %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

map_sf <- ne_countries(scale = "large") %>% 
  st_as_sf() %>% 
  filter(admin != "Antarctica") %>% 
  st_union()
  #select(admin)

ggplot(data = map_sf) +
  geom_sf()

grid_sf <- map_sf %>% 
  st_make_grid(.,
               n = c(180, 90),
               square = TRUE) %>% 
  st_as_sf() %>% 
  mutate(grid_id = row_number())

grid_sf <- grid_sf %>%  
  st_intersection(map_sf)

grid_sf2 <- grid_sf %>% st_make_valid()

centroids <- grid_sf2 %>% 
  st_centroid()

dist <- st_distance(centroids,
                    origo)

grid_sf <- grid_sf %>% 
  mutate(dist = dist) %>% 
  mutate(dist = parse_number(as.character(dist)))

grid_sf <- grid_sf %>% 
  select(-admin)

ggplot() +
  geom_sf(data = grid_sf, aes(fill = dist),
          color = "white", size = .1) +
  scale_fill_viridis_c(direction = -1)





