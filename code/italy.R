# https://www.envidat.ch/#/metadata/digitizing-historical-plague

# -------------------------------------------------------------
# preliminaries
# -------------------------------------------------------------

#rm(list = ls())
#devtools::install_github("ropensci/rnaturalearthhires") 

library(rio)
library(haven)
library(tidyverse)
library(magrittr)
library(janitor)
library(sf)
library(nngeo)
library(tmap)
library(rnaturalearth)
library(rnaturalearthhires)

#tmap_mode("view")

# -------------------------------------------------------------
# prepare data
# -------------------------------------------------------------

# raw <- read_dta("data/anatem.dta",
#                  encoding = "latin1") %>% 
#   tibble()

city <- raw %>% 
  filter(country == "italy") %>% 
  #filter(source == "Malanima") %>% 
  filter(!is.na(source)) %>% 
  filter(source != "") %>% 
  filter(!is.na(latitude) & !is.na(longitude) & !is.na(city_pop)) %>% 
  select(c(city, country, latitude, longitude, year, city_pop, source)) %>% 
  group_by(city, country, latitude, longitude) %>%
  mutate(city_id = cur_group_id()) %>%
  ungroup() %>%
  arrange(city_id, year) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(5643) %>% 
  filter(year %in% seq(1300, 1400, 1)) %>% 
  group_by(city_id) %>% 
  mutate(tally = n()) %>% 
  ungroup() %>% 
  filter(tally >= 2)
  

italy <- ne_countries(scale = "large") %>% 
  st_as_sf() %>% 
  st_transform(crs = 5643) %>% 
  filter(sovereignt == "Italy") %>% 
  select(sovereignt)


dim_m <- 50000

sf_grid <- italy %>% 
  st_make_grid(.,
               cellsize = c(dim_m, dim_m),
               square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(grid_id = row_number()) %>% 
  select(grid_id)

sf_grid <- sf_grid %>% 
  st_intersection(italy)





tmp <- city %>% 
  distinct(., city_id,
           .keep_all = TRUE) %>% 
  st_join(., sf_grid,
          join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  select(city_id,
         grid_id)





city <- city %>% 
  left_join(., tmp,
            by = "city_id")

city_summarized <- city %>% 
  arrange(grid_id, year) %>% 
  group_by(grid_id, year) %>%
  summarize(grid_pop = sum(city_pop)) %>% 
  mutate(lpop = lag(grid_pop, 1)) %>% 
  ungroup() %>% 
  mutate(delta = grid_pop - lpop) %>% 
  mutate(pct = 100*(delta/lpop)) %>% 
  filter(!is.na(pct)) %>% 
  st_drop_geometry()

sf_grid <- sf_grid %>% 
  left_join(.,
            city_summarized,
            by = "grid_id")


ggplot() + 
  geom_sf(data = sf_grid, aes(fill = pct),
          color = "white",
          size = .1) +
  scale_fill_gradient(low = "black",
                      high = "deeppink4") +
  geom_sf(data = italy,
          fill = "transparent") +
  theme_void()




ggplot() + 
  geom_sf(data = italy) +
  geom_sf(data = city)


ggplot() + 
  geom_sf(data = sf_grid, aes(fill = grid_id),
          color = "white",
          size = .1) +
  geom_sf(data = italy,
          fill = "transparent")
