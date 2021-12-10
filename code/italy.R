# https://www.envidat.ch/#/metadata/digitizing-historical-plague

# -------------------------------------------------------------
# preliminaries
# -------------------------------------------------------------

rm(list = ls())
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

tmap_mode("view")

# -------------------------------------------------------------
# prepare data
# -------------------------------------------------------------

city <- read_dta("data/anatem.dta",
                 encoding = "latin1")

city <- city %>% 
  tibble()


city_italy <- city %>% 
  filter(country == "italy") %>% 
  filter(source == "Malanima") %>% 
  filter(!is.na(latitude) & !is.na(longitude) & !is.na(city_pop)) %>% 
  select(c(city, country, latitude, longitude, year, city_pop)) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

italy <- ne_countries(scale = "large") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  filter(sovereignt == "Italy")

ggplot() + 
  geom_sf(data = italy)

dim_m <- 50000

sf_grid <- italy %>% 
  st_transform(5643) %>% 
  st_make_grid(.,
               cellsize = c(dim_m, dim_m),
               square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(grid_id = row_number())

italy_new <- italy %>% 
  st_transform(5643)

sf_grid <- sf_grid %>% 
  st_intersection(italy_new)

ggplot() + 
  geom_sf(data = sf_grid, aes(fill = grid_id),
          color = "white",
          size = .1) +
  geom_sf(data = italy_new,
          fill = "transparent")


city_list <- split(city_italy,
                   f = city_italy$year)


merge_list <- map(.x = city_list,
                  .f = function(data) {
                    
                    
                    
                  })


tmp <- city_italy %>% filter(year == 1300) %>% 
  st_transform(5643)

run <- st_join(tmp,
               sf_grid)




