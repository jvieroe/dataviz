# https://www.envidat.ch/#/metadata/digitizing-historical-plague

# -------------------------------------------------------------
# preliminaries
# -------------------------------------------------------------

library(rio)
library(tidyverse)
library(magrittr)
library(janitor)
library(sf)
library(tmap)
library(rnaturalearth)

tmap_mode("view")

# -------------------------------------------------------------
# prepare data
# -------------------------------------------------------------

# pd <- read_lines("data/plague_outbreaks.txt") %>% 
#   tibble() %>% 
#   separate('.',
#            into = c("location", "x", "y", "year", "lat", "lon"),
#            convert = TRUE)

pd <- rio::import("data/plague_outbreaks.txt") %>%
  tibble() %>% 
  clean_names()

tmp <- pd %>% 
  filter(location == "Eisleben") %>% 
  mutate(year_of_outbreak = lat_wgs84) %>% 
  mutate(lat_wgs84 = 51.524881,
         lon_wgs84 = 11.550808)

pd <- pd %>%
  filter(location != "Eisleben") %>% 
  rbind(tmp) ; rm(tmp)

# pd <- pd %>% 
#   group_by(location, latitude, longitude) %>%
#   mutate(id = cur_group_id()) %>%
#   ungroup() %>%
#   arrange(location, year)


pd <- pd %>% 
  st_as_sf(., coords = c("lat_wgs84", "lon_wgs84"),
           crs = 4326)


europe <- ne_countries() %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  filter(region_un == "Europe")



ggplot() + 
  geom_sf(data = europe) +
  geom_sf(data = pd)
