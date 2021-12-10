# https://www.envidat.ch/#/metadata/digitizing-historical-plague

# -------------------------------------------------------------
# preliminaries
# -------------------------------------------------------------

#devtools::install_github("ropensci/rnaturalearthhires") 

library(rio)
library(tidyverse)
library(magrittr)
library(janitor)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthhires)

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
  tibble()



  clean_names() %>% 
  select(-location)

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



getwd()
pd <- rio::import("2021/Day1/Historical_plague_outbreaks.txt") %>%
  tibble()

pd <- pd %>%
  rename(latitude = Lon_WGS84,
         longitude = Lat_WGS84,
         year = Year_of_Outbreak,
         location = Location) %>%
  select(-c(X_Coord_ERTS89,
            Y_Coord_ERTS89))

eisleben <- pd[pd$location == "Eisleben",]
eisleben$year[1] <- 1398
eisleben$year[2] <- 1681
eisleben$longitude <- 11.550808
eisleben$latitude <- 51.524881

pd <- pd %>%
  filter(location != "Eisleben") %>%
  rbind(eisleben) %>%
  group_by(location, latitude, longitude) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  arrange(location, year) ; rm(eisleben)

pd <- pd %>% 
  select(-location)


pd_sf <- pd %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

tm_shape(pd_sf) +
  tm_dots()


europe <- ne_countries(scale = "large") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

ggplot() + 
  geom_sf(data = europe) +
  geom_sf(data = pd_sf) +
  coord_sf(xlim = c(-11, 39),
           ylim = c(28, 62)) +
  theme_void()





