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


italy <- city %>% 
  filter(country == "italy") %>% 
  filter(year > 1200) %>%
  filter(year < 1600) %>%
  filter(source == "Malanima")
  filter(intpl_length < 20)

tabyl(italy$source)

tabyl(italy$year)

nrow(city)/1000





europe <- ne_countries(scale = "large") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

ggplot() + 
  geom_sf(data = europe) +
  geom_sf(data = pd_sf) +
  coord_sf(xlim = c(-11, 39),
           ylim = c(28, 62)) +
  theme_void()





