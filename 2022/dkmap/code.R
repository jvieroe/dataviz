library(tidyverse)
library(sf)
library(osmdata)
library(osmextract)
library(tmap)

# https://docs.ropensci.org/osmextract/

bb_dk <- getbb("Denmark", feature = "country")

osm <- bb_dk %>% 
  opq()

train_st <- osm %>% 
  add_osm_feature(key = 'public_transport', value = 'station') %>% 
  osmdata_sf()


train_st <- bb_dk %>%
  opq(timeout = 20) %>% 
  add_osm_feature(key = 'public_transport', value = 'station') %>% 
  osmdata_sf()


cycleways_england = opq("England") %>% 
  add_osm_feature(key = "highway", value = "cycleway") %>% 
  osmdata_sf()


cycleways_england = oe_get(
  "England",
  quiet = FALSE,
  query = "SELECT * FROM 'lines' WHERE highway = 'cycleway'"
)


