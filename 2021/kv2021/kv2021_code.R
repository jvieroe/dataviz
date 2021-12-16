rm(list = ls())

library(tidyverse)
library(sf)
library(nngeo)
#library(devtools)
#devtools::install_github("jvieroe/voters")
library(voters)
library(tmap)

tmap_mode("view")

# ----- polling stations
ps <- getPS(spatial = TRUE,
            id = TRUE)


# ----- geo data
url <- "https://api.dataforsyningen.dk/kommuner?format=geojson"
kommuner <- read_sf(url)

kommuner <- kommuner %>% 
  dplyr::select(c(regionsnavn,
                  navn))

match_ps <- st_join(ps,
                    kommuner,
                    join = st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  dplyr::select(c(navn,
                  regionsnavn,
                  dagi_id,
                  ps_id))


rm(ps, kommuner)

vs <- rio::import("2021/kv2021/Valgdata_valgsteder.csv")

vs <- vs %>% 
  left_join(.,
            match_ps,
            by = c("ValgstedId" = "ps_id"))


test <- vs %>% 
  filter(is.na(navn))






