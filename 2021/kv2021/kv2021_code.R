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

match_ps <- match_ps %>% 
  left_join(.,
            index,
            by = "dagi_id")








vs <- rio::import("2021/kv2021/Valgdata_valgsteder.csv")



index <- ps %>% 
  st_drop_geometry() %>% 
  dplyr::select(c(dagi_id,
           ps_id))




table(is.na(match_ps$ps_id.y))

rm(kommuner, ps)





