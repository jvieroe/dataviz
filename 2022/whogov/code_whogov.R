library(tidyverse)
library(sf)
library(janitor)
library(ggtext)
library(httr)

# --------------- load WhoGov data ---------------
# whogov_cs <- readxl::read_xlsx("2022/whogov/data/Denmark.xlsx")
# 
# whogov_ts <- readxl::read_xlsx("2022/whogov/data/Denmark_ts.xlsx")
# 
# 
# whogov_cs <- whogov_cs %>% 
#   select(name, birthyear, deadyear, placeofbirth)
# 
# whogov <- tidylog::left_join(whogov_ts,
#                              whogov_cs,
#                              by = )
# 
# 
# rm(list=ls())


whogov <- readxl::read_xlsx("2022/whogov/data/Denmark.xlsx")

locations <- whogov %>% 
  filter(!is.na(placeofbirth)) %>% 
  pull(placeofbirth) %>% 
  as.character()

locations



url <- "https://api.dataforsyningen.dk/supplerendebynavne2"


cities <- httr::GET(url) %>% 
  content(.)


cities <- cities

# byer <- content(byer)
# 
# b1 <- byer[[1]]
# 
# b1$navn
# b1$kommune$kode
# b1$kommune$navn
# b1$postnumre
# 
# zips <- b1$postnumre[[1]]
# zips$nr
# zips$navn
# 
# rm(coordss)
# coordss <- b1$visueltcenter[[1]]
# coordss

extract_func <- function(input, name, municipality, temp, zips, cooords, df) {
  
  name <- input$navn
  
  municipality <- tibble(muni_code = input$kommune$kode,
                         muni_name = input$kommune$navn)
  
  temp <- input$postnumre[[1]]
  
  zips <- tibble(zip_code = temp$nr,
                 zip_name = temp$navn)
  
  coords <- tibble(lat = input$visueltcenter[[2]],
                   lon = input$visueltcenter[[1]])
  
  df <- cbind(municipality,
              zips,
              coords) %>% 
    tibble() %>% 
    mutate(city_name = name)
  
  return(df)
  
  rm(name, municipality, temp, zips, coords, df)
  
}


city_list <- map(cities,
                 extract_func)



