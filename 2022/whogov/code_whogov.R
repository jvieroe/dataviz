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

b1 <- cities[[1]]


b1$navn
b1$kommune$kode
b1$kommune$navn
b1$postnumre


zips <- b1$postnumre
zips

zips <- b1$postnumre[[1]]
zips
length(b1$postnumre)

zips$nr
zips$navn

rm(coords)
coords <- b1$visueltcenter[[1]]
coords

rm(b1, zips,
   coords)

extract_func <- function(input, name, municipality, cooords, df) {
  
  name <- input$navn
  
  municipality <- tibble(muni_code = input$kommune$kode,
                         muni_name = input$kommune$navn)
  
  coords <- tibble(lat = input$visueltcenter[[2]],
                   lon = input$visueltcenter[[1]])
  
  # if (length(input$postnumre == 3)) {
  #   
  #   temp <- input$postnumre[[1]]
  #   
  #   zips <- tibble(zip_code = temp$nr,
  #                  zip_name = temp$navn)
  #   
  # } else if (length(input$postnumre != 3)) {
  #   
  #   zips <- tibble(zip_code = NA,
  #                  zip_name = NA)
  #   
  # }
  
  df <- cbind(municipality,
              coords) %>% 
    tibble() %>% 
    mutate(city_name = name)
  
  return(df)
  
  rm(name, municipality, coords, df)
  
}



c1 <- cities[[1]]
c2 <- cities[[2]]

temp_list <- list(c1, c2)

city_list <- map(cities,
                 extract_func)


city_df <- bind_rows(city_list)

city_df
