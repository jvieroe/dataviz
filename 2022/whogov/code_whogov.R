library(tidyverse)
library(sf)
library(janitor)
library(ggtext)
library(httr)
library(tmap)

tmap_mode("view")

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



# --------------- load cities data ---------------
url <- "https://api.dataforsyningen.dk/supplerendebynavne2"

cities <- httr::GET(url) %>% 
  content(.)


extract_func <- function(input, name, municipality, cooords, df) {
  
  name <- input$navn
  
  municipality <- tibble(muni_code = input$kommune$kode,
                         muni_name = input$kommune$navn)
  
  coords <- tibble(lat = input$visueltcenter[[2]],
                   lon = input$visueltcenter[[1]])
  
  df <- cbind(municipality,
              coords) %>% 
    tibble() %>% 
    mutate(city_name = name)
  
  return(df)
  
  rm(name, municipality, coords, df)
  
}




city_df <- map_dfr(cities,
                   extract_func)


temp <- city_df %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

tm_shape(temp) +
  tm_dots()

city_df %>% filter(muni_name == "Gentofte")
city_df %>% filter(city == "Aarhus")

city_df <- city_df %>% 
  mutate(city = city_name) %>% 
  mutate(city = ifelse(muni_code == "0101",
                       "København",
                       city)) %>% 
  mutate(city = ifelse(muni_code == "0751",
                       "Aarhus",
                       city)) %>% 
  mutate(city = ifelse(muni_code == "0751",
                       "Aarhus",
                       city))

city_df <- city_df %>% 
  arrange(city)


# --------------- merge data ---------------
locations <- whogov %>% 
  filter(!is.na(placeofbirth)) %>% 
  select(placeofbirth)


locations %>% filter(placeofbirth == "Aarhus")

locations %>% 
  filter(!placeofbirth %in% city_df$city) %>% 
  filter(!placeofbirth %in% city_df$muni_name)


temp <- city_df %>% 
  mutate(muni_code = as.numeric(muni_code))

city_df %>% filter(muni_code == "0101")

