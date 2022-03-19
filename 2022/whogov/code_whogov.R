library(tidyverse)
library(sf)
library(janitor)
library(ggtext)

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

library(httr)
byer <- httr::GET(url)

byer <- content(byer)

b1 <- byer[[1]]

b1$navn
b1$kommune$kode
b1$kommune$navn


extract_func <- function(input, name, municipality, cooords) {
  
  name <- input$navn
  
  muni_code <- input$kommune$kode
  muni_name <- input$kommune$navn
  
  
  
}




