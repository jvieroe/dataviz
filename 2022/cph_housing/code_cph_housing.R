library(tidyverse)
library(janitor)
library(httr)
library(sf)
library(tmap)

tmap_mode("view")



# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------

# ----- Map of Danish municipalities
muni_url <- "https://api.dataforsyningen.dk/kommuner/?format=geojson"
municipality <- read_sf(muni_url)

st_crs(municipality)

municipality <- municipality %>% 
  st_transform(crs = 4326)




# ----- Housing in Copenhagen
building_url <- "https://api.dataforsyningen.dk/bbrlight/bygninger?kommunekode=0101|0147"

buildings <- httr::GET(building_url)
http_status(buildings)

buildings <- content(buildings)


# Test inspection
test <- buildings[[100]]
test$OPFOERELSE_AAR
test$bygningspunkt$koordinater[[1]]
test$bygningspunkt$koordinater[[2]]

rm(test)

fun_year <- function(input, year) {
  
  year <- input$OPFOERELSE_AAR
  
  return(year)
  
}


fun_coords <- function(input, coords) {
  
  coords <- tibble(
    lon = input$bygningspunkt$koordinater[[1]],
    lat = input$bygningspunkt$koordinater[[2]],
  )
  
  return(coords)
  
}



year <- map_int(.x = buildings, .f = fun_year)

df_year <- tibble(
  id = 1:length(year),
  year = year
)


coords <- map_dfr(.x = buildings,
                  .f = fun_coords)

coords <- coords %>% 
  mutate(id = 1:length(buildings))



df <- coords %>% 
  tidylog::left_join(.,
                     df_year,
                     by = "id")



df_sf <- df %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

df_sf <- df_sf %>% 
  st_crop(municipality)

tm_shape(df_sf) +
  tm_dots(size = 0.00001)


hist(df$year)



df_sf <- df_sf %>% 
  mutate(year = ifelse(year > 0,
                       year,
                       NA))

hist(df_sf$year)

df_sf <- df_sf %>% 
  mutate(year_cat = case_when(year < 1900 ~ "< 1900",
                              year %in%  seq(1900, 1950, 1) ~ "1900-1950",
                              year %in%  seq(1951, 2000, 1) ~ "1951-2000",
                              year > 2000 ~ "> 2000",))

tabyl(is.na(df_sf$year))
tabyl(is.na(df_sf$year_cat))


library(rnaturalearthdata)
library(rnaturalearth)

base_map <- rnaturalearth::ne_countries(scale = "large") %>% 
  st_as_sf()

base_map <- base_map %>% 
  filter(admin == "Denmark")

ggplot() +
  geom_sf(data = base_map)


cph <- municipality %>% 
  filter(navn %in% c("København", "Frederiksberg"))

muni <- municipality %>% 
  st_drop_geometry()

ggplot() +
  geom_sf(data = df_sf, aes(color = year_cat),
          size = .01, alpha = .5) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(size = 10)))


ggsave(plot = last_plot(),
       "2022/cph_housing/plot_cph_housing.png")
