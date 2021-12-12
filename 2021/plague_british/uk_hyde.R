# https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
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
library(nngeo)
library(tmap)
library(rnaturalearth)
library(rnaturalearthhires)
library(raster)
library(sysfonts)
library(showtext)
library(svglite)

#tmap_mode("view")

# -------------------------------------------------------------
# load data
# -------------------------------------------------------------

# ----- map data
map_sf <- ne_countries(scale = "large") %>% 
  st_as_sf() %>% 
  #st_transform(crs = 5643) %>% 
  filter(admin == "United Kingdom" | admin == "Ireland") %>% 
  #filter(admin == "Italy") %>% 
  dplyr::select(admin)

ggplot() + 
  geom_sf(data = map_sf)

# class(map_sf$geometry)
# 
# map_sf <- map_sf %>%
#   st_simplify(dTolerance = 2500) %>% 
#   st_as_sf() %>% 
#   st_cast("MULTIPOLYGON")
# 
# class(map_sf$geometry)
# 
# ggplot() + 
#   geom_sf(data = map_sf)


# ----- x



# ----- grid data
dim_m <- 50000

map_sf <- map_sf %>% 
  st_transform(crs = 5643)

grid_sf <- map_sf %>% 
  st_make_grid(.,
               cellsize = c(dim_m, dim_m),
               square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(grid_id = row_number())

grid_sf <- grid_sf %>% 
  st_intersection(map_sf)

map_sf <- map_sf %>% 
  st_transform(crs = 4326)

grid_sf <- grid_sf %>% 
  st_transform(crs = 4326)

ggplot() + 
  geom_sf(data = grid_sf, aes(fill = grid_id),
          color = "white",
          size = .1) +
  geom_sf(data = map_sf,
          fill = "transparent")

st_crs(grid_sf)
st_crs(map_sf)



# ----- raster data
hyde_list <- list()
hyde_list[[1]] <- raster("data/1300AD_pop/popc_1300AD.asc")
hyde_list[[2]] <- raster("data/1400AD_pop/popc_1400AD.asc")

hyde_fun <- function(hyde, hyde_fixed) {
  
  hyde <- hyde
  crs(hyde) <- CRS('+init=EPSG:4326')
  hyde <- crop(hyde, extent(grid_sf))
  hyde <- mask(hyde, grid_sf)
  
  #hyde_fixed <- projectRaster(hyde, crs = CRS('+init=EPSG:5643'))
  
  hyde_fixed <- extract(hyde, grid_sf)
  
  return(hyde_fixed)
  
}


fix_list <- map(hyde_list,
                hyde_fun)



hyde13 <- fix_list[[1]]
hyde14 <- fix_list[[2]]


grid_sf$hyde13 <- unlist(lapply(hyde13,
                                function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))
grid_sf$hyde14 <- unlist(lapply(hyde14,
                                function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))



grid_sf <- grid_sf %>% 
  mutate(across(starts_with("hyde"),
                ~ .x / 1000,
                .names = "{.col}_k"))

grid_sf <- grid_sf %>% 
  mutate(delta = hyde13_k - hyde14_k) %>% 
  mutate(pct = ifelse(delta == 0,
                      0,
                      (delta/hyde13)*100))



#font_add_google("Roboto", "roboto")


ggplot() + 
  geom_sf(data = grid_sf, aes(fill = delta),
          color = "white",
          size = .1) +
  scale_fill_viridis_c(option = "inferno",
                       direction = -1) +
  geom_sf(data = map_sf,
          color = "white",
          size = .01,
          fill = "transparent") +
  labs(title = "The Black Death on the British Isles",
       subtitle = "Net population loss (thousands), 1300 - 1400",
       caption = "Data: HYDE 3.2\nGraphics: Jeppe Vierø (@Vieroe)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(color = "white",
                                  size = 18,
                             family = "Cinzel"),
        plot.subtitle = element_text(color = "white",
                                     size = 10,
                                     family = "Cinzel"),
        plot.caption = element_text(color = "white",
                                    size = 7,
                                    family = "Cinzel",
                                    vjust = -5,
                                    hjust = 0),
        panel.background = element_rect(fill = "gray15",
                                        color = NA),
        plot.background = element_rect(fill = "gray15",
                                       color = NA),
        plot.margin = grid::unit(c(t = 0, r = 15, b = 5, l = 5), "mm")) +
  guides(fill = guide_colorbar(barwidth = 15,
                               barheight = .5,
                               ticks = FALSE,
                               title = "",
                               frame.colour = "white",
                               label.theme = element_text(color = "white",
                                                          family = "Cinzel",
                                                          size = 7)))

ggsave(plot = last_plot(),
       filename = "2021/plague_british/black-death_british.png",
       dpi = 320, scale = 1, width = 4, height = 6, units = c("in"))
# knitr::plot_crop("2021/plague_british/black-death_british.png",
#                  quiet = T)


ggsave(plot = last_plot(),
       filename = "2021/plague_british/black-death_british.pdf")

ggsave(plot = last_plot(),
       filename = "2021/plague_british/black-death_british.svg")








