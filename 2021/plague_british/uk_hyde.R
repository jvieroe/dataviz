# -------------------------------------------------------------
# preliminaries
# -------------------------------------------------------------

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



# ----- raster data
hyde_list <- list()
hyde_list[[1]] <- raster("data/1300AD_pop/popc_1300AD.asc")
hyde_list[[2]] <- raster("data/1400AD_pop/popc_1400AD.asc")

hyde_fun <- function(hyde, hyde_fixed) {
  
  hyde <- hyde
  crs(hyde) <- CRS('+init=EPSG:4326')
  hyde <- crop(hyde, extent(grid_sf))
  hyde <- mask(hyde, grid_sf)
  
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
  labs(title = "The Black Death on the \nBritish Isles",
       subtitle = "Net population loss (k), 1300 - 1400",
       caption = "Graphics: Jeppe Vierø (@Vieroe)\nData: HYDE 3.2") +
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
        plot.margin = grid::unit(c(t = 0, r = 20, b = 5, l = 5), "mm")) +
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
       dpi = 320, scale = 1, width = 4.5, height = 6, units = c("in"))




# https://www.envidat.ch/#/metadata/digitizing-historical-plague
