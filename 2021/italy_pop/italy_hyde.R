library(raster)

italy <- ne_countries(scale = "large") %>% 
  st_as_sf() %>% 
  st_transform(crs = 5643) %>% 
  filter(sovereignt == "Italy") %>% 
  dplyr::select(sovereignt)

temp <- italy %>% 
  st_transform(4326)

hyde13 <- raster("data/1300AD_pop/urbc_1300AD.asc")
hyde14 <- raster("data/1400AD_pop/urbc_1400AD.asc")

hyde_list <- list()
hyde_list[[1]] <- hyde13
hyde_list[[2]] <- hyde14



hyde_fun <- function(hyde, hyde_fixed) {
  
  hyde <- hyde
  crs(hyde) <- CRS('+init=EPSG:4326')
  hyde <- crop(hyde, extent(temp))
  hyde <- mask(hyde, temp)
  
  hyde_fixed <- extract(hyde, sf_grid)
  
  return(hyde_fixed)
  
}


fix_list <- map(hyde_list,
                hyde_fun)

hyde13 <- fix_list[[1]]
hyde14 <- fix_list[[2]]

tmap_mode("plot")

tm_shape(hyde14) +
  tm_raster()

sf_grid <- sf_grid %>% 
  st_transform(4326)

sf_grid <- sf_grid %>% 
  st_intersection(italy)



sf_grid$hyde13 <- unlist(lapply(hyde13,
                                function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))
sf_grid$hyde14 <- unlist(lapply(hyde14,
                                function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))




ggplot() + 
  geom_sf(data = sf_grid, aes(fill = hyde13),
          color = "white",
          size = .1) +
  scale_fill_gradient(low = "black",
                      high = "deeppink4") +
  geom_sf(data = italy,
          fill = "transparent") +
  theme_void()


# match raster data to polygons
pop_data <- extract(raster, ne)

# calculate sums by polygon
ne$hyde <- unlist(lapply(pop_data,
                         function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))

df <- ne %>% 
  as.data.frame()

return(df)

