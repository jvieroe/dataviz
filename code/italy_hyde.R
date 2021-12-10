library(raster)

hyde13 <- raster("data/1300AD_pop/urbc_1300AD.asc")
hyde14 <- raster("data/1400AD_pop/urbc_1400AD.asc")

hyde_list <- list()
hyde_list[[1]] <- hyde13
hyde_list[[2]] <- hyde14

temp <- italy %>% 
  st_transform(4326)

hyde_fun <- function(hyde, hyde_fixed) {
  
  hyde_fixed <- hyde
  crs(hyde_fixed) <- CRS('+init=EPSG:4326')
  hyde_fixed <- crop(hyde_fixed, extent(temp))
  hyde_fixed <- mask(hyde_fixed, temp)
  
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

h13 <- extract(hyde13, sf_grid)

class(sf_grid$x)

