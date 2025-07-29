if(!require("raster")) install.packages("raster")
if(!require("giscoR")) install.packages("giscoR")

library(sf)
library(terra)
library(giscoR)
library(dplyr)

# Import des differentes couches
france <- giscoR::gisco_get_countries(resolution = "01", 
                                      country = "France") %>%
  sf::st_transform(2154)
bretagne <- giscoR::gisco_get_nuts(resolution = "01", 
                                   country = "France", 
                                   nuts_level = 2) %>%
  subset(NUTS_NAME == "Bretagne") %>%
  sf::st_transform(2154)
france_1x1 <- st_read(paste0(wd$data, 
                             "masques/France/fr_1km.shp")) %>%
  sf::st_transform(2154)

# Transformation en bretagne
france_1x1_bret <- france_1x1[bretagne, ]

# Avoir juste la côte
france_coast <- sf::st_cast(france, "MULTILINESTRING")

# Calcule la distance de chaque centroid au littoral
france_1x1_bret$centroid <- sf::st_centroid(france_1x1_bret$geometry)
france_1x1_bret$distance <- as.numeric(sf::st_distance(
  france_1x1_bret$centroid, 
  france_coast)) / 1000

# Rasterise
coords <- sf::st_coordinates(france_1x1_bret$centroid)
vals <- france_1x1_bret$distance

# Crée un SpatVector
points_sf <- sf::st_sf(distance = vals, 
                       geometry = sf::st_sfc(lapply(1:nrow(coords), 
                                                    function(i) st_point(coords[i,]))), 
                       crs = 2154)
points_sv <- terra::vect(points_sf)

# Fond du raster
ext <- terra::ext(points_sv)
r <- terra::rast(ext, 
                 resolution = 1000, 
                 crs = "EPSG:2154")
?rast
# Rasterise
distance_raster <- terra::rasterize(points_sv, 
                                    r, 
                                    field = "distance", 
                                    fun = "mean")

# Export
plot(distance_raster)
writeRaster(distance_raster, 
            "Distance_Littoral.tif", 
            overwrite = TRUE)
