if(!require("raster")) install.packages("raster")
if(!require("giscoR")) install.packages("giscoR")

#packages
library(giscoR)
library(sf)
library(raster)
library(RColorBrewer)

Bretagne <- giscoR::gisco_get_nuts(resolution = "01", 
                                   country = "France", 
                                   nuts_level = 2) %>%
  subset(NUTS_NAME == "Bretagne") %>%
  sf::st_transform(3055)

france_1x1 <- sf::st_read(
  paste0(wd$data, "masques/France/fr_1km.shp")
)


france <- sf::st_transform(france, 3055) # 3055 pour distance en metres

france_1x1 <- france_1x1%>%
  sf::st_transform(3055)%>%
  sf::st_intersection(france)

france_1x1_bret <- france_1x1[Bretagne, ]

distance_bret <- sf::st_distance(france, france_1x1_bret)

france_1x1_bret <- france_1x1_bret %>%
  mutate(distance = as.vector(distance_bret)/1000) # En km

### Plot : Verif
col_dist <- RColorBrewer::brewer.pal(11, "RdGy")
ggplot() +
  geom_sf(data = RegionBretagneConti) +
  labs(title = "Distance du littoral en Bretagne")+
  geom_sf(data = france_1x1_bret, aes(fill = distance), lwd = 0) +
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  theme_bw()

# Export sous forme Raster

ext <- raster::extent(as(france_1x1_bret, "Spatial"))

r <- raster:: raster(resolution = 1000, ext = ext,
                     crs = "+proj=utm +zone=27 +ellps=intl 
                     +towgs84=-73,47,-83,0,0,0,0 +units=m +no_defs")

distance_sf <- sf::st_as_sf(france_1x1_bret, 
                            coords=c("X_1km", "Y_1km")) %>%
  sf::st_set_crs(3055)

distance_raster <- rasterize(distance_sf, r, "distance", fun = mean)
distance_raster
plot(distance_raster)

terra::writeRaster(distance_raster, 
                   file = paste0(wd$data,
                                 "masques/VariablesStructurates/Distance_Littoral.tif"),
                   overwrite = TRUE)

bdd <- terra::rast(paste0(wd$data,
                          "masques/VariablesStructurates/Distance_Littoral.tif"))
terra::crs(bdd) <- "EPSG:2154"

plot(bdd) 
