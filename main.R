library(renv)
renv::repair()

loadedNamespaces()

library(dplyr)
library(sf)
library(ggplot2)

wd <- set_wd()
set.seed(12345)

summary(Total)

Total <- sf::st_read(paste0(wd$data, "derived/TotalComplet.shp"), 
                     options = "ENCODING=UTF8")
Total <- transform_Total()


Total %>%
  dplyr::distinct(cd_nom)%>%
  dplyr::select(nom_vernaculaire, cd_nom)


diro <- st_read(paste0(wd$data,"derived/diro.shp"))

# Total : 160991
# DIR Ouest : 26129

26129/160991

rm(list=setdiff(ls(), "Total"))

#### Nettoyage de donnees ####
# Total:
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Total_sites.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total_sites.html"))

#### Stats Desc ####
rmarkdown::render(paste0(wd$src, "finished/stats_desc/Repartitions.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Repartitions.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Geographie.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Geographie.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Observateurs.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Observateurs.html"))

#### Classification ####

rmarkdown::render(paste0(wd$src, 
                         "finished/acp/Ordres.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "acp/Ordres.html")
)

#### Regression ####

rmarkdown::render(paste0(wd$src, 
                         "finished/models/glm.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "models/glm/debuts.html"))

rmarkdown::render(paste0(wd$src, "brouillon/glm.Rmd"))
rmarkdown::render(paste0(wd$src, "brouillon/glm.Rmd"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Morts_Collisions.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Morts_Collision.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "brouillon/Diro_desc.Rmd"),
                  output_file = paste0(wd$output, 
                                       "stats_desc/Diro.html"),
                  encoding="UTF-8")

ggplot(grille_10x10) +
  geom_sf(aes(fill = Densite_Cultures_500m)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

ggplot(grille_10x10) +
  geom_sf(aes(fill = Distance_EcotoneArbore)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

ggplot(Total) +
  geom_sf(aes(fill = Indice_Diversite)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

###############

Total <- transform_Total(bdd = Total)
summary(Total)

Donnes_Nat <- Total%>%
  filter(technique_observation == "Vu")
corrplot::corrplot(cor(Donnes_Nat[-c("date", "nom_valide", "nom_verniculaire")]))
Donnes_Nat$

View(bdd_reg)

corrplot::corrplot(cor(bdd_reg))

Donnes_Nat$Indice_Diversite

Donnes_Nat <- Donnes_Nat %>%
  sf::st_drop_geometry()

bdd_reg <- tab_glm(Donnes_Nat, 
                   espece_interet = 61714,
                   espece_benchmark = c(61667, 61057))

summary(bdd_reg)
cor(bdd_reg[-c("clust_max", "famille_paysage_max", "Code_10km")])
corrplot::corrplot(cor(bdd_reg[c(1:3,5:7,9:12)]))

col <- colorRampPalette(c("#990000","#990000", 
                          "#eeeeee",
                          "#05600b","#05600b"))

corrplot::corrplot(cor(subset(bdd_reg, select=-c(famille_paysage_max, Code_10km))),
                   method="color", col=col(200),  
                   order="hclust", 
                   addCoef.col = "black", # Ajout du coefficient de correlation
                   tl.col="black", tl.srt=45 # Rotation des etiquettes de textes
)

t <- bdd_reg %>% dplyr::select(-clust_max, -famille_paysage_max, )
str((bdd_reg$Code_10km))

car::vif(lm(data = bdd_reg, proportion_interet ~ year + Distance_EcotoneArbore_m + Indice_Diversite_m + X_10km + Y_10km+Densite_Cultures_m))
# Des VIF correctes pour tous. On accepte toutes les propositions.

bdd_reg[4]

t <- bdd_reg %>% 
  dplyr::select(-clust_max, -famille_paysage_max, -Code_10km)%>%
  sf::st_drop_geometry()
cor(t)
str(t)


################
########## Distance littoral france.
################

#install the libraries if necessary
if(!require("raster")) install.packages("raster")
if(!require("giscoR")) install.packages("giscoR")

#packages
library(giscoR)
library(sf)
library(raster)
library(RColorBrewer)

#import the limits of Iceland
france <- giscoR::gisco_get_countries(resolution = "01", 
                                      country = "France")
# On prends une grille de la france pour éviter pb de frontières.
france_1x1 <- sf::st_read(
  paste0(wd$data, "masques/France/fr_1km.shp")
)

#transform to UTM
france <- sf::st_transform(france, 3055)
france_1x1 <- france_1x1%>%
  sf::st_transform(3055)%>%
  dplyr::select(CELLCODE)


france_1x1 <- sf::st_intersection(france, france_1x1)

france <- sf::st_cast(france, "MULTILINESTRING")
distance <- sf::st_distance(france, france_1x1)

#distance with unit in meters
france_1x1 <- france_1x1%>%
  transforme_carte()
centroids <- sf::st_centroid(france_1x1)
centroid_coords <- sf::st_coordinates(centroids)
france_1x1_centroids <- france_1x1 %>%
  dplyr::mutate(
    X_1km = centroid_coords[, "X"],
    Y_1km = centroid_coords[, "Y"]
  ) %>%
  dplyr::select(CELLCODE, X_1km, Y_1km)


france_1x1 <- france_1x1 %>%
  dplyr::left_join(
    as.data.frame(france_1x1_centroids),
    by = "CELLCODE"
  )


df <- cbind(france_1x1, distance=as.vector(distance)/1000)

# col_dist <- RColorBrewer::brewer.pal(11, "RdGy")
# ggplot() +
#   geom_sf(data = france_1x1) +
#   labs(title = "Distance du littoral en France Metropolitaine")+
#   geom_sf(data = df, aes(fill = distance)) +
#   scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
#   theme_bw()

# # Uniquement en Bretagne:  
# RegionBretagneConti <- sf::st_read(
#   paste0(wd$data, "masques/RegionBretagneConti/RegionBretagneConti.shp")
# )
# RegionBretagneConti <- RegionBretagneConti %>%
#   transforme_carte()
# df <- df %>%
#   transforme_carte()
# df2 <- sf::st_intersection(df, RegionBretagneConti)
# 
# col_dist <- RColorBrewer::brewer.pal(11, "RdGy")
# ggplot() +
#   geom_sf(data = RegionBretagneConti) +
#   labs(title = "Distance du littoral en Bretagne")+
#   geom_sf(data = df2, aes(fill = distance)) +
#   scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
#   theme_bw()

# Exporter les données.
france_1x1 <- france_1x1%>%
  sf::st_transform(3055)
df <- df %>%
  sf::st_transform(3055)

ext <- extent(as(france_10x10, "Spatial"))
ext

r <- raster:: raster(resolution = 1000, ext = ext,
                     crs = "+proj=utm +zone=27 +ellps=intl 
                     +towgs84=-73,47,-83,0,0,0,0 +units=m +no_defs")
distance_sf <- sf::st_as_sf(df, coords=c("X_10km", "Y_10km")) %>%
  sf::st_set_crs(3055)

distance_raster <- rasterize(distance_sf, r, "distance", fun = mean)
distance_raster
plot(distance_raster)

writeRaster(distance_raster, 
            file = paste0(wd$data, 
                          "masques/VariablesStructurates/Distance_Littoral.tif"), 
            format = "GTiff", overwrite = TRUE)

Distance_Littoral <- terra::rast(paste0(wd$data,
                          "masques/VariablesStructurates/", 
                          "Distance_Littoral", 
                          ".tif"))

terra::crs(Distance_Littoral) <- "EPSG:2154"

# Transformation
grille_10x10 <- sf::st_transform(grille_10x10, crs = "EPSG:2154")
grille_vect <- terra::vect(grille_10x10)
grille_10x10[Distance_Littoral] <- exactextractr::exact_extract(Distance_Littoral,
                                                      grille_10x10, 
                                                      'mean')
