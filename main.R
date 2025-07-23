library(renv)
renv::repair()

loadedNamespaces()
library(dplyr, sf, ggplot2)

wd <- set_wd()
set.seed(12345)

Total <- sf::st_read(paste0(wd$data, "derived/TotalComplet.shp"))
Total <- transform_Total()
library(dplyr)
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
                                       "models/glm/Morts_Collision.html"))

rmarkdown::render(paste0(wd$src, "brouillon/Diro_desc.Rmd"),
                  output_file = paste0(wd$output, 
                                       "stats_desc/Diro.html"))



install.packages("terra")
library(terra)

# Grille de base
grille_10x10 <- sf::st_read(
  paste0(wd$data, "masques/Grille_10x10/Grille_10X10.shp")
)
RegionBretagneConti <- sf::st_read(
  paste0(wd$data, "masques/RegionBretagneConti/RegionBretagneConti.shp")
)
grille_10x10 <- grille_10x10%>%
  transforme_carte()
RegionBretagneConti <- RegionBretagneConti%>%
  transforme_carte()
grille_10x10 <- sf::st_intersection(grille_10x10, RegionBretagneConti)

# Import
dist_ecotone_arbore <- terra::rast(paste0(wd$data, "masques/VariablesStructurates/Distance_EcotoneArbore.tif"))
Indice_Diversite <- terra::rast(paste0(wd$data, "masques/VariablesStructurates/Indice_Diversite_500m.tif"))

# Transformation
grille_10x10 <- sf::st_transform(grille_10x10, crs(Indice_Diversite))
grille_vect <- terra::vect(grille_10x10)
moyenne <- terra::extract(Indice_Diversite, grille_vect, fun = mean, na.rm = TRUE)
grille_10x10$Indice_Diversite_moyenne <- moyenne[, 2] 


ggplot(grille_10x10) +
  geom_sf(aes(fill = Indice_Diversite_moyenne)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))


