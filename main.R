library(renv)
renv::repair()

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


#  scale_fill_gradient(low="#fbf0d1", high="#daa702") +
EcoPaysage <- sf::st_read(paste0(wd$data, "masques/EcoPaysages/EcoPaysages_Vecteur_resol30m_L93.shp"))


# Importer et traiter le raster :
library(tiff)
library(raster)

str_name<-'MOD16A2_ET_0.05deg_GEO_2008M01.tif' 
install.packages("raster")
dist_ecotone_arbore <- tiff::readTIFF(paste0(wd$data, "masques/VariablesStructurates/Distance_EcotoneArbore.tif")) 
?readTIFF

imported_raster=raster::raster(paste0(wd$data, "masques/VariablesStructurates/Distance_EcotoneArbore.tif"))
install.packages("terra") 
r = visualraster::raster("raster.tif")
imported_raster=visualraster::raster(paste0(wd$data, "masques/VariablesStructurates/Distance_EcotoneArbore.tif"))

library("devtools")
install_github("etiennebr/visualraster")
