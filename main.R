library(renv)
renv::repair()

loadedNamespaces()

library(dplyr)
library(sf)
library(ggplot2)

wd <- set_wd()
set.seed(12345)

summary(Total)

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
