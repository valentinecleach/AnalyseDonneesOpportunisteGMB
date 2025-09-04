rm(list=setdiff(ls(), "Total"))
options(encoding = 'UTF-8')

source("init.R")
wd <- set_wd()

library(dplyr);library(sf);library(ggplot2);library(unmarked)

devtools::load_all()

set.seed(12345)

Total <- sf::st_read(paste0(wd$data, "derived/Total.shp"),
                     options = "ENCODING=UTF8")
Total <- Total%>%
  transform_Total()

VariablesSite <- sf::st_read(paste0(wd$data, "derived/VariablesSite.shp"),
                             options = "ENCODING=UTF8")
VariablesSite <- VariablesSite%>%
  transform_VarSites()

################################
######## KNIT / RENDER #########
################################

#### Nettoyage de donnees ####

# Creation de la base de donnée Total propre
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"),
                  encoding="UTF-8")

# Creation de la base de donnee Diro
rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"),
                  encoding="UTF-8")

# Creation d'une base de donnée Avec les sites, et les variables
rmarkdown::render(paste0(wd$src, "finished/cleaning/Sites.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Sites.html"),
                  encoding="UTF-8")

rm(params)
#### Stats Desc ####
rmarkdown::render(paste0(wd$src, "finished/stats_desc/Repartitions.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Repartitions.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Geographie.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Geographie.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Observateurs.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Observateurs.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Diro_desc.Rmd"),
                  output_file = paste0(wd$output, 
                                       "stats_desc/Diro.html"),
                  encoding="UTF-8")

#### Classification ####

rmarkdown::render(paste0(wd$src, 
                         "finished/autres/Observateurs_ACP.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "Observateurs_ACP.html",
                  encoding="UTF-8")
                  )

#### Regression ####

rmarkdown::render(paste0(wd$src, 
                         "finished/models/glm.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "models/glm/debuts.html"),
                  encoding="UTF-8")


rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lineaire/Reg_Lin_CollisionsRoutieres.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_CollisionsRoutieres.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lineaire/Reg_Lin_ToutesDonnes.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_ToutesDonnes.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lineaire/Reg_Lin_CollisionsRoutieres_DIRO.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_DIRO.html"),
                  encoding="UTF-8")

##### occupancy #####

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/OccupancyLapin.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancyLapin.html"),
                  encoding="UTF-8")


rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/OccupancyPutois.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancyPutois.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/OccupancyHerisson.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancyHerisson.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/temoins/OccuSanglier.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancySanglier.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/SiteCovariates.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/SiteCovariates.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/RpubsFinal.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/RpubsFinal.html"),
                  encoding="UTF-8")
knitr::knit(paste0(wd$src, "finished/RpubsFinal.Rmd"))


########################
### Unkown key etapes ###
########################
# Dans Bash
ls ~/.ssh/id_ed25519.pub 
ssh-keygen -t ed25519 -C "valentine.cleach@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
cat ~/.ssh/id_ed25519.pub
# Ajouter clef ssh a github:
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJaWSroBxK0IQswBQrxYPR6rUpm1IzNoVu5FM3j43nMx valentine.cleach@gmail.com
# Dans bash a nouveau.
ssh -T git@github.com
git remote -v


###########################
#####   BROUILLONS    #####
###########################

library(unmarked);library(dplyr);library(sf);library(ggplot2)

# Map des lapins

grille_10x10 <- sf::st_read(
  paste0(wd$data, "masques/Grille_10x10/Grille_10X10.shp")
)
RegionBretagneConti <- sf::st_read(
  paste0(wd$data, "masques/RegionBretagneConti/RegionBretagneConti.shp")
)

tab <- Total %>%
  dplyr::filter(cd_nom == 61714)%>%
  transforme_carte()
grille_10x10 <- grille_10x10%>%
  transforme_carte()
RegionBretagneConti <- RegionBretagneConti%>%
  transforme_carte()

grille_10x10$density <- lengths(sf::st_intersects(grille_10x10, 
                                                  tab))
grille_10x10 <- sf::st_intersection(grille_10x10, 
                                    RegionBretagneConti)

ggplot() +
  geom_sf(data = RegionBretagneConti) + 
  labs(title = paste0(". Densite des observations du lapin de garenne en bretagne")) +
  geom_sf(data = grille_10x10, aes(fill = density)) +
  scale_fill_gradient(low="white", high="orangered3") +
  theme_bw()

#############################################
#############################################
#############################################
