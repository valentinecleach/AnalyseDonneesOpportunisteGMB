source("init.R")

library(dplyr);library(sf);library(ggplot2);library(unmarked)
set.seed(12345)

devtools::load_all()
wd <- set_wd()

#########################
######## RENDER #########
#########################

#### Nettoyage de donnees ####

# Creation de la base de donnee Total propre
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"),
                  encoding="UTF-8")

# Creation de la base de donnee Diro
rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"),
                  encoding="UTF-8")

# Creation d'une base de donnee Avec les sites, et les variables
rmarkdown::render(paste0(wd$src, "finished/cleaning/Sites.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Sites.html"),
                  encoding="UTF-8")

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

##### Occupancy #####

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


#########################
### Unkown key etapes ###
#########################

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
carte_bretagne <- sf::st_read(
  paste0(wd$data, "masques/Bretagne_Conti_Buffer/bretagne.shp")
  )%>%
  transforme_carte()

tab <- Total %>%
  dplyr::filter(cd_nom == 61714)%>%
  transforme_carte()
grille_10x10 <- grille_10x10%>%
  transforme_carte()

grille_10x10$density <- lengths(sf::st_intersects(grille_10x10, 
                                                  tab))

grille_10x10 <- sf::st_intersection(grille_10x10, 
                                    carte_bretagne)

ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = paste0(". Densite des observations du lapin de garenne en bretagne")) +
  geom_sf(data = grille_10x10, aes(fill = density)) +
  scale_fill_gradient(low="white", high="orangered3") +
  theme_bw()

#############################################
#############################################
#############################################

Total <- sf::st_read(paste0(wd$data, "derived/Total.shp"),
                     options = "ENCODING=UTF8") %>%
  transform_Total()

Diro <- sf::st_read(paste0(wd$data, "derived/diro.shp"))


p1 <- Total%>%
  dplyr::filter(date>=as.Date("2000-01-01"),
                bdd_originale == VN)%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations depuis 2010 de", VN))+ 
  theme_bw()+
  geom_line(stat="density", color="#8B4513")


p2 <- Total%>%
  dplyr::filter(date>=as.Date("2000-01-01"),
                bdd_originale == GN)%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations depuis 2010 de", GN))+ 
  theme_bw()+
  geom_line(stat="density", color="#8B4513")

p3 <- Total%>%
  dplyr::filter(date>=as.Date("2000-01-01"))%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations depuis 2010 \nde", VN, "et", GN))+ 
  theme_bw()+
  geom_line(stat="density", color="#8B4513")

p4 <- Diro%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations depuis 2010 de la DIR Ouest"),
       y = "Densité", x = "Date")+ 
  theme_bw()+
  geom_line(stat="density", color="#8B4513")


p1 <- Total%>%
  dplyr::filter(date>=as.Date("2000-01-01"),
                bdd_originale == VN)%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations depuis 2000 de", VN),
       y = "Densité", x = "Date")+ 
  theme_bw()+
  geom_density(color="#8B4513")
  

p2 <- Total%>%
  dplyr::filter(date>=as.Date("2000-01-01"),
                bdd_originale == GN)%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations depuis 2000 de", GN),
       y = "Densité", x = "Date")+ 
  theme_bw()+
  geom_density(color="#8B4513")
  
p3 <- Total%>%
  dplyr::filter(date>=as.Date("2010-01-01"))%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations depuis 2010 de", VN, "et \nde", GN),
       y = "Densité", x = "Date")+ 
  theme_bw()+
  geom_density(color="#8B4513")
  
p4 <- Diro%>%
  ggplot2::ggplot(aes(date))+
  labs(title=paste("Densité des observations de la DIR Ouest"),
       y = "Densité", x = "Date")+ 
  theme_bw()+
  geom_density(color="#8B4513")
  

dim(Diro)
count(unique(Total%>%filter(bdd_originale==GN, date>as.Date("2010-01-01"), date<as.Date("2025-01-01"))))
24759
160754, 86958, 73796

86958 + 73796
dim(Total%>%filter(bdd_originale==GN))
ggpubr::ggarrange(p1,p2,p3,p4)

VN <- "VisioNature"
GN <- "GeoNature"

rm(list=setdiff(ls(), "Total"))

View(Total)

