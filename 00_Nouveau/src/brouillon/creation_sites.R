library(sf)          #For working with spatial data
library(tidyverse)   #For data wrangling and ggplot2
library(ggspatial)   #For spatial data with ggplot2
library(tigris)

rm(list=setdiff(ls(), "Total"))

source("~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/src/functions/divers.R")
wd <- set_wd()
set.seed(12345)
Total <- st_read(paste0(wd$data,"Total.shp"))
Total <- transform_Total()
source(paste0(wd$src,"functions/cartographie.R"), encoding="utf-8")

#####
##### Quadrillage
#####

Total <- creer_maille(bdd = Total, taille_en_km = 2)
Total <- creer_maille(bdd = Total, taille_en_km = 5)
Total <- creer_maille(bdd = Total, taille_en_km = 10)
Total <- creer_maille(bdd = Total, taille_en_km = 20)

#####
#### Quadrillage masque  NON Utilisé
#####

grille_20x20 <- st_read(
  paste0(wd$data, "masques/grille_20x20/METROP_L9320X20.shp")
)
Total <- transforme_carte(Total)
st_crs(Total)
st_crs(grille_20x20)
grille_20x20 <- grille_20x20%>%
  st_transform(st_crs(Total))

Total <- st_join(Total, grille_20x20, left = TRUE)

ggplot() +
  geom_sf(data = grille_20x20) 


  geom_sf_text(
    data = grille_20x20,
    aes(label = Nom),
    size = 3,
    color = "forestgreen"
  )

#####
##### Paysages Bretons 
#####

source("~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/src/functions/divers.R")
wd <- set_wd()
set.seed(12345)
Total <- transforme_carte(Total)


st_crs(Total)

famille_paysage <- st_read(
  paste0(wd$data, "masques/famille_paysage/famille_paysages.shp")
)

famille_paysage$Nom <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Nom)
famille_paysage$Nom <- gsub("\uFFFD", "e", famille_paysage$Nom)

famille_paysage$Famille <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Famille)
famille_paysage$Famille <- gsub("\uFFFD", "e", famille_paysage$Famille)


famille_paysage <- famille_paysage%>%
  transforme_carte()%>%
  filter(CODE_REG != 39)%>%
  st_buffer(famille_paysage, dist = 200)

Total <- st_join(Total, famille_paysage, left = TRUE)

ggplot() +
  geom_sf(data = famille_paysage) +
  geom_sf_text(
    data = famille_paysage,
    aes(label = Nom),
    size = 3,
    color = "forestgreen"
  )

ggplot() +
  geom_sf(data = (Total%>%filter(is.na(Nom))))+
  geom_sf(data=famille_paysage)
  
Total_sans_na <- Total%>%
  filter(!is.na(Nom))

Total <- Total%>%
  mutate(Nom = ifelse(is.na(Nom), 
                      Total$Nom[st_nearest_feature(Total, Total_sans_na)], 
                      Nom),
         Famille = ifelse(is.na(Famille), 
                      Total$Famille[st_nearest_feature(Total, Total_sans_na)], 
                      Famille))
  
Total <- Total %>%
  rename("paysage_ID" = "CODE_REG",
         "paysage_nom" = "Nom",
         "famille_paysage" = "Famille")
