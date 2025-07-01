library(readr)
library(ggplot2)
library(dplyr)
library(stringi)
library(stringr)
library(sf)
library(rsample)
library(patchwork)
library(scales)
library(viridis)
library(lubridate)
library(purrr)
library(forcats)
library(targets)
library(tarchetypes)
library(ggpubr)

library(Factoshiny)
library(collapse)
library(vegan)
library(permute)


# Graine aléatoire
set.seed(12345)

##################
## Importations ##
##################

setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")
Total <- read_delim("Total.csv", delim = ",", 
                    escape_double = FALSE, trim_ws = TRUE)
Total <- Total%>%
  mutate_at(c("bdd_originale", 
              "etat_biologique", "technique_observation", 
              "communes", "observateurs", 
              "famille", "ordre", 
              "nom_vernaculaire", "nom_valide"), 
            .funs = as.factor)


# Fonctions
source("~/work/AnalyseDonneesOpportunisteGMB/fonctions/fonctions.R")


couleur  <- c("Carnivora" = "orangered3",
              "Cetartiodactyla" = "#225d00", 
              "Eulipotyphla" = "bisque4", 
              "Lagomorpha" = "#CC8C3C", 
              "Rodentia" = "darkorange")
VN <- "VisioNature"
GN <- "GeoNature"

Total <- Total %>%
  filter(observateurs != "BELLIER DANIEL",
         technique_observation != "Indices")

###################
### Observateur ###
###################

nb_observations <- Total %>%
  group_by(observateurs) %>%
  summarise(total_obs = n(), .groups = "drop")

max_technique <- Total%>%
  group_by(observateurs) %>%
  summarise(tech_obs_max = collapse::fmode(technique_observation))

max_etat_bio <- Total%>%
  group_by(observateurs) %>%
  summarise(etat_bio_max = collapse::fmode(etat_biologique))

max_bdd <- Total%>%
  group_by(observateurs) %>%
  summarise(bdd_max = collapse::fmode(bdd_originale))


Observateurs <- nb_observations %>%
  left_join(max_technique, by = "observateurs")%>%
  left_join(max_etat_bio, by = "observateurs")%>%
  left_join(max_bdd, by= "observateurs")%>%
  left_join(nb_ordre(nom_ordre = "Carnivora"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Cetartiodactyla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Eulipotyphla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Lagomorpha"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Rodentia"), by = "observateurs")

Observateurs <- Observateurs %>%
  mutate(nb_carnivora = ifelse(is.na(nb_carnivora), 0, nb_carnivora),
         nb_cetartiodactyla = ifelse(is.na(nb_cetartiodactyla), 0, nb_cetartiodactyla),
         nb_eulipotyphla = ifelse(is.na(nb_eulipotyphla), 0, nb_eulipotyphla),
         nb_lagomorpha = ifelse(is.na(nb_lagomorpha), 0, nb_lagomorpha),
         nb_rodentia = ifelse(is.na(nb_rodentia), 0, nb_rodentia)
  )

Observateurs$shannon <- vegan::diversity(Observateurs[,6:10])

Observateurs$Pielou <- Observateurs$shannon / log(vegan::specnumber(Observateurs[,6:10]))
Observateurs <- Observateurs %>%
  mutate(Pielou = ifelse(shannon==0, 0, Pielou))

Observateurs <- Observateurs%>%
  mutate(prop_carnivora = nb_carnivora / total_obs,
         prop_cetartiodactyla = nb_cetartiodactyla / total_obs,
         prop_eulipotyphla = nb_eulipotyphla / total_obs,
         prop_lagomorpha = nb_lagomorpha / total_obs,
         prop_rodentia = nb_rodentia / total_obs,
         tech_obs_max = paste("Tech d'obs: ", tech_obs_max, sep=""),
         etat_bio_max = paste("Etat bio: ", etat_bio_max, sep=""),
         bdd_max = as.character(bdd_max)
  )


Observateurs <- Observateurs %>%
  filter(!(observateurs %in% c("BELLIER_DANIEL", "CHAPUIS_MARTINE")))




####################
#####   DIRO    ####
####################
collision_faune_diro <- read_csv("~/work/AnalyseDonneesOpportunisteGMB/donnees_brutes/collision_faune_diro.csv")


diro <- collision_faune_diro%>%
  mutate(
    geom_x = as.numeric(str_match(geom, "POINT \\(([^ ]+)")[,2]),
    geom_y = as.numeric(str_match(geom, "POINT \\([^ ]+ ([^\\)]+)")[,2])
  )


diro <- diro %>%
  mutate_at(c("route", "concessionpr", "cote", "district", "cei", "cigt", 
              "grp_espece", "espece", "commentaire", "commune"), 
            as.factor)%>%
  filter(!(espece %in% c("amphibien", "amphibiens", "autre", "autre oiseau",
                         "castor", "chat", "chauve-souris", "chien", "chouette",
                         "nr", "oiseaux sauf rapace", "rapace diurne", 
                         "rapace nocturne", "rapaces nocturnes", "rapaces diurnes",
                         "reptile", "reptiles")),
         !is.na(geom))%>%
  mutate(date = as.Date(paste(as.character(annee), mois, 01, sep="-")),
         espece = ifelse(commentaire == "Furet", 
                         "Putois d'Europe, Putois, Furet", espece))%>%
  mutate(bdd_originale = "DIRO",
         technique_observation = "Vu",
         etat_biologique = "Trouvé mort : impact routier")%>%
  select(-date_maj, -annee, -mois, -commentaire, -grp_espece)%>%
  mutate_at(c("route", "concessionpr", "cote", "district", "cei", "cigt", 
              "espece", "commune"), 
            as.factor) 

diro_sf <- st_as_sf(diro, 
                    coords = c("geom_x", "geom_y"), 
                    crs = 2154)

diro_sf <- st_transform(diro_sf, 
                        st_crs(4326))

carte_bretagne_44 <- st_read("~/work/AnalyseDonneesOpportunisteGMB/donnees/Masque_Bretagne_Continentale/Masque_Bretagne_Continentale.shp") %>%
  st_transform(2154)%>%
  st_transform(st_crs(4326))

carte_44 <- st_read("~/work/AnalyseDonneesOpportunisteGMB/donnees/Masque_44/Masque44.shp") %>%
  st_transform(2154) %>%
  st_transform(st_crs(4326))

total_bretagne_44 <- st_filter(diro_sf, 
                               carte_bretagne_44, 
                               .predicate = st_within)
total_44 <- st_filter(diro_sf, 
                      carte_44, 
                      .predicate = st_within)

total_44_id <- total_44 %>%
  st_drop_geometry() %>%
  select(id)

diro_sf <- anti_join(total_bretagne_44, 
                     total_44_id, 
                     by = "id")

ggplot() +
  geom_sf(data = carte_bretagne_44) +
  geom_sf(data = diro_sf)

diro <- st_drop_geometry(diro_sf)
