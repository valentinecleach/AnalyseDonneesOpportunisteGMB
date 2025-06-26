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

Observateurs <- nb_observations %>%
  left_join(max_technique, by = "observateurs")%>%
  left_join(max_etat_bio, by = "observateurs")%>%
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

Observateurs$shannon <- vegan::diversity(Observateurs[,5:9])

Observateurs$Pielou <- Observateurs$shannon / log(vegan::specnumber(Observateurs[,5:9]))
Observateurs <- Observateurs %>%
  mutate(Pielou = ifelse(shannon==0, 0, Pielou))

Observateurs <- Observateurs%>%
  mutate(prop_carnivora = nb_carnivora / total_obs,
         prop_cetartiodactyla = nb_cetartiodactyla / total_obs,
         prop_eulipotyphla = nb_eulipotyphla / total_obs,
         prop_lagomorpha = nb_lagomorpha / total_obs,
         prop_rodentia = nb_rodentia / total_obs,
         tech_obs_max = paste("Tech d'obs: ", tech_obs_max, sep=""),
         etat_bio_max = paste("Etat bio: ", etat_bio_max, sep="")
  )


