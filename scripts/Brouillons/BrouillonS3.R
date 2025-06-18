library(readr)
library(ggplot2)
library(dplyr)
library(stringi)
library(stringr)
library(sf)
library(rsample)
library(patchwork)
library(scales)

######################
setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees/bretagne_conti")
Total_B <- read_csv("TotalN_FB_Conti.csv")
GeoN <- read_csv("GeoN_Conti.csv")
VisioN_FB <- read_csv("VisioN_FB_Conti.csv")

summary(as.factor(VisioN_FB$technique_observation))


# Carte bretagne
setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees/DepartementsOuest")
carte_bretagne <- st_read("LIM_ADM_DepartementsOuest.shp")
carte_bretagne <- st_set_crs(carte_bretagne, 2154)
carte_bretagne <- st_transform(carte_bretagne, 4326)

# Données geographique
VisioN_sf <- VisioN_FB %>% 
  dplyr::select(x_centroid_4326,
                y_centroid_4326,
                ordre,
                date_debut) %>% 
  sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
               crs = sf::st_crs(4326))


grid <- st_make_grid(carte_bretagne, cellsize = c(0.12, 0.09)) %>% 
  st_sf() %>% 
  st_set_crs(4326)

grid_sf$density <- lengths(st_intersects(grid_sf, VisioN_sf))
grid_sf <- st_intersection(grid_sf, carte_bretagne)

ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Densité des observations en bretagne",
       subtitle = "VisioN_sf") +
  geom_sf(data = grid_sf, aes(fill = density)) +
  scale_fill_gradient(low="gray97", high="gray15") +
  theme_bw()+ facet_grid(. ~ ordre)


# For each ordre, compute grid densities
library(dplyr)
library(sf)
library(ggplot2)
library(purrr)

ordres <- unique(VisioN_sf$ordre)

grid_list <- map(ordres, function(o) {
  vis_ord <- VisioN_sf %>% filter(ordre == o)
  grid_tmp <- grid
  grid_tmp$density <- lengths(st_intersects(grid_tmp, vis_ord))
  grid_tmp$ordre <- o
  grid_tmp
})

grid_sf <- bind_rows(grid_list)
grid_sf <- st_intersection(grid_all, carte_bretagne)

ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Densité des observations en bretagne",
       subtitle = "VisioN_sf") +
  geom_sf(data = grid_sf, aes(fill = density), color = NA) +
  scale_fill_gradient(low="gray90", high="gray15") +
  theme_bw() +
  facet_grid(. ~ ordre)


VisioN_FB %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, 
                     breaks = seq(1980, max(year), by = 5), 
                     right = FALSE)
  ) %>%
  ggplot(aes(year_group, fill = ordre)) +
  geom_bar(position = "fill") +  
  scale_fill_grey(start = 0.2, end = 0.8)


VisioN_FB %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, 
                     breaks = seq(1980, max(year), by = 5), 
                     right = FALSE)
  ) %>%
  ggplot(aes(year_group, fill = nom_valide)) +
  geom_bar(position = "fill") +  
  scale_fill_grey(start = 0.2, end = 0.8)

library(dplyr)
library(ggplot2)
library(forcats)

VisioN_FB %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 5), right = FALSE),
    nom_valide_regroupe = fct_lump(nom_valide, prop = 0.03)
  ) %>%
  ggplot(aes(year_group, fill = nom_valide_regroupe)) +
  geom_bar(position = "fill") +
  labs(
    x = "Période",
    main = "Répartition des differentes espèces observés depuis 1980",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d()

VisioN_FB %>%
  filter(date_debut > as.Date("2010-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 2), right = FALSE),
    nom_valide_regroupe = fct_lump(nom_valide, prop = 0.03)
  ) %>%
  ggplot(aes(year_group, fill = nom_valide_regroupe)) +
  geom_bar(position = "fill") +
#  scale_fill_grey(start = 0.1, end = 0.9) +
  labs(
    main = "Répartition des differentes espèces observés depuis 2010",
    x = "Période",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d()



VisioN_FB <- VisioN_FB %>%
  mutate(nom_vernaculaire = ifelse(cd_nom == 61585, "Rat brun", nom_vernaculaire))
VisioN_FB %>%
  filter(is.na(nom_vernaculaire))%>%
  group_by(nom_valide)%>%
  select(nom_valide, cd_nom)


VisioN_FB %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 5), right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.03)
  ) %>%
  ggplot(aes(year_group, fill = nom_vernaculaire_grp)) +
  geom_bar(position = "fill") +
  labs(
    x = "Période",
    title = "Répartition des differentes espèces observés depuis 1980",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d()


GeoN %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 5), right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.03)
  ) %>%
  ggplot(aes(year_group, fill = nom_vernaculaire_grp)) +
  geom_bar(position = "fill") +
  labs(
    x = "Période",
    title = "Répartition des differentes espèces observés depuis 1980",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d()

VisioN_FB %>%
  filter(date_debut > as.Date("2010-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 2), right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.03)
  ) %>%
  ggplot(aes(year_group, fill = nom_vernaculaire_grp)) +
  geom_bar(position = "fill") +
  labs(
    title = "Répartition des differentes espèces observés depuis 2010",
    x = "Période",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d()

GeoN %>%
  filter(date_debut > as.Date("2010-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 2), right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.03)
  ) %>%
  ggplot(aes(year_group, fill = nom_vernaculaire_grp)) +
  geom_bar(position = "fill") +
  labs(
    title = "Répartition des differentes espèces observés depuis 2010",
    x = "Période",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d()


summary(as.factor(VisioN_FB$ordre))


summary(GeoN)

id_synthèse
date_debut
nom_valide
nom_vernaculaire
ordre
famille
rang_taxo
nombre_min
observateurs
communes
geometrie_wkt_4326 
x_centroid_4326
y_centroid_4326
comment_occurrence 
niveau_validation    
technique_observation
etat_biologique

summary(as.factor(GeoN$niveau_validation))

summary(VisioN_FB)summacomment_occurrencery(VisioN_FB)

GeoN %>%
  filter(niveau_validation == "Douteux")%>%
  select(nom_vernaculaire, date_debut, observateurs, nombre_min)
# Supression : 6 niveau validation == Invalide, 2 Douteux

GeoN %>%
  filter(cd_nom == 194481)%>%
  select(nom_valide)

Martre des pins

t <- GeoN %>%
  filter(ordre=="Carnivora")%>%
  select(nom_vernaculaire)

summary(as.factor(t$nom_vernaculaire))

GeoN%>%
  filter(nom_vernaculaire == "Martre des pins, Martre")%>%
  select(ordre, cd_nom)

GeoN%>%
  select
GeoN%>%
  filter(nom_vernaculaire == "Écureuil roux")

GeoN%>%
  mutate(nom_vernaculaire = ifelse(cd_nom==194944, 
                                   "Vison d'Europe, Vison, Petite loutre, Putois d'eau",
                                   nom_vernaculaire),
         cd_nom = ifelse(cd_nom==194944, 
                         60704, 
                         cd_nom),
         nom_vernaculaire = ifelse(cd_nom==197486, 
                                   "Écureuil roux", 
                                   nom_vernaculaire),
         cd_nom = ifelse(cd_nom==197486, 
                         61153, 
                         cd_nom)
         nom_vernaculaire = ifelse(cd_nom==194481, 
                                   "Martre des pins, Martre", 
                                   nom_vernaculaire),
         cd_nom = ifelse(cd_nom==194481, 
                         60658, 
                         cd_nom)
         
           )

GeoN%>%
  filter(nom_valide == nom_vernaculaire,
         cd_nom != 194481)%>%
  select(nom_valide, cd_nom)
GeoN%>%
  filter(nom_vernaculaire == "Vison d'Europe, Vison, Petite loutre, Putois d'eau")%>%
  select(cd_nom, nom_valide)
GeoN%>%
  filter(cd_nom == 194928)%>%
  select(observateurs, communes, niveau_validation)
  

Martes
194481
194928 
194944 
197486 

######################
# Packages
library(readr)
library(ggplot2)
library(dplyr)
library(stringi)
library(sf)

setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees_brutes")
data_original_22_29 <- read_delim("GeoN_22-29_2025-06-05T09_18_37.225Z.csv", 
                                  delim = ";", 
                                  escape_double = FALSE, 
                                  trim_ws = TRUE)
problems()

data_original_35_56 <- read_delim("GeoN_35-56_2025-06-05T09_21_08.380Z.csv", 
                                  delim = ";", 
                                  escape_double = FALSE, 
                                  trim_ws = TRUE)

data_original_GeoRodents <- read_delim("VN_compl_Rodentia_2025-06-13T14_05_03.107Z.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

GeoN <- rbind(data_original_22_29, 
              data_original_35_56, 
              data_original_GeoRodents)

GeoN <- GeoN %>%
  filter( !niveau_validation %in% c("Douteux","Invalide"))%>%
  mutate(
    # Vison
    nom_vernaculaire = ifelse(cd_nom==194944, 
                              "Vison d'Europe, Vison, Petite loutre, Putois d'eau",
                              nom_vernaculaire),
    cd_nom = ifelse(cd_nom==194944, 60704, cd_nom),
    # Ecureuil
    nom_vernaculaire = ifelse(cd_nom==197486, 
                              "Écureuil roux", 
                              nom_vernaculaire),
    cd_nom = ifelse(cd_nom==197486, 61153, cd_nom),
    # Martre
    nom_vernaculaire = ifelse(cd_nom==194481, 
                              "Martre des pins, Martre", 
                              nom_vernaculaire),
    cd_nom = ifelse(cd_nom==194481, 60658, cd_nom),
    # Rat
    nom_vernaculaire = ifelse(cd_nom==61585, 
                              "Rat brun", 
                              nom_vernaculaire),
    nom_vernaculaire = ifelse(cd_nom==197057, 
                              "Rat Fischer", 
                              nom_vernaculaire)
  )%>%
  filter(cd_nom != 194928)%>%
  mutate(nombre = ifelse(nombre_max != nombre_min, 
                         median(c(nombre_max,nombre_min)),
                         nombre_min)
  )%>%
  mutate(technique_observation = ifelse(
    technique_observation %in% c("Fèces/Guano/Epreintes", 
                                 "Empreintes", "Coulée",
                                 "Galerie/terrier", 
                                 "Hutte (Castor, Rat musqué)", "Nid/Gîte",
                                 "Indices de présence divers",
                                 "Restes de repas",
                                 "Empreintes et fèces",
                                 "Restes dans pelote de réjection"
    ),
    "Indices",
    ifelse(technique_observation %in% c("Entendu", "Ultrasons"), 
           "Entendu/Ultasons",
           technique_observation)
  ))%>%
  mutate(etat_biologique = ifelse(etat_biologique == "NSP", 
                                  "Non renseigné", etat_biologique))%>%
  filter(!jdd_nom %in% c(490,53))%>% 
  # 490: [visionature_opportunistic] Observations ponctuelles de Faune Bretagne
  # 53:  Données faunebretagne.org 
  mutate(date = as.Date(date_debut),
         cd_nom = as.factor(cd_nom),
         ordre = as.factor(ordre),
         famille = as.factor(famille),
         technique_observation,
         etat_biologique=as.factor(etat_biologique),
         jdd_nom = as.factor(jdd_nom),
         nom_valide = as.factor(nom_valide),
         nom_vernaculaire = as.factor(nom_vernaculaire),
         technique_observation = as.factor(technique_observation),
         bdd_originale = as.factor("GeoNature"))%>%
  select("id_synthese",
         "date",
         "cd_nom",
         "nom_valide",	"nom_vernaculaire",
         "ordre",
         "famille",	"rang_taxo",
         "nombre",
         "observateurs",
         "communes",	
         "geometrie_wkt_4326",	
         "x_centroid_4326",
         "y_centroid_4326",
         "comment_occurrence",
         "technique_observation",
         "etat_biologique", "bdd_originale")

donnees_VisioNature_FB <- read_delim("VisioN_FB_2025-06-05T09_19_40.789Z.csv",
                                     delim = ";", 
                                     escape_double = FALSE,
                                     trim_ws = TRUE)
VN_Rodentia <- read_delim("GN_compl_Rodentia_2025-06-13T14_06_22.043Z.csv",
                          delim = ";", 
                          escape_double = FALSE, 
                          trim_ws = TRUE)

VisioN_FB <- rbind(donnees_VisioNature_FB, VN_Rodentia)

VisioN_FB <- VisioN_FB %>%
  filter( !niveau_validation %in% c("Douteux","Invalide"))%>%
  filter(technique_observation != "Restes dans pelote de réjection")%>%
  mutate(technique_observation = ifelse(
    !is.na(comment_occurrence) & technique_observation == "Inconnu",
    ifelse(any(grepl("TAUPINIERE", toupper(gsub("[[:punct:]]", 
                                                "", 
                                                stri_trans_general(
                                                  comment_occurrence, 
                                                  "Latin-ASCII")))),
               grepl("EMPREINTES", toupper(gsub(":punct:]]",
                                                "",
                                                stri_trans_general(
                                                  comment_occurrence, 
                                                  "Latin-ASCII")))),
               grepl("TRACE", toupper(gsub(":punct:]]",
                                           "",
                                           stri_trans_general(
                                             comment_occurrence, 
                                             "Latin-ASCII"))))),
           "Indices",
           technique_observation),
    technique_observation))%>%
  mutate(technique_observation = ifelse(technique_observation=="Entendu",
                                        "Entendu/Ultrasons",
                                        technique_observation))%>%
  mutate(nombre = nombre_min, # nombre max = soit na soit nombre min
         date = as.Date(date_debut),
         cd_nom = as.factor(cd_nom),
         ordre = as.factor(ordre),
         famille = as.factor(famille),
         technique_observation,
         etat_biologique=as.factor(etat_biologique),
         jdd_nom = as.factor(jdd_nom),
         nom_valide = as.factor(nom_valide),
         nom_vernaculaire = as.factor(nom_vernaculaire),
         technique_observation = as.factor(technique_observation),
         bdd_originale = as.factor("VisioNature"))%>%
  select("id_synthese",
         "date",
         "cd_nom",
         "nom_valide", "nom_vernaculaire",
         "ordre",
         "famille",	"rang_taxo",
         "nombre",
         "observateurs",
         "communes",	
         "geometrie_wkt_4326",	
         "x_centroid_4326",
         "y_centroid_4326",
         "comment_occurrence",
         "technique_observation",
         "etat_biologique", "bdd_originale")

Total <- rbind(VisioN_FB, GeoN)

summary(Total)
any(duplicated(Total))

######################

# Carte bretagne

library(sf)
library(ggplot2)


Total_sf <- st_as_sf(Total,
                     coords = c("x_centroid_4326", "y_centroid_4326"), 
                     crs = 4326)
CRS = st_crs(Total_sf)

#importation des cartes
setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees/Masque_Bretagne_Continentale")
carte_bretagne_44 <- st_read("Masque_Bretagne_Continentale.shp")
carte_bretagne_44 <- st_transform(carte_bretagne_44, st_crs(CRS))
setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees/Masque_44")
carte_44 <- st_read("Masque44.shp")
carte_44 <- st_transform(carte_44, st_crs(CRS))

#filtre
total_bretagne_44 <- st_filter(Total_sf, carte_bretagne_44, .predicate = st_within)
# ajouter au dessus une marge.
total_44 <- st_filter(Total_sf, carte_44, .predicate = st_within)
total_44_id_synthese <- total_44 %>%
  st_drop_geometry() %>%
  select(id_synthese)
only_bretagne <- anti_join(total_bretagne_44, total_44_id_synthese, by = "id_synthese")

# Plot -> verifs?
ggplot() +
  geom_sf(data = carte_bretagne_44) +
  geom_sf(data = only_bretagne) +
  labs(title = "Points in Bretagne (sauf 44)")

Total_Conti <- only_bretagne %>%
  st_drop_geometry()
summary(Total_Conti)
