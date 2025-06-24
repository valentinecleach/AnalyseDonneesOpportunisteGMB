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
setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")
Total <- read_csv("Total.csv")
VN <- "VisioNature"
GN <- "GeoNature"


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
carte_bretagne_44 <- st_transform(carte_bretagne_44, 2154)
carte_bretagne_44 <- st_buffer(carte_bretagne_44, dist = 200)
carte_bretagne_44  <- st_transform(carte_bretagne_44, st_crs(CRS))

setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees/Masque_44")
carte_44 <- st_read("Masque44.shp")
carte_44 <- st_transform(carte_44, 2154)
carte_44 <- st_buffer(carte_44, dist = 200) 
# Il nous manquera peut être qqes données entre le 44 et le 35 mais j'espère pas trop grave?
carte_44 <- st_transform(carte_44, st_crs(CRS))


#filtre
total_bretagne_44 <- st_filter(Total_sf, carte_bretagne_44, .predicate = st_within)
total_44 <- st_filter(Total_sf, carte_44, .predicate = st_within)
total_44_id_synthese <- total_44 %>%
  st_drop_geometry() %>%
  select(id_synthese)
only_bretagne <- anti_join(total_bretagne_44, total_44_id_synthese, by = "id_synthese")

# Plot
ggplot() +
  geom_sf(data = carte_bretagne_44) +
  geom_sf(data = only_bretagne) +
  labs(title = "Points in Bretagne (sauf 44)")

Total_Conti <- only_bretagne %>%
  st_drop_geometry()


Total <- read_csv("~/work/AnalyseDonneesOpportunisteGMB/donnees/Total.csv")
Total <- Total[-1]
str(Total)

Total%>%
  ggplot(aes(technique_observation, fill=etat_biologique))+
  geom_bar(position="dodge")+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red")+
  labs(title = "1a. Répartition de l'état biologique selon les techniques d'observations")

Total%>%
  ggplot(aes(technique_observation, fill=etat_biologique))+
  geom_bar(position="dodge")+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red")+
  coord_flip()+
  labs(title = "1a. Répartition de l'état biologique selon les techniques d'observations")

Total%>%
  ggplot(aes(technique_observation, fill=ordre))+
  geom_bar(position="dodge")+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red")+
  coord_flip()+
  labs(title = "1a. Répartition de l'état biologique selon les techniques d'observations")

Total %>%
  count(nom_vernaculaire, ordre, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), y = n, fill = ordre)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  facet_wrap(~ ordre, scales = "free_y") +
  labs(
    title = "Répartition des espèces",
    y = "Nombre d'observations", x = "Nom vernaculaire"
  ) +
  scale_x_discrete(labels = label_wrap(40)) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12)
  )

Total%>%
  filter(date> as.Date("2010-01-01"))%>%
  ggplot(aes(date))+
  labs(title="Fréquence des observations dans le temps")+ 
  theme_bw()+
  geom_line(stat="density")

Total%>%
  filter(date> as.Date("2010-01-01"))%>%
  ggplot(aes(date))+
  labs(title="Fréquence des observations dans le temps")+ 
  facet_grid(ordre ~ .)+
  theme_bw()+
  geom_line(stat="density")


### density selon les jours de l'année

library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(grid)
library(gridExtra)

Total%>%
  mutate(month = month(date))

date_min <- as.Date("2010-01-01")

Total%>%
    filter(date > date_min)%>%
    ggplot(aes(factor(month(date))))+
    labs(title="Barplot des observation selon le mois",
         y="Nombre d'observations",
         x="Mois")+ 
    theme_bw()+
    geom_bar()+
    scale_x_discrete(labels=c("1" = "Janvier", 
                              "2" = "Février",
                              "3" = "Mars",
                              "4" = "Avril",
                              "5" = "Mai",
                              "6" = "Juin",
                              "7" = "Juillet",
                              "8" = "Aout",
                              "9" = "Septembre",
                              "10" = "Octobre",
                              "11" = "Novembre",
                              "12" = "Decembre"))

Total %>%
  filter(date > date_min)%>%
  mutate(jour_annee = qday(date)+(quarter(date,with_year = FALSE)-1)*91)%>%
  ggplot(aes(jour_annee)) +
  labs(title="Frequence des observations selon le jour de l'année",
       y="Proportion",
       x="Jour de l'année")+
  geom_density()

t <- Total %>%
  mutate(jour_annee = qday(date)+(quarter(date,with_year = FALSE)-1)*91)
summary(t$jour_annee)

str(qday(Total$date))

str(
  quarter(
    Total$date,
    with_year = FALSE))

summary(as.factor(quarter(Total$date, with_year = FALSE)))

Total%>%
  ggplot(aes(as.factor(wday(date, 
                            week_start = getOption("lubridate.week.start", 1)))))+
  labs(title="Barplot des observation selon le jour de la semaine",
       y="Nombre d'observations",
       x="Jour de la semaine")+ 
  theme_bw()+
  geom_bar()+
  scale_x_discrete(labels=c("1" = "Lundi", 
                            "2" = "Mardi",
                            "3" = "Mercredi",
                            "4" = "Jeudi",
                            "5" = "Vendredi",
                            "6" = "Samedi",
                            "7" = "Dimanche"
                            ))

#######################################
##### Classification Observateurs #####
#######################################

On peut aussi effectuer une classification des observateurs selon les espèces observées, le nombre d’observations par taxon, etc., afin de créer une typologie des
observateurs.

summary(as.factor(Total$ordre))

Total <- Total %>%
  mutate(observateurs = toupper(observateurs))

nb_carnivora <- Total %>%
  filter(ordre=="Carnivora")%>%
  group_by(observateurs) %>%
  summarise(nb_carnivora = n(), .groups = "drop")
summary(nb_carnivora)

nb_cetartiodactyla <- Total %>%
  filter(ordre=="Cetartiodactyla")%>%
  group_by(observateurs) %>%
  summarise(nb_cetartiodactyla = n(), .groups = "drop")

nb_eulipotyphla <- Total %>%
  filter(ordre=="Eulipotyphla")%>%
  group_by(observateurs) %>%
  summarise(nb_eulipotyphla = n(), .groups = "drop")

nb_lagomorpha <- Total %>%
  filter(ordre=="Lagomorpha")%>%
  group_by(observateurs) %>%
  summarise(nb_lagomorpha  = n(), .groups = "drop")

nb_rodentia <- Total %>%
  filter(ordre=="Rodentia")%>%
  group_by(observateurs) %>%
  summarise(nb_rodentia  = n(), .groups = "drop")

nb_observations <- Total %>%
  group_by(observateurs) %>%
  summarise(total_obs = n(), .groups = "drop")

# idées dans mon tableau: Ordre préféré.

Observateurs <- nb_observations %>%
  left_join(nb_carnivora, by = "observateurs") %>%
  left_join(nb_cetartiodactyla, by = "observateurs") %>%
  left_join(nb_eulipotyphla, by = "observateurs") %>%
  left_join(nb_lagomorpha, by = "observateurs") %>%
  left_join(nb_rodentia, by = "observateurs") %>%
  mutate_at( c("observateurs"), 
             .funs = as.factor)

Observateurs <- Observateurs %>%
  mutate(nb_carnivora = ifelse(is.na(nb_carnivora), 0, nb_carnivora),
         nb_cetartiodactyla = ifelse(is.na(nb_cetartiodactyla), 0, nb_cetartiodactyla),
         nb_eulipotyphla = ifelse(is.na(nb_eulipotyphla), 0, nb_eulipotyphla),
         nb_lagomorpha = ifelse(is.na(nb_lagomorpha), 0, nb_lagomorpha),
         nb_rodentia = ifelse(is.na(nb_rodentia), 0, nb_rodentia)
         )




Total %>%
  filter(date > as.Date("1980-01-01"),
         ordre == "Carnivora",
         nom_vernaculaire != "Loutre d'Europe, Loutre commune, Loutre") %>%
  mutate(
    year = as.numeric(format(date, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 5), right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.015)
  ) %>%
  ggplot(aes(year_group, fill = nom_vernaculaire_grp)) +
  geom_bar(position = "fill") +
  labs(
    x = "Période",
    title = "Répartition des differentes carnivores observés depuis 1980",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d(labels = label_wrap(40))+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))+
  coord_flip()


summary(as.factor(Total$ordre))

Total %>%
  filter(date > as.Date("1980-01-01"),
         ordre == "Lagomorpha",
         nom_vernaculaire != "Loutre d'Europe, Loutre commune, Loutre") %>%
  mutate(
    year = as.numeric(format(date, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 5), right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.015)
  ) %>%
  ggplot(aes(year_group, fill = nom_vernaculaire_grp)) +
  geom_bar(position = "fill") +
  labs(
    x = "Période",
    title = "Répartition des differentes carnivores observés depuis 1980",
    y = "Proportion",
    fill = "Espèce"
  ) +
  theme_minimal()+
  scale_fill_viridis_d(labels = label_wrap(40))+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))+
  coord_flip()

lapins <- Total%>%
  filter(ordre == "Lagomorpha")%>%
  mutate(
    year = as.numeric(format(date, "%Y")),
    year_group = cut(year, breaks = seq(1980, max(year), by = 5), right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.015)
  ) 
lapins$nom_vernaculaire_grp

prop.table(lapins$nom_vernaculaire)

str(Total)
paste(Total$bdd_originale)


Total%>%
  filter(date > params$date_min,
         bdd_originale == "VisioNature")%>%
  ggplot(aes(ordre))+
  geom_bar()+
  labs(title="3a. Répartition des differents ordres",
       subtitle = paste(Total$bdd_originale))+
  theme_bw()+
  scale_fill_manual(
  values = c("skyblue", "royalblue", "blue", "navy","black"))


Total <- Total%>%
  mutate(observateurs = ifelse(!is.na(observateurs), 
                               toupper(observateurs), NA))%>%
  mutate_at(c("bdd_originale", 
              "etat_biologique", "technique_observation", 
              "communes", "observateurs", 
              "famille", "ordre", 
              "nom_vernaculaire", "nom_valide"), 
            .funs = as.factor)
summary(Total)

setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")

Total_sf <- st_read("Total_sf.shp")
Total_sf%>%
ordre_data <- Total %>% filter(ordre == "Lagomorpha")
bdd_props <- prop.table(table(ordre_data$bdd_originale))
n_bdd1 <- round(5000 * bdd_props[1])
n_bdd2 <- 5000 - n_bdd1
bdd1 <- ordre_data %>% 
  filter(bdd_originale == "GeoNature") %>% 
  sample_n(min(n_bdd1, n()))
bdd2 <- ordre_data %>% 
  filter(bdd_originale == "VisioNature") %>% 
  sample_n(min(n_bdd2, n()))

# Transformation pour des données de la carte
carte_ordre <- ordre_graph %>%
  select(grp_date, ordre, date) %>%
  filter(ordre == ordre_voulu)

VN <- "VisioNature"
GN <- "GeoNature"

date_min <- as.Date("2010-01-01")
etat_bio_espece <- function(nom_ordre, bdd){
  Total%>%
    filter(ordre == nom_ordre) %>%
    count(ordre, sort = TRUE) %>% 
    {
             ggplot(., aes(technique_observation, 
                           fill=nom_vernaculaire)) + 
               geom_bar(position="dodge")+
               theme_bw()
             }
}

etat_bio_espece <- function(nom_ordre, bdd){
  data <- Total %>%
    filter(ordre == nom_ordre) 
  
  # Get order of techniques by count
  technique_order <- data %>%
    count(technique_observation, sort = TRUE) %>%
    pull(technique_observation)
  
  data %>%
    mutate(technique_observation = factor(technique_observation, levels = technique_order)) %>%
    ggplot(aes(technique_observation, fill = nom_vernaculaire)) + 
    geom_bar(position = "dodge") +
    theme_bw()
}

data <- Total %>%
  filter(ordre == "Cetartiodactyla") 

# Get order of techniques by count
technique_order <- data %>%
  count(technique_observation, sort = TRUE) %>%
  pull(technique_observation)

(technique_order)

data %>%
  mutate(technique_observation = factor(technique_observation, levels = technique_order)) %>%
  ggplot(aes(technique_observation, fill = nom_vernaculaire)) + 
  geom_bar(position = "dodge") +
  theme_bw()
rm(t)


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

library(dplyr)
library(forcats)
library(ggplot2)

repartition_espece <- function(nom_ordre, repartition){
  Total %>%
  filter(ordre == nom_ordre) %>%
  mutate(
    nom_vernaculaire_grp = ifelse(
      length(unique(fct_lump(Total$nom_vernaculaire))) > 5 , 
           fct_lump(nom_vernaculaire, prop = 0.03),
           nom_vernaculaire),
    nom_vernaculaire_grp = fct_infreq(nom_vernaculaire_grp)  # reorder by frequency
  )%>%
    ggplot(aes({{repartition}}, fill = nom_vernaculaire_grp)) + 
    geom_bar(position = "dodge") +
    coord_flip()
}



library(dplyr)
library(forcats)
library(ggplot2)

repartition_espece <- function(bdd = Total, nom_ordre, repartition, date_min = params$date_min) {
  
  bdd_filtre <- bdd %>%
    filter(ordre == nom_ordre,
           date > params$date_min)
  
  grps <- fct_lump(bdd_filtre$nom_vernaculaire, prop = 0.03)
  
  if (length(unique(grps)) > 5) {
    bdd_filtre <- bdd_filtre %>%
      mutate(nom_vernaculaire_grp = grps)
  } else {
    bdd_filtre <- bdd_filtre %>%
      mutate(nom_vernaculaire_grp = nom_vernaculaire)
  }
  
  bdd_filtre %>%
    mutate(nom_vernaculaire_grp = fct_infreq(nom_vernaculaire_grp)) %>% 
    ggplot(aes(x = {{ repartition }}, fill = nom_vernaculaire_grp)) +
    geom_bar(position = "dodge") +
    coord_flip()+
    scale_fill_discrete(labels = label_wrap(40)) +
    theme_bw()
}

## Technique D'observation
repartition_espece(nom_ordre = "Carnivora", 
                   repartition = technique_observation)

repartition_espece(nom_ordre = "Cetartiodactyla", 
                   repartition = technique_observation)

repartition_espece(nom_ordre = "Eulipotyphla", 
                   repartition = technique_observation)

repartition_espece(nom_ordre = "Lagomorpha", 
                   repartition = technique_observation)

repartition_espece(nom_ordre = "Rodentia", 
                   repartition = technique_observation)

## Etat Bio
repartition_espece(nom_ordre = "Carnivora", 
                   repartition = etat_biologique)

repartition_espece(nom_ordre = "Cetartiodactyla", 
                   repartition = etat_biologique)

repartition_espece(nom_ordre = "Eulipotyphla", 
                   repartition = etat_biologique)

repartition_espece(nom_ordre = "Lagomorpha", 
                   repartition = etat_biologique)

repartition_espece(nom_ordre = "Rodentia", 
                   repartition = etat_biologique)

source("~/work/AnalyseDonneesOpportunisteGMB/fonctions/fonctions.R")

repartition_espece(nom_ordre = "Rodentia", 
                   repartition = technique_observation)

source("~/work/AnalyseDonneesOpportunisteGMB/fonctions/fonctions.R")

application_tous_ordres(repartition_espece, 
                        repartition = etat_biologique)

application_tous_ordres(repartition_espece, 
                        repartition = technique_observation)
