# Brouillon Semaine 2

library(readr)
library(ggplot2)
library(dplyr)
library(stringi)
library(sf)
# library(corrplot)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(sf)
library(mapview)
library(maptiles)
library(tidyterra)
library(kableExtra)
library(knitr)

GeoN <- read_csv("donnees/GeoN.csv")
VisioN_FB <- read_csv("donnees/VisioN_FB.csv")

any(duplicated(Total_B))

####################
#################### Regarder la période ############
####################

ggplot(GeoN, aes(ordre))+
  geom_bar()+
  labs(title="Répartition des differents ordres",
       subtitle="GeoN")


GeoN%>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="GeoN")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()


ggplot(VisioN_FB, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="VisioN_FB")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()

# On a des données a differentes périodes entre ces deux bases de données

Total_B <- VisioN_FB %>%
  bind_rows(GeoN)%>%
  filter(date_debut > as.Date("2010-01-01"))

ggplot(Total_B, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total_B")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()


ggplot(VisioN_FB, aes(date_debut))+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total VisioNature depuis 2010")+ facet_grid(ordre ~ .)+
  theme_bw()+
  geom_line(stat="density")

ggplot(GeoN, aes(date_debut))+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total GeoNature depuis 2010")+ facet_grid(ordre ~ .)+
  theme_bw()+
  geom_line(stat="density")


ggplot(Total_B, aes(date_debut))+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total Bretagne depuis 2010")+ facet_grid(ordre ~ .)+
  theme_bw()+
  geom_line(stat="density")


####################
#################### Moyennes Mobiles ###############
####################

Total_B <- VisioN_FB %>%
  bind_rows(GeoN)


#' Moving Average (Moyenne Mobile)
#' 
#' @param x 
#' @param y 
#' @returns 
#' @examples
ma <- function(x, n = 5){
  return(stats::filter(x, rep(1 / n, n), 
                       sides = 2))
}

dates <- data.frame(date_debut = seq(
  from = min(as.Date("2010-01-01")),
  to = max(Total_B$date_debut),
  by = "day"
))

Total_B_new <- Total_B %>%
  arrange(date_debut) %>%
  left_join(Total_B, by = "date_debut") %>%
  mutate(nombre_min = ifelse(is.na(nombre_min), 0, nombre_min))%>%
  filter(date_debut > as.Date("2010-01-01")) %>%
  # need to sum all observations if same day
  mutate(nombre_obs_jour = sum(nombre_min))
  mutate(moyenne_mobile_obs = ma(nombre_obs_jour, n=10))
  library(dplyr)
  
  
Total_B <- dates %>%
    left_join(Total_B, by = "date_debut") %>%
    arrange(date_debut) %>%
    mutate(nombre_min = ifelse(is.na(nombre_min), 0, nombre_min)) %>%
    filter(date_debut > as.Date("2010-01-01")) %>%
    group_by(date_debut) %>%
    summarise(nombre_obs_jour = sum(nombre_min)) %>%
    mutate(moyenne_mobile_obs = ma(nombre_obs_jour, n = 10)) %>%
    ungroup()
  

Total_B <- VisioN_FB %>%
  bind_rows(GeoN)%>%
  filter(date_debut > as.Date("2010-01-01"))


Total_B <- Total_B %>%
  filter(date_debut > as.Date("2010-01-01")) %>%
  select(date_debut, ordre, nombre_min, observateurs) %>%
  mutate(nombre_min = ifelse(is.na(nombre_min), 0, nombre_min)) %>%
  group_by(ordre, date_debut) %>%
  summarise(nombre_obs_jour = sum(nombre_min), .groups = "drop") %>%
  arrange(ordre, date_debut) %>%
  group_by(ordre) %>%
  mutate(moyenne_mobile_obs = ma(nombre_obs_jour, n = 10)) %>%
  ungroup()%>%
  mutate(ordre = as.factor(ordre))


ggplot(Total_B, aes(date_debut,moyenne_mobile_obs))+
  geom_bar()+
  labs(title="Répartition des MM (de 10) selon les differents ordres",
       subtitle="Total_B")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()



summary(Total_B)
ggplot(Total_B, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total Bretagne")+ facet_grid(technique_observation  ~ .)+
  ylim(0, 30)+
  theme_bw()

GeoN_new <- GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))
ggplot(GeoN_new, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="GeoNature")+ facet_grid(technique_observation  ~ .)+
  ylim(0, 30)+
  theme_bw()

VisioN_FB_new <- VisioN_FB %>%
  filter(date_debut > as.Date("2010-01-01"))
ggplot(VisioN_FB_new, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="VisioNature Bretagne")+ facet_grid(technique_observation  ~ .)+
  ylim(0, 30)+
  theme_bw()


####################
#################### Rep Cartographique #############
####################

library(sf)
library(ggplot2)
library(rnaturalearth)

# Carte bretagne
carte_bretagne <- st_read("donnees/LIM_ADM_DepartementsOuest.shp")
carte_bretagne <- st_set_crs(carte_bretagne, 2154)
carte_bretagne <- st_transform(carte_bretagne, 4326)

# Données geographique
Geo_VisioN <- VisioN_FB_new %>% 
  dplyr::select(x_centroid_4326,
                y_centroid_4326,
                ordre,
                date_debut) %>% 
  sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
               crs = sf::st_crs(4326))

Geo_GeoN <- GeoN_new %>% 
  dplyr::select(x_centroid_4326,
                y_centroid_4326,
                ordre,
                date_debut) %>% 
  sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
               crs = sf::st_crs(4326))


ggplot() +
  geom_sf(data = Geo_VisioN, aes(color = ordre)) +
  labs(title = "Carte des observations en bretagne",
       color = "Ordre") +
  theme_bw()

ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Carte des observations en Bretagne selon les ordres",
       subtitle = "VisioNature Bretagne") +
  geom_sf(data = Geo_VisioN, aes(color = ordre)) +
  theme_bw()




ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Carte des observations en Bretagne selon les ordres",
       subtitle = "GeoNature") +
  geom_sf(data = Geo_GeoN, aes(color = ordre)) +
  theme_bw()


ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Carte des observations en Bretagne selon les ordres",
       subtitle = "GeoNature") +
  geom_sf(data = Geo_GeoN, size=0.01) +
  theme_bw() + facet_grid(. ~ ordre)




# Ajouter des libelés
GeoN_1 <- GeoN %>%
  mutate(
    grp_date = case_when(
      date_debut >= as.Date("2010-01-01") & date_debut <= as.Date("2014-12-31") ~ "T1",
      date_debut >= as.Date("2015-01-01") & date_debut <= as.Date("2019-12-31") ~ "T2",
      date_debut > as.Date("2019-12-31") ~ "T3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(date_debut > as.Date("2010-01-01"))


# regarder les commentaires
Total_B%>%
  mutate(comment_occurrence= as.factor(comment_occurrence),
         technique_observation= as.factor(technique_observation))%>%
  select(technique_observation, comment_occurrence)%>%
  filter(!is.na(comment_occurrence))%>%
  filter(technique_observation == "Inconnu")%>%
  print(n=3000)



setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")
GeoN <- read_csv("GeoN.csv")
VisioN_FB <- read_csv("VisioN_FB.csv")

####################
#################### Les observateurs #############
####################

str(GeoN$observateurs)
summary(as.factor(GeoN$observateurs))

GeoN %>% 
  summarize(nb_observateurs = n_distinct(observateurs))
# 2778 observateurs dans la GeoNature
# 1 ou pls observateurs a la fois

VisioN_FB %>% 
  summarize(nb_observateurs = n_distinct(observateurs))
# 3662 observateurs, 1813 NA
# Pas même format -> Nom de famille a mettre en majuscule

summary(as.factor(VisioN_FB_modif$observateurs))

library(dplyr)
library(stringr)

VisioN_FB_modif <- VisioN_FB %>%
  mutate(
    observateurs = na_if(observateurs, "NA"),
    observateurs = na_if(observateurs, "NA NA")
  )


VisioN_FB_modif %>%
  filter(!is.na(observateurs) & !str_starts(observateurs, "Anonyme")) %>%
  select(observateurs)

# cant really change just surname => capitilise everything

VisioN_FB <- VisioN_FB %>%
  mutate(
    observateurs = na_if(observateurs, "NA"),
    observateurs = na_if(observateurs, "NA NA"),
    observateurs = toupper(observateurs)
  )

GeoN <- GeoN %>% 
  mutate(observateurs = toupper(observateurs))

prop.table(table(GeoN$observateurs))

GeoN %>%
  group_by(observateurs) %>%
  summarize(nbr_obversations_par_observateur = count(n_distinct(observateurs)))%>%
  mutate(name = fct_reorder(name, val)) %>%
  

# Lets see the distribution of the observateurs


GeoN_summary <- GeoN %>%
  group_by(observateurs) %>%
  summarize(nbr_obs_obr = n()) %>%
  filter(nbr_obs_obr > 200) %>%
  arrange(nbr_obs_obr)

GeoN_summary$observateurs <- factor(GeoN_summary$observateurs,
                                    levels = GeoN_summary$observateurs)

ggplot(GeoN_summary, aes(x=observateurs, y=nbr_obs_obr)) +
  geom_point() + 
  geom_segment( aes(x=observateurs, xend=observateurs, 
                    y=0, yend=nbr_obs_obr))+
  coord_flip()+
  labs(title="Lollipop plot des principaux observateurs",
       subtitle="GeoNature")+
  theme_bw()

# Il faudra faire gaffe a pas biaser les résultats car certains participe 
# très grandement au receuil des données donc un changement de leur comportement
# implique également un changement du comportement de la base de donnée

# Regarder les dates des principaux observateurs ainsi que leurs espèces, et les lieux?
# des biais pour un endroit du territoire?
# des biais pour une espece?
# déplacement donc refletement dans les observations?
# Arrêt des déclarations pendant un moment?

# Observons t-on la même chose avec VisioNature?
VisioN_summary <- VisioN_FB %>%
  group_by(observateurs) %>%
  summarize(nbr_obs_obr = n()) %>%
  filter(nbr_obs_obr > 200) %>%
  arrange(nbr_obs_obr)

VisioN_summary$observateurs <- factor(VisioN_summary$observateurs,
                                    levels = VisioN_summary$observateurs)

ggplot(VisioN_summary, aes(x=observateurs, y=nbr_obs_obr)) +
  geom_point() + 
  geom_segment( aes(x=observateurs, xend=observateurs, 
                    y=0, yend=nbr_obs_obr))+
  coord_flip()+
  labs(title="Lollipop plot des principaux observateurs",
       subtitle="VisioNature")+
  theme_bw()

# Encore pire
# Bcp de NA aussi -> pas tant que ca un problème je penses

VisioN_summary <- VisioN_FB %>%
  group_by(observateurs) %>%
  summarize(nbr_obs_obr = n()) %>%
  filter(nbr_obs_obr > 400) %>%
  arrange(nbr_obs_obr)

VisioN_summary$observateurs <- factor(VisioN_summary$observateurs,
                                      levels = VisioN_summary$observateurs)

ggplot(VisioN_summary, aes(x=observateurs, y=nbr_obs_obr)) +
  geom_point() + 
  geom_segment( aes(x=observateurs, xend=observateurs, 
                    y=0, yend=nbr_obs_obr))+
  coord_flip()+
  labs(title="Lollipop plot des principaux observateurs",
       subtitle="VisioNature")+
  theme_bw()

# Faire att a Belier Daniel

###################
#DatesObservations# 
###################

# Exemple

VisioN_FB %>%
  filter(observateurs == toupper("BELLIER DANIEL"),
         date_debut > as.Date("2010-01-01")) %>%
  ggplot(aes(date_debut))+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle = paste("Données de ", "BELLIER DANIEL")) + 
  theme_bw()+
  geom_line(stat="density")+
  scale_x_date(breaks = seq(from = min(VisioN_FB$date_debut), 
                            to = max(VisioN_FB$date_debut), 
                            by = "2 years"),               
               labels = date_format("%Y")) 

########
#Ordres# 
########

# exemple

VisioN_FB %>%
  filter(observateurs == toupper("BELLIER DANIEL")) %>%
  ggplot(aes(x = ordre)) +  
  geom_bar() + 
  labs(title = "Répartition des differents ordres", 
       subtitle = paste("Données de ", "BELLIER DANIEL")) +
  theme_bw()

##############
#Cartographie# 
##############


setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")
carte_bretagne <- st_read("LIM_ADM_DepartementsOuest.shp")
carte_bretagne <- st_set_crs(carte_bretagne, 2154)
carte_bretagne <- st_transform(carte_bretagne, 4326)

# Données geographique
geo_bellier <- VisioN_FB %>% 
  filter(date_debut > as.Date("2010-01-01")) %>%
  filter(observateurs == toupper("BELLIER DANIEL")) %>%
  dplyr::select(x_centroid_4326,
                y_centroid_4326,
                ordre,
                date_debut) %>% 
  sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
               crs = sf::st_crs(4326))

ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Carte des observations",
       subtitle = paste("Données de ", "BELLIER DANIEL")) +
  geom_sf(data = geo_bellier, size=0.01) +
  theme_bw()

library(patchwork)

stats_observateur <- function(nom_obs, bdd, carte){
  
  # Date
  p1 <- bdd %>%
    filter(observateurs == toupper(nom_obs),
           date_debut > as.Date("2010-01-01")) %>%
    ggplot(aes(date_debut))+
    labs(title="Répartition des dates",
         subtitle = paste("Données de ", nom_obs)) + 
    theme_bw()+
    geom_line(stat="density")+
    scale_x_date(breaks = seq(from = min(bdd$date_debut), 
                              to = max(bdd$date_debut), 
                              by = "2 years"),               
                 labels = date_format("%Y")) 
  
  # Ordres
  p2 <- bdd %>%
    filter(observateurs == toupper(nom_obs)) %>%
    ggplot(aes(x = ordre)) +  
    geom_bar() + 
    labs(title = "Répartition des differents ordres", 
         subtitle = paste("Données de ", nom_obs)) +
    theme_bw()
  
  # Carte
  geo_obsteur <- bdd %>% 
      filter(date_debut > as.Date("2010-01-01")) %>%
      filter(observateurs == toupper(nom_obs)) %>%
      dplyr::select(x_centroid_4326,
                    y_centroid_4326,
                    ordre,
                    date_debut) %>% 
      sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
                   crs = sf::st_crs(4326))
  p3 <- ggplot() +
      geom_sf(data = carte) + 
      labs(title = "Carte des observations",
           subtitle = paste("Données de ", nom_obs)) +
      geom_sf(data = geo_obsteur, size=0.01) +
      theme_bw()
  

  # Affichage des plots
  p1 + p2 + p3
  }

setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")
carte_bretagne <- st_read("LIM_ADM_DepartementsOuest.shp")
carte_bretagne <- st_set_crs(carte_bretagne, 2154)
carte_bretagne <- st_transform(carte_bretagne, 4326)

stats_observateur(nom_obs="BELLIER DANIEL", 
                  bdd=VisioN_FB, 
                  carte=carte_bretagne)


GeoN%>%
  filter(etat_biologique == "Trouvé mort : impact routier",
         date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(class=etat_biologique, x=nombre_min))+
  geom_boxplot()


GeoN %>%
  filter(date_debut > as.Date("2010-01-01"),
         nombre_min <1000) %>%
  ggplot(aes(x = etat_biologique, y = nombre_min)) +
  geom_boxplot()
# More frequent to see animals together alive than on road?
GeoN %>%
  filter(date_debut > as.Date("2010-01-01"),
         nombre_min <1000,
         etat_biologique != "NSP") %>%
  ggplot(aes(x = etat_biologique, y = nombre_min)) +
  geom_boxplot()

GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(x=etat_biologique))+
  geom_bar()+
  theme_bw()+
  labs(title="Repartition des etats biologique",
       subtitle="GeoN")

VisioN_FB %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(x=etat_biologique))+
  geom_bar()+
  theme_bw()+
  labs(title="Repartition des etats biologique",
       subtitle="VisioN_FB")

GeoN %>%
  mutate(communes_clean = stri_trans_general(communes, 
                                             "Latin-ASCII"),
         communes_clean = gsub("[[:punct:]]", 
                               "", 
                               communes_clean)) %>%
  filter(str_detect(communes_clean, 
                    regex("\\b(ile de|lile|ile )", 
                          ignore_case = TRUE))) %>%
  select(communes)
  
library(dplyr)
library(stringi)
library(stringr)


Ile_GeoN <- GeoN %>%
  mutate(
    communes_clean = stri_trans_general(communes, "Latin-ASCII"),
    communes_clean = gsub("[[:punct:]]", " ", communes_clean),
  )%>%
  filter(str_detect(communes_clean, 
                    regex("\\b(ile de|ile|lile)\\b", 
                          ignore_case = TRUE))
         )

SansIles_GeoN <- GeoN %>%
  mutate(
    communes_clean = stri_trans_general(communes, "Latin-ASCII"),
    communes_clean = gsub("[[:punct:]]", " ", communes_clean),
  )%>%
  filter(!str_detect(communes_clean, 
                    regex("\\b(ile de|ile|lile)\\b", 
                          ignore_case = TRUE))
  )

Ile_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(ordre))+geom_bar()+theme_bw()
# comme attendu bcp de lapins/lièvres, 
# et peu de carnivores qui pourraient les manger
Ile_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(etat_biologique))+geom_bar()+theme_bw()
# pas de ecrasé par les voitures
Ile_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(nom_vernaculaire))+geom_bar()+theme_bw()+
  labs(title="nom vernaculaire sur les iles de GeoNature")
Ile_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(technique_observation))+geom_bar()+theme_bw()+
  labs(title="technique observation sur les iles de GeoNature")

SansIles_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(ordre))+geom_bar()+theme_bw() +labs(title = "hors ile")
# Surtout carnivores
SansIles_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(etat_biologique))+geom_bar()+theme_bw()+labs(title = "hors ile")
# plus de morts par impact routiers
SansIles_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"),
         ordre == "Rodentia")%>%
  ggplot(aes(nom_vernaculaire))+geom_bar()+theme_bw()+
  labs(title="nom vernaculaire des rodentia hors des iles. GeoNature")
# voir les tops animaux par catégorie après
SansIles_GeoN %>%
  filter(date_debut > as.Date("2010-01-01"))%>%
  ggplot(aes(technique_observation))+geom_bar()+theme_bw()+
  labs(title="technique observation hors iles de GeoNature")

#verif que hors ile, il n'y a bien pas d'iles?


setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")
carte_bretagne <- st_read("LIM_ADM_DepartementsOuest.shp")
carte_bretagne <- st_set_crs(carte_bretagne, 2154)
carte_bretagne <- st_transform(carte_bretagne, 4326)

geo_horsile <- SansIles_GeoN %>% 
  filter(date_debut > as.Date("2010-01-01")) %>%
  dplyr::select(x_centroid_4326,
                y_centroid_4326,
                ordre,
                date_debut) %>% 
  sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
               crs = sf::st_crs(4326))
ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Carte des observations",
       subtitle = paste("Données hors îles")) +
  geom_sf(data = geo_horsile, size=0.01) +
  theme_bw()

# Il reste des îles


library(sf)
bzh <- st_read("bretagne-latest-free.shp.zip")
# The shapefile includes layers like 'natural' and 'waterway'.
:contentReference[oaicite:5]{index=5}
:contentReference[oaicite:6]{index=6}
:contentReference[oaicite:7]{index=7}
:contentReference[oaicite:8]{index=8}


summary(as.factor(GeoN$ordre))

GeoN %>%
  filter(ordre == "Carnivora")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les carnivores",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))

GeoN %>%
  filter(ordre == "Cetartiodactyla")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les cétartiodactyles",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(30))

GeoN %>%
  filter(ordre == "Eulipotyphla")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les eulipotyphles",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))

GeoN %>%
  filter(ordre == "Lagomorpha")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les lagomorphes",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))

GeoN %>%
  filter(ordre == "Rodentia")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les rongeurs",
       subtitle = "GeoNature",
       y="Nombre d'observations",x="Nom Vernaculaire")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))


VisioN_FB %>%
  filter(ordre == "Carnivora")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les carnivores",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))


VisioN_FB %>%
  filter(ordre == "Cetartiodactyla")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les cétartiodactyles",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(30))



VisioN_FB %>%
  filter(ordre == "Eulipotyphla")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les eulipotyphles",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))



VisioN_FB %>%
  filter(ordre == "Lagomorpha")%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les lagomorphes",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))

VisioN_FB %>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces",
       subtitle = "VisioNature",
       y="Nombre d'observations",x="Nom Vernaculaire")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))+    
  facet_grid(. ~ ordre)

GeoN %>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces",
       subtitle = "VisioNature",
       y="Nombre d'observations",x="Nom Vernaculaire")+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))+    
  facet_grid(. ~ ordre)

VisioN_FB %>%
  count(nom_vernaculaire, ordre, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), y = n, fill = ordre)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  facet_wrap(~ ordre, scales = "free_y") +
  labs(
    title = "Répartition des espèces",
    subtitle = "VisioNature",
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

GeoN %>%
  count(nom_vernaculaire, ordre, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), y = n, fill = ordre)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  facet_wrap(~ ordre, scales = "free_y") +
  labs(
    title = "Répartition des espèces",
    subtitle = "GeoNature",
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

########## Carte denouveau ##########

library(tidyverse) 
library(sf)

setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees")
carte_bretagne <- sf::read_sf("LIM_ADM_DepartementsOuest.shp")
carte_bretagne <- st_set_crs(carte_bretagne, 2154)
carte_bretagne <- st_transform(carte_bretagne, 4326)

# Données geographique
Geo_GeoN <- GeoN %>%
  filter(date_debut > as.Date("2010-01-01")) %>%
  dplyr::select(x_centroid_4326,
                y_centroid_4326,
                ordre,
                etat_biologique,
                date_debut) %>% 
  sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
               crs = sf::st_crs(4326))

ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Carte des observations en Bretagne selon les ordres",
       subtitle = "GeoNature") +
  theme_bw()+
  geom_hex(data = Geo_GeoN, aes=())


Geo_GeoN <- Geo_GeoN %>%
  mutate(etat_biologique = as.factor(etat_biologique),
         densite_obs_vivant = )

Geo_GeoN$density_A <- ifelse(Geo_GeoN$etat_biologique == "Observé vivant", 1, 0)

ggplot(carte_bretagne) +
  geom_sf(data = Geo_GeoN, aes(fill = density_A)) +
  facet_grid(. ~ etat_biologique) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_bw()

ggplot(carte_bretagne) +
  geom_sf(data = Geo_GeoN, aes(fill = etat_biologique)) +
  scale_fill_manual(
    values = c("Observé vivant" = "blue", "NSP" = "green", "Trouvé mort" = "red"),
    na.value = "white"
  )



ggplot(Geo_GeoN, aes(x=, y=ordre) ) +
  geom_sf(data = carte_bretagne) +
  geom_bin2d() +
  theme_bw()

?geom_hex


# Step 1: Create a variable for each category as a dummy
Geo_GeoN <- Geo_GeoN %>%
  mutate(etat_biologique = as.factor(etat_biologique),
         density_A = ifelse(etat_biologique == "Observé vivant", 1, 0))

# Step 2: Aggregate (sum) by spatial unit (assuming polygons are in Geo_GeoN)
Geo_density <- Geo_GeoN %>%
  group_by(geometry) %>%
  summarise(density_A = sum(density_A, na.rm = TRUE))

# Step 3: Plot
ggplot(carte_bretagne) +
  geom_sf(data = Geo_density, 
          aes(fill = density_A)
  ) +
  scale_fill_gradient(low = "lightblue", 
                      high = "darkblue", 
                      name = "Densité") +
  theme_bw()

ggplot(Geo_density) +
  geom_sf(aes(fill = density_A)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "lightblue") +
  theme_bw()



 GeoN%>%
  group_by(nom_valide)%>%
  select(nom_valide, nom_vernaculaire, ordre, cd_nom)

GeoN%>%
   filter(cd_nom != 60630)%>%
   ggplot(aes(ordre))+
   geom_bar()+
   labs(title="Répartition des differents ordres",
        subtitle="GeoNature sans les loutres")
 
GeoN %>%
  filter(ordre == "Carnivora")%>%
  filter(cd_nom != 60630)%>%
  ggplot(aes(nom_vernaculaire))+
  labs(title = "Répartition des espèces parmi les carnivores",
       subtitle = "GeoNature",
       x="Nom Vernaculaire",
       y="Nombre d'observations")+
  geom_bar()+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90))+ 
  coord_flip()+
  scale_x_discrete(labels = label_wrap(40))



GeoN_Conti <- read_csv("work/AnalyseDonneesOpportunisteGMB/donnees/GeoN_Conti.csv")
GeoN <- read_csv("work/AnalyseDonneesOpportunisteGMB/donnees/GeoN.csv")

VisioN_FB_Conti <- read_csv("work/AnalyseDonneesOpportunisteGMB/donnees/VisioN_FB_Conti.csv")



VisioN_FB %>%
  count(nom_vernaculaire, ordre, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), y = n, fill = ordre)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  facet_wrap(~ ordre, scales = "free_y") +
  labs(
    title = "Répartition des espèces",
    subtitle = "VisioNature",
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


GeoN %>%
  count(nom_vernaculaire, ordre, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), y = n, fill = ordre)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  facet_wrap(~ ordre, scales = "free_y") +
  labs(
    title = "Répartition des espèces",
    subtitle = "GeoNature",
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


summary(as.factor(VisioN_FB$nom_vernaculaire))

VisioN_FB %>%
  count(nom_vernaculaire, ordre, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), y = n, fill = ordre)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  facet_wrap(~ ordre, scales = "free_y") +
  labs(
    title = "Répartition des espèces",
    subtitle = "GeoNature",
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

VisioN_FB_Conti %>%
  count(nom_vernaculaire, ordre, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), y = n, fill = ordre)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  facet_wrap(~ ordre, scales = "free_y") +
  labs(
    title = "Répartition des espèces",
    subtitle = "GeoNature",
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



# Conti vs non conti:
  
# Hérisson d'Europe
# Lapin de Garenne
# Loutre -> etonnant en vrai non?
# Chevreuil

# nombre d'observations?

VisioN_FB
82344-79064
3280 obs supprimés

Faune Bretagne
52553 - 51949
604 obs supprimés

# GeoNature
GeoN_sf <- st_as_sf(GeoN, 
                  coords = c("x_centroid_4326", "y_centroid_4326"), 
                  crs = 4326)
grid_sf <- st_sf(geometry = st_make_grid(carte_bretagne, 
                                         cellsize = c(0.12, 0.09))
              )
grid_sf$density <- lengths(st_intersects(grid_sf, GeoN_sf))
grid_sf <- st_intersection(grid_sf, carte_bretagne)
ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Densité des observations en bretagne",
       subtitle = "GeoNature avec îles") +
  geom_sf(data = grid_sf, aes(fill = density)) +
  scale_fill_gradient(low="gray97", high="gray15") +
  theme_bw()
# C'est pas sizun le gros point noir?

# VisioNature
VisioN_FB_sf <- st_as_sf(VisioN_FB, 
                    coords = c("x_centroid_4326", "y_centroid_4326"), 
                    crs = 4326)
grid_sf <- st_sf(geometry = st_make_grid(carte_bretagne, 
                                         cellsize = c(0.12, 0.09)))
grid_sf$density <- lengths(st_intersects(grid_sf, VisioN_FB))
grid_sf <- st_intersection(grid_sf, carte_bretagne)
ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Densité des observations en bretagne",
       subtitle = "VisioNature avec îles") +
  geom_sf(data = grid_sf, aes(fill = density)) +
  scale_fill_gradient(low="grey97", high="gray15") +
  theme_bw()


# En voyant cette carte, on aimerait bien supprimer Martine et Daniel Bellier...

VisioN_FB_modif <- VisioN_FB%>%
  mutate(observateurs = toupper(observateurs))%>%
  filter(!observateurs %in% c("BELLIER DANIEL", "CHAPUIS MARTINE"))
  
VisioN_FB_sf <- st_as_sf(VisioN_FB_modif, 
                      coords = c("x_centroid_4326", "y_centroid_4326"), 
                      crs = 4326)
grid_sf <- st_sf(geometry = st_make_grid(carte_bretagne, 
                                         cellsize = c(0.12, 0.09))
)
grid_sf$density <- lengths(st_intersects(grid_sf, VisioN_FB_sf))
grid_sf <- st_intersection(grid_sf, carte_bretagne)
ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Densité des observations en bretagne",
       subtitle = "VisioNature avec îles, sans Daniel et Martine") +
  geom_sf(data = grid_sf, aes(fill = density)) +
  scale_fill_gradient(low="grey97", high="gray15") +
  theme_bw()




stats_observateur <- function(nom_obs, bdd, carte){
  
  # Date
  p1 <- bdd %>%
    filter(observateurs == toupper(nom_obs),
           date_debut > as.Date("2010-01-01")) %>%
    ggplot(aes(date_debut))+
    labs(title="Répartition des dates",
         subtitle = paste("Données de ", toupper(nom_obs))) + 
    theme_bw()+
    geom_line(stat="density")+
    scale_x_date(
      breaks = seq(from = min(bdd$date_debut), 
                   to = max(bdd$date_debut), 
                   by = "2 years"),               
      labels = scales::label_date("%Y")
    )
  
  # Ordres
  p2 <- bdd %>%
    filter(observateurs == toupper(nom_obs)) %>%
    ggplot(aes(x = ordre)) +  
    geom_bar() + 
    labs(title = "Répartition des differents ordres", 
         subtitle = paste("Données de ", toupper(nom_obs))) +
    theme_bw()
  
  # Carte
  geo_obsteur <- bdd %>% 
    filter(date_debut > as.Date("2010-01-01")) %>%
    filter(observateurs == toupper(nom_obs)) %>%
    dplyr::select(x_centroid_4326,
                  y_centroid_4326,
                  ordre,
                  date_debut) %>% 
    sf::st_as_sf(coords = c("x_centroid_4326", "y_centroid_4326"),
                 crs = sf::st_crs(4326))
  p3 <- ggplot() +
    geom_sf(data = carte) + 
    labs(title = "Carte des observations",
         subtitle = paste("Données de ", toupper(nom_obs))) +
    geom_sf(data = geo_obsteur, size=0.01) +
    theme_bw()
  
  
  grid_sf <- st_sf(geometry = st_make_grid(carte, 
                                           cellsize = c(0.12, 0.09)))
  grid_sf$density <- lengths(st_intersects(grid_sf, geo_obsteur))
  grid_sf <- st_intersection(grid_sf, carte)
  p4 <- ggplot() +
    geom_sf(data = carte) + 
    labs(title = "Densité des observations en bretagne",
         subtitle = paste(bdd)) +
    geom_sf(data = grid_sf, aes(fill = density)) +
    scale_fill_gradient(low="grey97", high="gray15") +
    theme_bw()
  
  # Affichage des plots
  p1 + p2 + p3 + p4
}

stats_observateur(nom_obs="simonnet franck", 
                  bdd=GeoN, 
                  carte=carte_bretagne)

library(patchwork)

GeoN%>%
  filter(date_debut > as.Date("1980-01-01"))%>%
  mutate(year = format(date_debut,"%Y"))%>%
  ggplot(aes(year, fill = nom_vernaculaire))+ 
  geom_bar(position = "fill")+ 
  scale_fill_grey(start = 0.2, end = 0.8)+
  theme_bw()

GeoN%>%
  filter(date_debut > as.Date("1980-01-01"))%>%
  mutate(year = format(date_debut,"%Y"))%>%
  ggplot(aes(year, fill = ordre))+ 
  geom_bar(position = "fill")+ 
#  scale_fill_grey(start = 0.1, end = 0.9)+
  theme_bw()+
  scale_fill_brewer(palette = "BrBG")


GeoN %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, 
                     breaks = seq(1980, max(year), by = 5), 
                     right = FALSE)
  ) %>%
  ggplot(aes(year_group, fill = ordre)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "BrBG")

GeoN %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, 
                     breaks = seq(1980, max(year), by = 5), 
                     right = FALSE)
  ) %>%
  ggplot(aes(year_group, fill = ordre)) +
  geom_bar() +
  scale_fill_brewer(palette = "BrBG")


GeoN %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, 
                     breaks = seq(1980, max(year), by = 5), 
                     right = FALSE)
  ) %>%
  ggplot(aes(year_group, fill = ordre)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "BrBG")


GeoN %>%
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

GeoN %>%
  filter(date_debut > as.Date("1980-01-01")) %>%
  mutate(
    year = as.numeric(format(date_debut, "%Y")),
    year_group = cut(year, 
                     breaks = seq(1980, max(year), by = 5), 
                     right = FALSE)
  ) %>%
  ggplot(aes(year_group, fill = ordre)) +
  geom_bar(position = "dodge") +
  scale_fill_grey(start = 0.2, end = 0.8)


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
  ggplot(aes(year_group, fill = ordre)) +
  geom_bar(position = "dodge") +
  scale_fill_grey(start = 0.2, end = 0.8)

RColorBrewer::display.brewer.all() 
