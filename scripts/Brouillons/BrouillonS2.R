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

any(duplicated(Total_B))

####################
#################### Regarder la période ############
####################

ggplot(GeoN, aes(ordre))+
  geom_bar()+
  labs(title="Répartition des differents ordres",
       subtitle="GeoN")

ggplot(GeoN_new, aes(date_debut))+
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
  bind_rows(GeoN)

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
  to = max(Total_B_new$date_debut),
  by = "day"
))

Total_B_new <- Total_B_new %>%
  arrange(date_debut) %>%
  left_join(Total_B_new, by = "date_debut") %>%
  mutate(nombre_min = ifelse(is.na(nombre_min), 0, nombre_min))%>%
  filter(date_debut > as.Date("2010-01-01")) %>%
  # need to sum all observations if same day
  mutate(nombre_obs_jour = sum(nombre_min))
  mutate(moyenne_mobile_obs = ma(nombre_obs_jour, n=10))
  library(dplyr)
  
  
Total_B_new2 <- dates %>%
    left_join(Total_B_new, by = "date_debut") %>%
    arrange(date_debut) %>%
    mutate(nombre_min = ifelse(is.na(nombre_min), 0, nombre_min)) %>%
    filter(date_debut > as.Date("2010-01-01")) %>%
    group_by(date_debut) %>%
    summarise(nombre_obs_jour = sum(nombre_min)) %>%
    mutate(moyenne_mobile_obs = ma(nombre_obs_jour, n = 10)) %>%
    ungroup()
  

Total_B_new <- Total_B_new %>%
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


ggplot(Total_B_new, aes(date_debut,moyenne_mobile_obs))+
  geom_bar()+
  labs(title="Répartition des MM (de 10) selon les differents ordres",
       subtitle="Total_B")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()

cor(Total_B_new$date_debut, Total_B_new$moyenne_mobile_obs)

summary(Total_B_new)
ggplot(Total_B_new, aes(date_debut))+
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

