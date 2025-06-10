# Brouillon Semaine 2

library(readr)
library(ggplot2)
library(dplyr)
library(stringi)
library(sf)
# library(corrplot)
library(dplyr)


GeoN <- GeoN %>%
  mutate_at(c("communes", "nom_valide", 
                  "nom_vernaculaire",
                  "ordre", 
                  "famille",
                  "observateurs"), .funs = as.factor)

summary(GeoN)

ggplot(GeoN, aes(ordre))+
  geom_bar()+
  labs(title="Répartition des differents ordres",
       subtitle="GeoN")

str(GeoN)

GeoN_new <- GeoN %>%
  filter(date_debut > as.Date("2005-01-01"))

ggplot(GeoN_new, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="GeoN")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()


VisioN_FB_new <- VisioN_FB %>%
  filter(date_debut > as.Date("2005-01-01"))

ggplot(VisioN_FB_new, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="VisioN_FB")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()

summary(VisioN_FB_new)
VisioN_FB_new <- VisioN_FB_new %>%
  mutate_at(c("communes", "nom_valide", "nom_vernaculaire",
              "ordre", "famille", "observateurs"), 
            .funs = as.factor)

# On a des données a differentes périodes entre ces deux bases de données

Total_B <- VisioN_FB %>%
  bind_rows(GeoN)

Total_B_new <- Total_B %>%
  filter(date_debut > as.Date("2010-01-01"))
  
ggplot(Total_B_new, aes(date_debut))+
  geom_bar()+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total_B")+ facet_grid(ordre ~ .)+
  ylim(0, 30)+
  theme_bw()

ggplot(Total_B_new, aes(date_debut))+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total Bretagne depuis 2010")+ facet_grid(ordre ~ .)+
  theme_bw()+
  geom_line(stat="density")

ggplot(VisioN_FB_new, aes(date_debut))+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total VisioNature depuis 2010")+ facet_grid(ordre ~ .)+
  theme_bw()+
  geom_line(stat="density")

ggplot(GeoN_new, aes(date_debut))+
  labs(title="Répartition des dates selon les differents ordres",
       subtitle="Total GeoNature depuis 2010")+ facet_grid(ordre ~ .)+
  theme_bw()+
  geom_line(stat="density")

# A mon gout couper avant 2010 pour eviter de trop changer les données
# voir par rapport aux observateurs ce que ca donne 
# Ie + d'observateurs => plus d'observations car ils les voyaient sans les inscrire avant?
# Créer colonne observations = somme des observations 
# Si une observation de 2 lapins => 2 observations.

# voir corrélation sur differentes moyennes mobiles?


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
  
summary(Total_B_new2)

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


