# Brouillon Semaine 2

library(readr)
library(ggplot2)
library(dplyr)
library(stringi)
library(sf)


GeoN <- read_csv("donnees/GeoN.csv")
VisioN_FB <- read_csv("donnees/VisioN_FB.csv")


GeoN <- GeoN %>%
  mutate(across(c(communes, 
                  nom_valide, 
                  nom_vernaculaire,
                  ordre, 
                  famille,
                  observateurs), as.factor))

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
  mutate(across(c(communes, 
                  nom_valide, 
                  nom_vernaculaire,
                  ordre, 
                  famille,
                  observateurs), as.factor))

# On a des données a differentes périodes entre ces deux bases de données

Total_B <- 