library(readr)
library(ggplot2)
library(dplyr)
library(stringi)
library(stringr)
library(sf)
library(rsample)
library(patchwork)
library(scales)


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
