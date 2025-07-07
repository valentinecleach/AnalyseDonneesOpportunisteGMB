wd <- set_wd()

source("~/work/AnalyseDonneesOpportunisteGMB/fonctions/fonctions.R")


#### Nettoyage de données ####
# Total:
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"))

#### Stats Desc ####
rmarkdown::render(paste0(wd$src, "finished/stats_desc/Repartitions.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Repartitions.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Geographie.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Geographie.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Observateurs.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Observateurs.html"))


library(formatR)
?tidy.source()


#### Classification ####

rmarkdown::render(paste0(wd$src, "finished/acp/Ordres.Rmd"), 
                  output_file = paste0(wd$output, "acp/Ordres.html"))



#### Regression ####


# Carte bretagne
setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees/DepartementsOuest")
carte_bretagne <- st_read("LIM_ADM_DepartementsOuest.shp")
carte_bretagne <- st_set_crs(carte_bretagne, 2154)
carte_bretagne <- st_transform(carte_bretagne, 4326)


# GeoNature


grid_sf <- st_sf(geometry = st_make_grid(carte_bretagne, 
                                         cellsize = c(0.15, 0.11))
)
grid_sf$density <- lengths(st_intersects(grid_sf, Total_sf))
grid_sf <- st_intersection(grid_sf, carte_bretagne)
ggplot() +
  geom_sf(data = carte_bretagne) + 
  labs(title = "Densité des observations en Bretagne") +
  geom_sf(data = grid_sf, aes(fill = density)) +
  scale_fill_gradient(low="#fbf0d1", high="#daa702") +
  theme_bw()

daa702

FFC72C