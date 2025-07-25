library(renv)
renv::repair()

loadedNamespaces()

library(dplyr)
library(sf)
library(ggplot2)

wd <- set_wd()
set.seed(12345)

summary(Total)

Total <- sf::st_read(paste0(wd$data, "derived/TotalComplet.shp"), 
                     options = "ENCODING=UTF8")
Total <- transform_Total()


Total %>%
  dplyr::distinct(cd_nom)%>%
  dplyr::select(nom_vernaculaire, cd_nom)


diro <- st_read(paste0(wd$data,"derived/diro.shp"))

# Total : 160991
# DIR Ouest : 26129

26129/160991

rm(list=setdiff(ls(), "Total"))

#### Nettoyage de donnees ####
# Total:
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Total_sites.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total_sites.html"))

#### Stats Desc ####
rmarkdown::render(paste0(wd$src, "finished/stats_desc/Repartitions.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Repartitions.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Geographie.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Geographie.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Observateurs.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Observateurs.html"))

#### Classification ####

rmarkdown::render(paste0(wd$src, 
                         "finished/acp/Ordres.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "acp/Ordres.html")
)

#### Regression ####

rmarkdown::render(paste0(wd$src, 
                         "finished/models/glm.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "models/glm/debuts.html"))

rmarkdown::render(paste0(wd$src, "brouillon/glm.Rmd"))
rmarkdown::render(paste0(wd$src, "brouillon/glm.Rmd"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Morts_Collisions.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Morts_Collision.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "brouillon/Diro_desc.Rmd"),
                  output_file = paste0(wd$output, 
                                       "stats_desc/Diro.html"),
                  encoding="UTF-8")

ggplot(grille_10x10) +
  geom_sf(aes(fill = Densite_Cultures_500m)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

ggplot(grille_10x10) +
  geom_sf(aes(fill = Distance_EcotoneArbore)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

ggplot(Total) +
  geom_sf(aes(fill = Indice_Diversite)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

###############

Total <- transform_Total(bdd = Total)
summary(Total)

Donnes_Nat <- Total%>%
  filter(technique_observation == "Vu")
corrplot::corrplot(cor(Donnes_Nat[-c("date", "nom_valide", "nom_verniculaire")]))
Donnes_Nat$

View(bdd_reg)

corrplot::corrplot(cor(bdd_reg))

Donnes_Nat$Indice_Diversite

Donnes_Nat <- Donnes_Nat %>%
  sf::st_drop_geometry()

bdd_reg <- tab_glm(Donnes_Nat, 
                   espece_interet = 61714,
                   espece_benchmark = c(61667, 61057))

summary(bdd_reg)
cor(bdd_reg[-c("clust_max", "famille_paysage_max", "Code_10km")])
corrplot::corrplot(cor(bdd_reg[c(1:3,5:7,9:12)]))

col <- colorRampPalette(c("#990000","#990000", 
                          "#eeeeee",
                          "#05600b","#05600b"))

corrplot::corrplot(cor(subset(bdd_reg, select=-c(famille_paysage_max, Code_10km))),
                   method="color", col=col(200),  
                   order="hclust", 
                   addCoef.col = "black", # Ajout du coefficient de correlation
                   tl.col="black", tl.srt=45 # Rotation des etiquettes de textes
)

t <- bdd_reg %>% dplyr::select(-clust_max, -famille_paysage_max, )
str((bdd_reg$Code_10km))

car::vif(lm(data = bdd_reg, proportion_interet ~ year + Distance_EcotoneArbore_m + Indice_Diversite_m + X_10km + Y_10km+Densite_Cultures_m))
# Des VIF correctes pour tous. On accepte toutes les propositions.

bdd_reg[4]

t <- bdd_reg %>% 
  dplyr::select(-clust_max, -famille_paysage_max, -Code_10km)%>%
  sf::st_drop_geometry()
cor(t)
str(t)
