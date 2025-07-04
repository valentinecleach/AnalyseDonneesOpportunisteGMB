rm(list=setdiff(ls(), "Total"))


#############
## Road VN ##
#############




setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees_brutes")

donnees_VisioNature_FB <- read_delim("VisioN_FB_2025-06-05T09_19_40.789Z.csv",
                                     delim = ";", 
                                     escape_double = FALSE,
                                     trim_ws = TRUE)

VM <- donnees_VisioNature_FB%>%
  mutate(etat_biologique = ifelse(champs_additionnels %in% c("{'death_cause': 'ROAD_VEHICLE'}",
                                                             "{'death_cause': 'OTHER_TRANSPORT'}",
                                                             "{'death_cause': 'UNKNOWN_TRANSPORT'}"),
                                  "Trouvé mort : impact routier",
                                  etat_biologique),
         etat_biologique = ifelse(grepl("ROUT", toupper(comment_occurrence)), 
                                  "Trouvé mort : impact routier",
                                  etat_biologique)
         )

########################
## Répartition UNKOWN ##
########################

rm(list=setdiff(ls(), "Total"))


t1 <- Total%>%
  filter(bdd_originale == "VisioNature",
         champs_additionnels == "{'death_cause': 'UNKNOWN'}")%>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+coord_flip()+labs(title = "Death_cause : UNKNOWN")

t2 <- Total%>%
  filter(bdd_originale == "VisioNature",
         etat_biologique == "Trouvé mort : impact routier")%>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+coord_flip()+labs(title = "Impacte routier")

ggarrange(t1, t2)

Total%>%
  filter(bdd_originale == "VisioNature",
         champs_additionnels == "{'death_cause': 'UNKNOWN'}")%>%
  group_by(observateurs)%>%
  filter(n()>30)%>%
  ggplot(aes(observateurs))+
  geom_bar()+coord_flip()+labs(title = "Death_cause : UNKNOWN")


res.PCA<-PCA(Observateurs[,-c(1)],quali.sup=c(2,3,4),quanti.sup=c(1,5,6,7,8,9),graph=FALSE)
plot.PCA(res.PCA,choix='var') # Corrélation negative entre Pielou, 
#                               une proportion de rongeurs elevé, et 
#                               une proportion de carnviores élevés
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),label =c('quali'))
summary(res.PCA)
# Dim 1: Shannon et Pielou
# Dim 2: Prop Carnivores et rongeurs
# Dim 3: Prop Eulipotyphles (+ rongeurs, carnivores)                       
# Dim 4: ?

res.PCA<-PCA(Observateurs[,-c(1)],quali.sup=c(2,3,4),quanti.sup=c(12,13,14,15,16),graph=FALSE)
plot.PCA(res.PCA,choix='var') # Corrélation positive entre les variables
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),label =c('quali'))
summary(res.PCA)
# Dim 1: Nombre d'observations (positif)
# Dim 2: Shannon et pielou (positif)

res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
summary(res.HCPC)
# On a 3 clusters
# Le 1er : peu de diversité et peu d'observations (surtout visioNature)
# 2e : forte diversité (surtout geonature)
# 3e : peu de diversité et de nombreuses observations (pas plus visionature ni geonature)

# On a la même chose avec (48%) 1er axe et 3e de l'ACP des espèces. 
Observateurs_espece <- Observateurs[,1:5]

liste_cd_noms <- Total %>%
  group_by(cd_nom)%>%
  distinct(cd_nom)%>%
  select(cd_nom)

for (i in 1:nrow(liste_cd_noms)) {
  cd_nom_value <- as.integer(liste_cd_noms[i,])
  
  nb_o <- Total %>%
    filter(cd_nom == cd_nom_value) %>%
    group_by(observateurs) %>%
    summarise(nbr = n(), .groups = "drop") %>%
    mutate(nbr = ifelse(is.na(nbr), 0, nbr))
  
  nom_espece <- Total %>%
    filter(cd_nom == cd_nom_value) %>%
    summarise(nom = first(nom_vernaculaire))
  nom <- nom_espece$nom
  nom <- paste0("nb_", gsub(" ", "", nom))
  
  nb_o <- nb_o %>% rename({{nom}} := nbr)
  
  Observateurs_espece <- Observateurs_espece %>%
    left_join(nb_o, by = "observateurs")
}

Observateurs_espece[,6:28] <- Observateurs_espece[,6:28] %>% replace(is.na(.), 0)

Observateurs_espece$Shannon <- vegan::diversity(Observateurs_espece[,6:28])

Observateurs_espece$Pielou <- Observateurs_espece$Shannon / log(vegan::specnumber(Observateurs_espece[,6:28]))
Observateurs_espece <- Observateurs_espece %>%
  mutate(Pielou = ifelse(Shannon==0, 0, Pielou))

colonne <- as.numeric(Observateurs_espece[[2]])
diviseur <- as.data.frame(matrix(rep(colonne, 23), 
                                 ncol = 23, 
                                 nrow = nrow(Observateurs_espece), 
                                 byrow = FALSE))

Observateurs_espece[paste(names(Observateurs_espece)[6:28], "_prop", sep="")] <- Observateurs_espece[6:28] / diviseur


str(Observateurs_espece)
PCAshiny(Observateurs_espece)



res.PCA<-PCA(Observateurs[,-c(1)],quali.sup=c(2,3,4),quanti.sup=c(12,13,14,15,16),graph=FALSE)
plot.PCA(res.PCA,choix='var') # Corrélation positive entre les variables
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),label =c('quali'))
summary(res.PCA)
# Dim 1: Nombre d'observations (positif)
# Dim 2: Shannon et pielou (positif)

res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
summary(res.HCPC)
# On a 3 clusters
# Le 1er : peu de diversité et peu d'observations (surtout visioNature)
# 2e : forte diversité (surtout geonature)
# 3e : peu de diversité et de nombreuses observations (pas plus visionature ni geonature)

res.HCPC$data.clust # retourne toutes les valeurs et l'appartenance au cluster pour chaque individu
res.HCPC$data.clust[1,]$clust # pour le premier individu
table(res.HCPC$data.clust$clust) # donne le tableau des fréquences par cluster

Observateurs_clust <- res.HCPC$data.clust%>%
  mutate(observateurs = Observateurs$observateurs)

Total_clust <- Total %>%
  mutate(clust = case_when(
    Observateurs_clust$clust == 1 ~ 1,
    Observateurs_clust$clust == 2 ~ 2,
    Observateurs_clust$clust == 3 ~ 3,
    .default = NA
  ))

str(Total)
str(Observateurs_clust)

library(dplyr)

Total <- Total %>%
  filter(!(observateurs %in% c("CHAPUIS_MARTINE", "BELLIER_DANIEL")))%>%
  left_join(
    Observateurs_clust %>% select(observateurs, clust),
    by = "observateurs"
  )

summary(Total$clust)

Cluster_1 <- Total %>%
  filter(clust == 1)

Cluster_1 %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n)))+
  geom_bar()+
  scale_x_discrete(labels = label_wrap(40)) +
  coord_flip()+theme_bw()

Total%>%
  filter(clust == 1)%>%
  count(nom_vernaculaire, sort = TRUE) %>%
  ggplot(aes(x = reorder(nom_vernaculaire, n), 
             y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Répartition des espèces du Cluster 1",
    x = "Nom vernaculaire"
  ) +
  scale_x_discrete(labels = label_wrap(40)) +
  theme_bw()+
  theme(axis.text=element_text(size=8))

repartition_espece_cluster <- function(bdd = Total, cluster){
  bdd %>%
    filter(clust == cluster) %>%
    count(nom_vernaculaire, sort = TRUE) %>%
    ggplot(aes(x = reorder(nom_vernaculaire, n), 
               y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Répartition des espèces du cluster ", cluster),
      x = "Nom vernaculaire"
    ) +
    scale_x_discrete(labels = label_wrap(40)) +
    theme_bw() +
    theme(axis.text=element_text(size=8))
}
repartition_espece_cluster(cluster = 1)
repartition_espece_cluster(cluster = 2)
repartition_espece_cluster(cluster = 3)


#############
### DIRO ####
#############

collision_faune_diro <- read_csv("~/work/AnalyseDonneesOpportunisteGMB/donnees_brutes/collision_faune_diro.csv")


diro <- collision_faune_diro%>%
  mutate(
    geom_x = as.numeric(str_match(geom, "POINT \\(([^ ]+)")[,2]),
    geom_y = as.numeric(str_match(geom, "POINT \\([^ ]+ ([^\\)]+)")[,2])
  )


diro <- diro %>%
  mutate_at(c("route", "concessionpr", "cote", "district", "cei", "cigt", 
              "grp_espece", "espece", "commentaire", "commune"), 
            as.factor)%>%
  filter(!(espece %in% c("amphibien", "amphibiens", "autre", "autre oiseau",
                         "castor", "chat", "chauve-souris", "chien", "chouette",
                         "nr", "oiseaux sauf rapace", "rapace diurne", 
                         "rapace nocturne", "rapaces nocturnes", "rapaces diurnes",
                         "reptile", "reptiles")),
         !is.na(geom))%>%
  mutate(date = as.Date(paste(as.character(annee), mois, 01, sep="-")),
         espece = ifelse(commentaire == "Furet", 
                         "Putois d'Europe, Putois, Furet", espece))%>%
  mutate(bdd_originale = "DIRO",
         technique_observation = "Vu",
         etat_biologique = "Trouvé mort : impact routier")%>%
  select(-date_maj, -annee, -mois, -commentaire, -grp_espece)%>%
  mutate_at(c("route", "concessionpr", "cote", "district", "cei", "cigt", 
              "espece", "commune"), 
            as.factor) 

diro_sf <- st_as_sf(diro, 
                    coords = c("geom_x", "geom_y"), 
                    crs = 2154)

diro_sf <- st_transform(diro_sf, 
                        st_crs(4326))

carte_bretagne_44 <- st_read("~/work/AnalyseDonneesOpportunisteGMB/donnees/Masque_Bretagne_Continentale/Masque_Bretagne_Continentale.shp") %>%
  st_transform(2154)%>%
  st_transform(st_crs(4326))

carte_44 <- st_read("~/work/AnalyseDonneesOpportunisteGMB/donnees/Masque_44/Masque44.shp") %>%
  st_transform(2154) %>%
  st_transform(st_crs(4326))

total_bretagne_44 <- st_filter(diro_sf, 
                               carte_bretagne_44, 
                               .predicate = st_within)
total_44 <- st_filter(diro_sf, 
                      carte_44, 
                      .predicate = st_within)

total_44_id <- total_44 %>%
  st_drop_geometry() %>%
  select(id)

diro_sf <- anti_join(total_bretagne_44, 
                     total_44_id, 
                     by = "id")

ggplot() +
  geom_sf(data = carte_bretagne_44) +
  geom_sf(data = diro_sf)

diro <- st_drop_geometry(diro_sf)

