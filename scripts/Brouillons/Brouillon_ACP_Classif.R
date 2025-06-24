
#######################################
##### Classification Observateurs #####
#######################################

library(dplyr)
library(Factoshiny)
library(collapse)
library(vegan)
library(permute)

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


Total <- Total %>%
  mutate(observateurs = toupper(observateurs),
         observateurs = ifelse( observateurs == "BALLOT JEAN NOËL", 
                               "BALLOT JEAN-NOËL",
                               observateurs),
         observateurs = as.factor(replace(
           as.character(observateurs),
           which(is.na(observateurs)),
           paste("NA_", seq_len(sum(is.na(observateurs))), sep="")))
  )
#####################

nb_observations <- Total %>%
  group_by(observateurs) %>%
  summarise(total_obs = n(), .groups = "drop")

nb_ordre <- function(nom_ordre){
  nb_o <- Total %>%
    filter(ordre=="Carnivora")%>%
    group_by(observateurs) %>%
    summarise(nb_carnivora = n(), .groups = "drop")
  
  return(nb_o)
}


max_technique <- Total%>%
  group_by(observateurs) %>%
  summarise(tech_obs_max = collapse::fmode(technique_observation))

max_etat_bio <- Total%>%
  group_by(observateurs) %>%
  summarise(etat_bio_max = collapse::fmode(etat_biologique))

Observateurs <- nb_observations %>%
  left_join(nb_ordre(nom_ordre = "Carnivora"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Cetartiodactyla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Eulipotyphla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Lagomorpha"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Rodentia"), by = "observateurs")%>%
  left_join(max_technique, by = "observateurs")%>%
  left_join(max_etat_bio, by = "observateurs")

# Indice de Shannon


Observateurs <- Observateurs %>%
  mutate(nb_carnivora = ifelse(is.na(nb_carnivora), 0, nb_carnivora),
         nb_cetartiodactyla = ifelse(is.na(nb_cetartiodactyla), 0, nb_cetartiodactyla),
         nb_eulipotyphla = ifelse(is.na(nb_eulipotyphla), 0, nb_eulipotyphla),
         nb_lagomorpha = ifelse(is.na(nb_lagomorpha), 0, nb_lagomorpha),
         nb_rodentia = ifelse(is.na(nb_rodentia), 0, nb_rodentia)
  )

Observateurs$shannon <- vegan::diversity(Observateurs[,3:7])

Observateurs$Pielou <- Observateurs$shannon / log(vegan::specnumber(Observateurs[,3:7]))
Observateurs <- Observateurs %>%
  mutate(Pielou = ifelse(shannon==0, 0, Pielou))

boxplot(Observateurs$Pielou, main="Boxplot des Indice de Pielou")
summary(Observateurs$Pielou)

Observateurs <- Observateurs%>%
  mutate(prop_carnivora = nb_carnivora / total_obs,
         prop_cetartiodactyla = nb_cetartiodactyla / total_obs,
         prop_eulipotyphla = nb_eulipotyphla / total_obs,
         prop_lagomorpha = nb_lagomorpha / total_obs,
         prop_rodentia = nb_rodentia / total_obs)

rm(max_etat_bio, max_technique,
   nb_carnivora, nb_cetartiodactyla, nb_eulipotyphla,
   nb_lagomorpha, nb_observations, nb_rodentia)

PCAshiny(Observateurs)

res.PCA<-PCA(Observateurs,quali.sup=c(1),quanti.sup=c(2,8),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),habillage='cos2',label =c('quali'))
summary(res.PCA)

Factoshiny(Observateurs)

####################

# Intéressant, ceux qui ont le + sur les routes, ont peu de diversité
res.PCA<-PCA(Observateurs[,-c(1)],quali.sup=c(7,8),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),select='cos2  0.3',cex=0.5,cex.main=0.5,cex.axis=0.5,label =c('quali'))


# globalement, ceux qui observent beaucoup les carnivores ou rongeurs ont un
# indice faible d'equivalence

# graph: supp : nb de chaque, tech_obs et etat_bio
# -> ceux avec un une haute proportion de carnivores, sont 
# ceux qui trouvent morts : colision routière  mais aussi juste mort

# Ici, 80% sans les proportions -> très bon
res.PCA<-PCA(Observateurs[,-c(1)],quali.sup=c(7,8),quanti.sup=c(11,12,13,14,15),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),label =c('quali'))
# On remarque
# bcp d'obs -> bcp de "autre" (quel catégorie? etat bio ou technique obs?)
# peu obs -> entendu/ultrasons (logique)
# bcp de diversité -> non renseigné?
# peu diversité -> autre, entendu/ultrason, mort / mort routier...


# Pas vrmt de clusters visible 
# -> vaut pas le coup d'essayer de forcer des groupes.


