
#######################################
##### Classification Observateurs #####
#######################################

library(dplyr)
library(tibble)

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


nb_observations <- Total %>%
  group_by(observateurs) %>%
  summarise(total_obs = n(), .groups = "drop")

nb_carnivora <- Total %>%
  filter(ordre=="Carnivora")%>%
  group_by(observateurs) %>%
  summarise(nb_carnivora = n(), .groups = "drop")

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


Observateurs <- nb_observations %>%
  left_join(nb_carnivora, by = "observateurs") %>%
  left_join(nb_cetartiodactyla, by = "observateurs") %>%
  left_join(nb_eulipotyphla, by = "observateurs") %>%
  left_join(nb_lagomorpha, by = "observateurs") %>%
  left_join(nb_rodentia, by = "observateurs")

#Indice de Shannon
library(vegan)
library(permute)

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

summary(Observateurs$shannon)

summary(Observateurs$Pielou)

library(Factoshiny)
PCAshiny(Observateurs)

boxplot(Observateurs$Pielou, main="Boxplot des Indice de Pielou")
summary(Observateurs$Pielou)

haut_pielou <- Observateurs%>%
  filter(Pielou >= 0.9)


summary(haut_pielou)
PCAshiny(haut_pielou)


haut_pielou%>%
  filter(observateurs == "BALLOT JEAN-NOËL")


res.PCA<-PCA(haut_pielou,quali.sup=c(1),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),label =c('quali')) 


res.PCA<-PCA(Observateurs,quali.sup=c(1),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),label =c('quali')) 

res.PCA<-PCA(Observateurs,quali.sup=c(1),quanti.sup=c(2,8),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),habillage='cos2',label =c('quali'))
res.PCA<-PCA(Observateurs,quali.sup=c(1),quanti.sup=c(2,8),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),habillage='cos2',label ='none')
summary(res.PCA)

Factoshiny(Observateurs)

Observateurs %>%
  filter(is.na(observateurs))
