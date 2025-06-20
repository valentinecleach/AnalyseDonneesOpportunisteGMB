
#######################################
##### Classification Observateurs #####
#######################################

On peut aussi effectuer une classification des observateurs selon les espèces observées, le nombre d’observations par taxon, etc., afin de créer une typologie des
observateurs.

summary(as.factor(Total$ordre))

Total <- Total %>%
  mutate(observateurs = toupper(observateurs))

nb_carnivora <- Total %>%
  filter(ordre=="Carnivora")%>%
  group_by(observateurs) %>%
  summarise(nb_carnivora = n(), .groups = "drop")
summary(nb_carnivora)

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

nb_observations <- Total %>%
  group_by(observateurs) %>%
  summarise(total_obs = n(), .groups = "drop")

# idées dans mon tableau: Ordre préféré.


Observateurs <- nb_observations %>%
  left_join(nb_carnivora, by = "observateurs") %>%
  left_join(nb_cetartiodactyla, by = "observateurs") %>%
  left_join(nb_eulipotyphla, by = "observateurs") %>%
  left_join(nb_lagomorpha, by = "observateurs") %>%
  left_join(nb_rodentia, by = "observateurs") %>%
  mutate_at( c("observateurs"), 
             .funs = as.factor)

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
Observateurs <- Observateurs%>%
  mutate(Pielou = ifelse(shannon==0, 0, Pielou))



Observateurs <- Observateurs %>%
  mutate(shannon = vegan::diversity(Observateurs[,3:7]),
         Pielou = shannon / log(vegan::specnumber(Observateurs[,3:7])),
         Pielou = ifelse(shannon==0, 0, Pielou))



library(Factoshiny)
PCAshiny(Observateurs)

boxplot(Observateurs$Pielou, main="Boxplot des Indice de Pielou")
summary(Observateurs$Pielou)

Observateurs <- Observateurs%>%
  mutate(Pielou = ifelse(shannon==0, 0, Pielou))

res.PCA<-PCA(Observateurs,quali.sup=c(1),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA,invisible=c('ind','ind.sup'),label =c('quali')) 
