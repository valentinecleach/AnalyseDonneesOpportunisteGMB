# Brouillon S4

summary(as.factor(Total$nom_vernaculaire))



library(slider)
slide_mean()

Total%>%
  filter(date > params$date_min,
         etat_biologique == "Trouvé mort : impact routier")%>%
  mutate(
    year = as.numeric(format(date, "%Y")),
    year_group = cut(year, 
                     breaks = seq(1980, max(year), by = 1), 
                     right = FALSE),
    nom_vernaculaire_grp = fct_lump(nom_vernaculaire, prop = 0.02)
  ) %>%
  ggplot(aes(year_group, fill = nom_vernaculaire_grp)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))


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

NA_table <- Total%>%
  filter(is.na(observateurs))
view(NA_table)
summary(NA_table)

view(Total)
view(Total%>%
  arrange(observateurs)
)

nb_observations <- Total %>%
  group_by(observateurs) %>%
  summarise(total_obs = n(), .groups = "drop")

max_technique <- Total%>%
  group_by(observateurs) %>%
  summarise(tech_obs_max = collapse::fmode(technique_observation))

max_etat_bio <- Total%>%
    group_by(observateurs) %>%
  summarise(etat_bio_max = collapse::fmode(etat_biologique))

Observateurs <- nb_observations %>%
  left_join(max_technique, by = "observateurs")%>%
  left_join(max_etat_bio, by = "observateurs")%>%
  left_join(nb_ordre(nom_ordre = "Carnivora"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Cetartiodactyla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Eulipotyphla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Lagomorpha"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Rodentia"), by = "observateurs")

Observateurs <- Observateurs %>%
  mutate(nb_carnivora = ifelse(is.na(nb_carnivora), 0, nb_carnivora),
         nb_cetartiodactyla = ifelse(is.na(nb_cetartiodactyla), 0, nb_cetartiodactyla),
         nb_eulipotyphla = ifelse(is.na(nb_eulipotyphla), 0, nb_eulipotyphla),
         nb_lagomorpha = ifelse(is.na(nb_lagomorpha), 0, nb_lagomorpha),
         nb_rodentia = ifelse(is.na(nb_rodentia), 0, nb_rodentia)
  )

Observateurs$shannon <- vegan::diversity(Observateurs[,5:9])

Observateurs$Pielou <- Observateurs$shannon / log(vegan::specnumber(Observateurs[,5:9]))
Observateurs <- Observateurs %>%
  mutate(Pielou = ifelse(shannon==0, 0, Pielou))

Observateurs <- Observateurs%>%
  mutate(prop_carnivora = nb_carnivora / total_obs,
         prop_cetartiodactyla = nb_cetartiodactyla / total_obs,
         prop_eulipotyphla = nb_eulipotyphla / total_obs,
         prop_lagomorpha = nb_lagomorpha / total_obs,
         prop_rodentia = nb_rodentia / total_obs,
         tech_obs_max = paste("Tech d'obs: ", tech_obs_max, sep=""),
         etat_bio_max = paste("Etat bio: ", etat_bio_max, sep="")
  )



boxplot(Observateurs$Pielou, 
        main="Répartition des Indice de Pielou",
        ylab="Indice de Pielou")

summary(Observateurs$Pielou)

Observateurs <- Observateurs %>%
  filter(observateurs != "BELLIER DANIEL")



Factoshiny(Observateurs)
Factoshiny(Observateurs[,-1])

t <- Total%>%
  select(nom_vernaculaire, cd_nom)

summary(as.factor(Total$ordre))
summary(Total%>%
      mutate_all(as.factor)%>%
      select(nom_vernaculaire, cd_nom)%>%
        arrange(desc(cd_nom)))
Total%>%
  filter(ordre == "Carnivora")%>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+
  coord_flip()

Total %>%
  filter(nom_vernaculaire == "Genette commune, Genette")%>%
  select(date, cd_nom, nom_valide, nom_vernaculaire, observateurs, communes, 
         etat_biologique, technique_observation, bdd_originale)

99999004 # hermine / belette -> on sait pas
99999005 # Puton, Putois / Vison
99999006 # Vison d'europe ou d'amérique?

60831 # Genette : Introduit
60582 # chien viverrin : Ne peux pas se reproduire seul
60822 # Raton laveur : Introduit evahissante
60579 # Chacal doré
199752 # Putois domestique : introduit eteinte


Total %>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+
  coord_flip()


Total %>%
  filter(nom_vernaculaire == "Hermine")%>%
  select(cd_nom, nom_valide, observateurs, communes, bdd_originale)%>%
  print(n=20)

Total %>%
  filter(nom_vernaculaire == "Hérisson d'Europe",
         technique_observation == "Entendu/Ultrasons")%>%
  select(date, cd_nom, nom_valide, observateurs, communes, bdd_originale)%>%
  print(n=20)

Total <- Total %>%
  mutate(technique_observation = ifelse(cd_nom == 60015, "Vu", technique_observation))

Total %>%
  filter(technique_observation == 2)

#############
nb_observations <- Total %>%
  group_by(observateurs) %>%
  summarise(total_obs = n(), .groups = "drop")

max_technique <- Total%>%
  group_by(observateurs) %>%
  summarise(tech_obs_max = collapse::fmode(technique_observation))

max_etat_bio <- Total%>%
  group_by(observateurs) %>%
  summarise(etat_bio_max = collapse::fmode(etat_biologique))

Observateurs <- nb_observations %>%
  left_join(max_technique, by = "observateurs")%>%
  left_join(max_etat_bio, by = "observateurs")%>%
  left_join(nb_ordre(nom_ordre = "Carnivora"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Cetartiodactyla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Eulipotyphla"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Lagomorpha"), by = "observateurs") %>%
  left_join(nb_ordre(nom_ordre = "Rodentia"), by = "observateurs")

Observateurs <- Observateurs %>%
  mutate(nb_carnivora = ifelse(is.na(nb_carnivora), 0, nb_carnivora),
         nb_cetartiodactyla = ifelse(is.na(nb_cetartiodactyla), 0, nb_cetartiodactyla),
         nb_eulipotyphla = ifelse(is.na(nb_eulipotyphla), 0, nb_eulipotyphla),
         nb_lagomorpha = ifelse(is.na(nb_lagomorpha), 0, nb_lagomorpha),
         nb_rodentia = ifelse(is.na(nb_rodentia), 0, nb_rodentia)
  )
str(Observateurs[,5:9])
Observateurs$shannon <- vegan::diversity(Observateurs[,5:9])

Observateurs$Pielou <- Observateurs$shannon / log(vegan::specnumber(Observateurs[,5:9]))
Observateurs <- Observateurs %>%
  mutate(Pielou = ifelse(shannon==0, 0, Pielou))

Observateurs <- Observateurs%>%
  mutate(prop_carnivora = nb_carnivora / total_obs,
         prop_cetartiodactyla = nb_cetartiodactyla / total_obs,
         prop_eulipotyphla = nb_eulipotyphla / total_obs,
         prop_lagomorpha = nb_lagomorpha / total_obs,
         prop_rodentia = nb_rodentia / total_obs,
         tech_obs_max = paste("Tech d'obs: ", tech_obs_max, sep=""),
         etat_bio_max = paste("Etat bio: ", etat_bio_max, sep="")
  )

########################################
Observateurs_espece <- Observateurs[,1:4]

liste_cd_noms <- Total %>%
  group_by(cd_nom)%>%
  distinct(cd_nom)%>%
  select(cd_nom)

as.integer(liste_cd_noms[1,])

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

Observateurs_espece[,5:27] <- Observateurs_espece[,5:27] %>% replace(is.na(.), 0)

Observateurs_espece$Shannon <- vegan::diversity(Observateurs_espece[,5:27])

Observateurs_espece$Pielou <- Observateurs_espece$Shannon / log(vegan::specnumber(Observateurs_espece[,5:27]))
Observateurs_espece <- Observateurs_espece %>%
  mutate(Pielou = ifelse(Shannon==0, 0, Pielou))

boxplot(Observateurs_espece$Pielou)

t <- Observateurs_espece%>%
  filter(Pielou>0.9)


View(t)

PCAshiny(Observateurs_espece)

Observateurs <- Observateurs %>%
  mutate(nb_carnivora = ifelse(is.na(nb_carnivora), 0, nb_carnivora),
         nb_cetartiodactyla = ifelse(is.na(nb_cetartiodactyla), 0, nb_cetartiodactyla),
         nb_eulipotyphla = ifelse(is.na(nb_eulipotyphla), 0, nb_eulipotyphla),
         nb_lagomorpha = ifelse(is.na(nb_lagomorpha), 0, nb_lagomorpha),
         nb_rodentia = ifelse(is.na(nb_rodentia), 0, nb_rodentia)
  )