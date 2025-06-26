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




Factoshiny(Observateurs)
Factoshiny(Observateurs[,-1])


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

########################################

Observateurs_espece <- Observateurs[,1:4]

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

Observateurs_espece[,5:27] <- Observateurs_espece[,5:27] %>% replace(is.na(.), 0)

Observateurs_espece$Shannon <- vegan::diversity(Observateurs_espece[,5:27])

Observateurs_espece$Pielou <- Observateurs_espece$Shannon / log(vegan::specnumber(Observateurs_espece[,5:27]))
Observateurs_espece <- Observateurs_espece %>%
  mutate(Pielou = ifelse(Shannon==0, 0, Pielou))

boxplot(Observateurs_espece$Pielou)

colonne <- as.numeric(Observateurs_espece[[2]])
diviseur <- as.data.frame(matrix(rep(colonne, 23), 
                                 ncol = 23, 
                                 nrow = nrow(Observateurs_espece), 
                                 byrow = FALSE))


Observateurs_espece[paste(names(Observateurs_espece)[5:27], "_prop", sep="")] <- Observateurs_espece[5:27] / diviseur


PCAshiny(Observateurs_espece)


# stats desc comparaison espece et etat bio
par(mfrow = c(2, 1))

plot(Total$technique_observation)

Total%>%
  filter(bdd_originale == VN)%>%
  ggplot(aes(technique_observation, nom_vernaculaire)) + 
  geom_count()+
  theme_bw()+
  labs(subtitle = VN)+
  scale_y_discrete(labels = label_wrap(50))

Total%>%
  filter(bdd_originale == GN)%>%
  ggplot(aes(technique_observation, nom_vernaculaire)) + 
  geom_count()+
  theme_bw()+
  labs(subtitle = GN)+
  scale_y_discrete(labels = label_wrap(35))
  
Total%>%
  filter(bdd_originale == GN)%>%
  ggplot(aes(nom_vernaculaire)) + 
  geom_bar()+
  facet_grid(.~technique_observation)+
  theme_bw()+
  labs(subtitle = GN)+
  coord_flip()+
  scale_x_discrete(labels = label_wrap(35))

Total%>%
  filter(bdd_originale == VN)%>%
  ggplot(aes(nom_vernaculaire)) + 
  geom_bar()+
  facet_grid(.~technique_observation)+
  theme_bw()+
  labs(subtitle = VN)+
  coord_flip()+
  scale_x_discrete(labels = label_wrap(35))


collision_faune_diro <- read_csv("~/work/AnalyseDonneesOpportunisteGMB/donnees_brutes/collision_faune_diro.csv")
summary(collision_faune_diro$espece)

collision_faune_diro <- collision_faune_diro%>%
  mutate_at(c("route", "concessionpr", "cote", "district", "cei", "cigt", 
              "grp_espece", "espece", "commentaire", "commune"), 
            as.factor)

collision_faune_diro <- collision_faune_diro %>%
  filter(!(espece %in% c("amphibien", "amphibiens", "autre", "autre oiseau",
                         "castor", "chat", "chauve-souris", "chien", "chouette",
                         "nr", "oiseaux sauf rapace", "rapace diurne", 
                         "rapace nocturne", "rapaces nocturnes", "rapaces diurnes",
                         "reptile", "reptiles")))


str(collision_faune_diro)

collision_faune_diro <- collision_faune_diro %>%
  mutate(date = as.Date(paste(as.character(annee), mois, 01, sep="-")))

collision_faune_diro%>%
  ggplot(aes(mois))+
  labs(title="Barplot des observation selon le mois",
       y="Nombre d'observations",
       subtitle="DIRO",
       x="Mois")+ 
  theme_bw()+
  geom_bar()+
  scale_x_discrete(labels=c("01" = "Janvier", 
                            "02" = "Février",
                            "03" = "Mars",
                            "04" = "Avril",
                            "05" = "Mai",
                            "06" = "Juin",
                            "07" = "Juillet",
                            "08" = "Aout",
                            "09" = "Septembre",
                            "10" = "Octobre",
                            "11" = "Novembre",
                            "12" = "Decembre"))

collision_faune_diro%>%
  ggplot(aes(as.character(annee)))+
  labs(title="Barplot des observation selon l'année",
       y="Nombre d'observations",
       subtitle="DIRO",
       x="")+ 
  theme_bw()+
  geom_bar()


barplot(collision_faune_diro$espece)

collision_faune_diro %>%
  ggplot(aes(espece))+
  geom_bar()+
  coord_flip()

chat 199
chouette 4
renard 6339
Non renseigné 270
sanglier 3610




collision_faune_diro %>%
  mutate(espece = ifelse (espece == "vison d'amérique",
                          "Vison d'Amérique, Vison",
                          ifelse(espece == "sanglier",
                                 "Sanglier",
                                 ifelse(espece == "renard",
                                        "Renard roux, Renard, Goupil",
                                        ifelse(espece == "rat musqué",
                                               "Rat musqué",
                                               ifelse(
                                                 ...
                                               ))))))
"Vison d'Amérique, Vison" = "vison d'amérique"
"Sanglier" = "sanglier"
"Renard roux, Renard, Goupil" = "renard"
"Rat musqué" = "rat musqué"
"Rat surmulot, Surmulot, Rat d'égout" 
"Rat noir, Rat commun"
"Ragondin" = "ragondin"
"Putois d'Europe, Putois, Furet" = "putois"
"Martre des pins, Martre" = "martre"
"Loutre d'Europe, Loutre commune, Loutre" = "loutre"
"Lièvre d'Europe" = "lièvre"
"Lapin de garenne" = "lapin"
"Hermine" = "hermine"
"Hérisson d'Europe" = "hérisson"
"Fouine" = "fouine"
"Écureuil roux" = "écureuil"
"Daim européen, Daim" 
"Chevreuil européen, Chevreuil, Brocard (mâle), Chevrette (femelle)" = "chevreuil"
"Cerf élaphe" = "cerf/biche"
"Blaireau européen, Blaireau" = "blaireau"
"Belette d'Europe, Belette" = "belette"