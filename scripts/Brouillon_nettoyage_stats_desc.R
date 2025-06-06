library(readr)
library(dplyr)
library(stringi)
library(sf)

data_original <- read_delim("GeoN_22-29_2025-06-05T09_18_37.225Z.csv", 
                            delim = ";", 
                            escape_double = FALSE, 
                            trim_ws = TRUE)

GeoN_22_29 <- data_original%>%
  select("id_synthese",
         "date_debut", 
         "cd_nom", 
         "nom_valide",	"nom_vernaculaire",
         "ordre",
         "famille",	"rang_taxo",
         "nombre_min",
         "nombre_max",
         "observateurs",
         "communes",	
         "geometrie_wkt_4326",	
         "x_centroid_4326",
         "y_centroid_4326",
         "comment_occurrence",
         "niveau_validation",
         "jdd_nom",
         "jdd_uuid",
         "jdd_id",
         "ca_nom"	,
         "ca_uuid",	
         "ca_id",
         "technique_observation",
         "etat_biologique"
  )

GeoN_22_29 <- GeoN_22_29 %>%
  mutate(date_debut = as.Date(date_debut),
         cd_nom = as.factor(cd_nom),
         ordre = as.factor(ordre),
         famille = as.factor(famille),
         technique_observation,
         etat_biologique=as.factor(etat_biologique),
         jdd_nom = as.factor(jdd_nom)
  )%>%
  filter(jdd_nom !=  "Données faunebretagne.org" )

# regarder ce qu'il y a sur 


GeoN_22_29%>%
  select(observateurs, nombre_min, nombre_max)%>%
  filter(nombre_max != nombre_min)
# Seule Enora observe des animaux sans savoir combien ils sont

GeoN_22_29%>%
  select(nom_valide, nom_vernaculaire)%>%
  filter(is.na(nom_vernaculaire))


GeoN_22_29 <- GeoN_22_29 %>%
  mutate(
    nom_vernaculaire = ifelse(
      is.na(nom_vernaculaire),
      nom_valide,
      nom_vernaculaire
    ),
    nom_valide = as.factor(nom_valide),
    nom_vernaculaire = as.factor(nom_vernaculaire),
    nombre_min = ifelse(
      nombre_max != nombre_min,
      median(c(nombre_max, nombre_min)),
      nombre_min
    )
  )


# Putain y'a un loup dans le jardin quoi
# qqun a mis 1000 lapins d'un coup ca m'embête


GeoN_22_29 <- GeoN_22_29 %>%
  mutate(technique_observation = ifelse(
    technique_observation %in% c("Fèces/Guano/Epreintes", 
                                 "Empreintes", "Coulée",
                                 "Galerie/terrier", 
                                 "Hutte (Castor, Rat musqué)", "Nid/Gîte",
                                 "Indices de présence divers",
                                 "Restes de repas",
                                 "Empreintes et fèces",
                                 "Restes dans pelote de réjection"
    ),
    "Indices",
    ifelse(technique_observation %in% c("Entendu", "Ultrasons"), 
           "Entendu/Ultasons",
           technique_observation)
  ))

summary(as.factor(GeoN_22_29$technique_observation))

summary(GeoN_22_29$etat_biologique)


# pas le nombre, es ce quon a vu ou pas. 
# Composition 
# Technique_obs et etat_biologique.

# Representation graphique du territroire.

GeoN_22_29%>%
  filter(nombre_min==1000)%>%
  select(nombre_min, observateurs)
# Données sur les iles. Peut etre les exclure. Pas de prédateurs.


data_original_35_56 <- read_delim("GeoN_35-56_2025-06-05T09_21_08.380Z.csv", 
                                  delim = ";", 
                                  escape_double = FALSE, 
                                  trim_ws = TRUE)
GeoN_34_56 <- data_original_35_56%>%
  select("id_synthese",
         "date_debut", 
         "cd_nom", 
         "nom_valide",	"nom_vernaculaire",
         "ordre",
         "famille",	"rang_taxo",
         "nombre_min",
         "nombre_max",
         "observateurs",
         "communes",	
         "geometrie_wkt_4326",	
         "x_centroid_4326",
         "y_centroid_4326",
         "comment_occurrence",
         "niveau_validation",
         "jdd_nom",
         "jdd_uuid",
         "jdd_id",
         "ca_nom"	,
         "ca_uuid",	
         "ca_id",
         "technique_observation",
         "etat_biologique"
  )%>%
  mutate(date_debut = as.Date(date_debut),
         cd_nom = as.factor(cd_nom),
         ordre = as.factor(ordre),
         famille = as.factor(famille),
         technique_observation,
         etat_biologique=as.factor(etat_biologique),
         jdd_nom = as.factor(jdd_nom)
  )%>%
  filter(jdd_nom !=  "Données faunebretagne.org" )%>%
  mutate(
    nom_vernaculaire = ifelse(
      is.na(nom_vernaculaire),
      nom_valide,
      nom_vernaculaire
    ),
    nom_valide = as.factor(nom_valide),
    nom_vernaculaire = as.factor(nom_vernaculaire),
    nombre_min = ifelse(nombre_max != nombre_min, 
                        median(c(nombre_max,nombre_min)),
                        nombre_min)
  )%>%  
  mutate(technique_observation = ifelse(
    technique_observation %in% c("Fèces/Guano/Epreintes", 
                                 "Empreintes", "Coulée",
                                 "Galerie/terrier", 
                                 "Hutte (Castor, Rat musqué)", "Nid/Gîte",
                                 "Indices de présence divers",
                                 "Restes de repas",
                                 "Empreintes et fèces",
                                 "Restes dans pelote de réjection"
    ),
    "Indices",
    ifelse(technique_observation %in% c("Entendu", "Ultrasons"), 
           "Entendu/Ultasons",
           technique_observation)
  ))%>%
  mutate(technique_observation = as.factor(technique_observation))
summary(GeoN_34_56)

GeoN <- merge(as.dataframe(GeoN_22_29[4,4]), as.df(GeoN_34_56[4,4]))
GeoN <- rbind(GeoN_22_29, GeoN_34_56)
summary(GeoN)

data_original_44 <- read_delim("GeoN_44_2025-06-05T09_22_40.948Z.csv", 
                               delim = ";", 
                               escape_double = FALSE, 
                               trim_ws = TRUE)
GeoN_44 <- data_original_44%>%
  select("id_synthese",
         "date_debut", 
         "cd_nom", 
         "nom_valide",	"nom_vernaculaire",
         "ordre",
         "famille",	"rang_taxo",
         "nombre_min",
         "nombre_max",
         "observateurs",
         "communes",	
         "geometrie_wkt_4326",	
         "x_centroid_4326",
         "y_centroid_4326",
         "comment_occurrence",
         "niveau_validation",
         "jdd_nom",
         "jdd_uuid",
         "jdd_id",
         "ca_nom"	,
         "ca_uuid",	
         "ca_id",
         "technique_observation",
         "etat_biologique"
  )%>%
  mutate(date_debut = as.Date(date_debut),
         cd_nom = as.factor(cd_nom),
         ordre = as.factor(ordre),
         famille = as.factor(famille),
         technique_observation,
         etat_biologique=as.factor(etat_biologique),
         jdd_nom = as.factor(jdd_nom)
  )%>%
  filter(jdd_nom !=  "Données faunebretagne.org" )%>%
  mutate(
    nom_vernaculaire = ifelse(
      is.na(nom_vernaculaire),
      nom_valide,
      nom_vernaculaire
    ),
    nom_valide = as.factor(nom_valide),
    nom_vernaculaire = as.factor(nom_vernaculaire),
    nombre_min = ifelse(nombre_max != nombre_min, 
                        median(c(nombre_max,nombre_min)),
                        nombre_min)
  )%>%  
  mutate(technique_observation = ifelse(
    technique_observation %in% c("Fèces/Guano/Epreintes", 
                                 "Empreintes", "Coulée",
                                 "Galerie/terrier", 
                                 "Hutte (Castor, Rat musqué)", "Nid/Gîte",
                                 "Indices de présence divers",
                                 "Restes de repas",
                                 "Empreintes et fèces",
                                 "Restes dans pelote de réjection"
    ),
    "Indices",
    ifelse(technique_observation %in% c("Entendu", "Ultrasons"), 
           "Entendu/Ultasons",
           technique_observation)
  ))%>%
  mutate(technique_observation = as.factor(technique_observation))
summary(GeoN_44)

GeoN <- rbind(GeoN, GeoN_44)
summary(GeoN)

GeoN$communes

barplot(GeoN$technique_observation)

library(ggplot2)
ggplot(GeoN, aes(technique_observation, fill=ordre))+
  geom_bar(position="dodge")+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red")+
  labs(title = "Barplot de la technique d'observation selon l'ordre")

ggplot(GeoN, aes(technique_observation, fill=etat_biologique
))+
  geom_bar(position="dodge")+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red")+
  labs(title = "Barplot de la techique d'observation selon l'état biologique")
GeoN$date_debut

GeoN_sans_1000 <- GeoN%>%
  filter(nombre_min!=1000)

ggplot(GeoN_sans_1000, aes(date_debut, nombre_min))+
  geom_point()

ggplot(GeoN, aes(date_debut))+
  geom_histogram()+
  labs(title = "Distribution des la quantité d'observations",
       x = "date")

donnes_VisioNature_FB <- read_delim("VisioN_FB_2025-06-05T09_19_40.789Z.csv",
                                    delim = ";", 
                                    escape_double = FALSE,
                                    trim_ws = TRUE)
str(donnes_VisioNature_FB)

VisioN_FB <- donnes_VisioNature_FB %>%
  dplyr::select(id_synthese,
                date_debut,
                cd_nom,
                cd_ref,
                nom_valide,
                nom_vernaculaire,
                ordre,
                famille,
                rang_taxo,
                nombre_min, # nbre_min=nbre_max
                observateurs,
                communes,
                geometrie_wkt_4326,
                x_centroid_4326,
                y_centroid_4326,
                comment_occurrence,
                technique_observation,
                etat_biologique
  )%>%
  dplyr::mutate(etat_biologique = as.factor(etat_biologique),
                nom_valide=as.factor(nom_valide),
                nom_vernaculaire=as.factor(nom_vernaculaire),
                ordre = as.factor(ordre),
                famille = as.factor(famille),
                rang_taxo=as.factor(rang_taxo)
  )%>%
  dplyr::filter(technique_observation != "Restes dans pelote de réjection")


VisioN_FB <- VisioN_FB%>%
  dplyr::mutate(technique_observation = ifelse(
    !is.na(comment_occurrence) & technique_observation == "Inconnu",
    ifelse(any(grepl("TAUPINIERE", toupper(gsub("[[:punct:]]", 
                                                "", 
                                                stri_trans_general(
                                                  comment_occurrence, 
                                                  "Latin-ASCII")
    )
    )
    ),
    grepl("EMPREINTES", toupper(gsub(":punct:]]",
                                     "",
                                     stri_trans_general(
                                       comment_occurrence, 
                                       "Latin-ASCII")
    ))
    ),
    grepl("TRACE", toupper(gsub(":punct:]]",
                                "",
                                stri_trans_general(
                                  comment_occurrence, 
                                  "Latin-ASCII"
                                )
    )
    )
    )
    ),
    "Indices",
    technique_observation),
    technique_observation
  )
  )

"""
VisioN_FB <- VisioN_FB %>%
  dplyr::mutate(
    technique_observation = ifelse(
      !is.na(comment_occurrence) & technique_observation == "Inconnu" &
        grepl("TAUPINIERE|EMPREINTES",
              toupper(gsub("[[:punct:]]", 
                           "", 
                           stri_trans_general(comment_occurrence, 
                                              "Latin-ASCII")
              )
              )
        ),
      "Indices",
      technique_observation
    )
  )

VisioN_FB <- VisioN_FB %>%
  dplyr::mutate(comment_propre = toupper(gsub("[[:punct:]]", 
                                   "", 
                                   stri_trans_general(
                                     comment_occurrence, 
                                     "Latin-ASCII")
                                   )
                              )
     ) %>%
  mutate(technique_observation = ifelse(
    !is.na(comment_occurrence) & 
      technique_observation == "Inconnu",
    ifelse(grepl("TAUPINIERE",comment_propre),
           "Indice",
           technique_observation),
    ifelse(grepl("EMPREINTES",comment_propre),
           "Indice",
           technique_observation)
    ))
    
test <- VisioN_FB%>%
  dplyr:: mutate(technique_observation = as.factor(technique_observation),
                comment_occurrence = as.factor(comment_occurrence))%>%
  dplyr:: filter(technique_observation == "Inconnu")
"""

summary(test)

sum(!is.na(test$comment_occurrence))

ggplot(VisioN_FB, aes(date_debut))+
  geom_histogram()+
  labs(title = "Distribution des la quantité d'observations",
       x = "date")+theme_bw()

ggplot(VisioN_FB, aes(technique_observation, fill=ordre))+
  geom_bar(position="dodge")+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red")+
  labs(title = "Barplot de la technique d'observation selon l'ordre")

ggplot(VisioN_FB, aes(technique_observation, fill=etat_biologique
))+
  geom_bar(position="dodge")+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red")+
  labs(title = "Barplot de la techique d'observation selon l'état biologique")

carte_bretagne <- st_read("LIM_ADM_DepartementsOuest.shp")


taille_lots_sf <- st_as_sf(, wkt = "geometrie")

ggplot() +
  geom_sf(data = carte_bretagne) + 
  theme_bw() +
  labs(title = "Carte de la Bretagne selon...") +
  geom_sf(data =  , aes(color = ), size = 1)
