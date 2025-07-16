Avant_2018 <- Total %>%
  st_drop_geometry() %>%
  filter(date < as.Date("2018-01-01"))

Apres_2018 <- Total %>%
  st_drop_geometry() %>%
  filter(date >= as.Date("2018-01-01"))

median(Total$date)
mean(Total$date)
Apres_2018$paysage_ID


x <- as.numeric(liste_cd_nom[[1]][17])


Avant_2018 %>%
  filter(cd_nom == x) %>%
  group_by(paysage_ID) %>%
  distinct(paysage_ID) %>%
  n_distinct()

Apres_2018 %>%
  filter(cd_nom == x) %>%
  group_by(paysage_ID) %>%
  distinct(paysage_ID) %>%
  n_distinct()

x
Total%>%
  filter(cd_nom == x)%>%
  distinct(nom_vernaculaire)

# different pour 60674 : Fouine
# Avant: 39, Apres: 36

# different pour 60015 : Herisson d'Europe
# Avant: 37, Apres: 38

# different pour 61000 : Cerf elaphe
# Avant: 21, Apres: 23

# different pour 61153 : Ecureuil roux
# Avant: 39, Apres: 38

# different pour 60630 : Loutre
# Avant: 33, Apres: 34

# different pour 60746 : Vison
# Avant: 31, Apres: 27

# different pour 60686 : Hermine
# Avant: 31, Apres: 21

# different pour 61028 : Daim
# Avant: 4, Apres: 7

Total%>%
  distinct(60577)

?seq
mot <- c()
db <- as.data.frame(seq(NA,))
for (i in 1:23){
  cd_nom_i = as.numeric(liste_cd_nom[[1]][i])
  
  avant <- Avant_2018 %>%
    filter(cd_nom == cd_nom_i) %>%
    group_by(paysage_ID) %>%
    distinct(paysage_ID) %>%
    n_distinct()
  
  apres <- Apres_2018 %>%
    filter(cd_nom == cd_nom_i) %>%
    group_by(paysage_ID) %>%
    distinct(paysage_ID) %>%
    n_distinct()
  
  if (avant != apres) {
    nom <- Total %>%
      filter(cd_nom == cd_nom_i) %>%
      distinct(nom_vernaculaire)
    
    nom <- as.character(nom[[1]][1]) 
    cbind(avant, apres, cd_nom_i, nom)
  }
}

nom <- Total %>%
  filter(cd_nom == 60577) %>%
  distinct(nom_vernaculaire)
as.character(nom)
as.character(nom[[1]][1])



