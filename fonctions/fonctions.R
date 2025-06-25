#' Fait un barplot selon les espèces. Les espèces sont rangés sur le plot
#'
#' @param bdd La base de donnée
#' @param nom_ordre Le nom de l'ordre parmis lesquels ont veut...
#' @param repartition ce quon veut etudier
#' @param date_min  la date minimum
#'
#' @return Une liste de plots
#' @export
#'
#' @examples
#' 
repartition_espece <- function(bdd = Total, nom_ordre, repartition, date_min = params$date_min) {
  
  pour_titre1 = deparse(substitute(repartition))
  pour_titre2 = deparse(substitute(nom_ordre))
  
  bdd_filtre <- bdd %>%
    filter(ordre == nom_ordre,
           date > params$date_min)
  
  grps <- fct_lump(bdd_filtre$nom_vernaculaire, prop = 0.03)
  
  if (length(unique(grps)) > 5) {
    bdd_filtre <- bdd_filtre %>%
      mutate(nom_vernaculaire_grp = grps)
  } else {
    bdd_filtre <- bdd_filtre %>%
      mutate(nom_vernaculaire_grp = nom_vernaculaire)
  }
  
  p <- bdd_filtre %>%
    mutate(nom_vernaculaire_grp = fct_infreq(nom_vernaculaire_grp)) %>% 
    ggplot(aes(x = {{ repartition }}, fill = nom_vernaculaire_grp)) +
    geom_bar(position = "dodge") +
    coord_flip()+
    scale_fill_discrete(labels = label_wrap(40)) +
    theme_bw()+
    labs(title = paste("Barplot de la ", 
                       pour_titre1, 
                       " des ", 
                       pour_titre2))
}

#' Applique une fonction qui plot qqe chose a tous les ordres
#'
#' @param fonction La fonction qu'on veux
#' @param repartition_cherche un parametre de la fonction d'avant
#'
#' @return Une liste de plots
#' @export
#'
#' @examples
#' 
application_tous_ordres <- function(fonction, repartition_cherche=NA){
  p1 <- fonction(nom_ordre = "Carnivora", 
                 repartition = {{repartition_cherche}})
  p2 <- fonction(nom_ordre = "Cetartiodactyla", 
                 repartition = {{repartition_cherche}})
  p3 <- fonction(nom_ordre = "Eulipotyphla", 
                 repartition = {{repartition_cherche}})
  p4 <- fonction(nom_ordre = "Lagomorpha", 
                 repartition = {{repartition_cherche}})
  p5 <- fonction(nom_ordre = "Rodentia", 
                 repartition = {{repartition_cherche}})
  return(list(p1,p2,p3,p4,p5))
}

#' Donne le nombre d'observation de chaque ordre pour chaque individu
#'
#' @param nom_ordre Le nom de l'ordre qu'on veut compter
#'
#' @return Un tibble observateurs et nombre
#' @export
#'
#' @examples
#' 
nb_ordre <- function(nom_ordre){
  nb_o <- Total %>%
    filter(ordre==nom_ordre)%>%
    group_by(observateurs) %>%
    summarise(nbr = n(), .groups = "drop")
  
  nom = paste("nb_",tolower(nom_ordre), sep="")
  nb_o <- nb_o %>% 
    rename_at("nbr",~nom)
  
  return(nb_o)
}


liste_cd_noms <- Total %>%
  group_by(cd_nom)%>%
  distinct(cd_nom)%>%
  select(cd_nom)

Observateurs_espece <- Observateurs[1]

for (i in 1:dim(liste_cd_noms)[1]){
  cd_nom_ici <- as.integer(liste_cd_noms[i, 1])
  
  nb_o <- Total %>%
    filter(cd_nom == cd_nom_ici) %>%
    group_by(observateurs) %>%
    summarise(nbr = n(), .groups = "drop") %>%
    mutate(nbr = ifelse(is.na(nbr), 0, nbr))
  
  nom_espece <- Total %>%
    filter(cd_nom == cd_nom_ici) %>%
    summarise(nom = first(nom_vernaculaire))
  
  nom <- nom_espece$nom
  nom <- paste0("nb_", gsub(" ", "", nom))
  nb_o <- nb_o %>% rename({{nom}} := nbr)
  
  Observateurs_espece <- Observateurs_espece %>%
    left_join(nb_o, by = "observateurs")
}

for (i in 2:dim(Observateurs_espece)[2]){
  Observateurs_espece[[i]] <-  ifelse(is.na(Observateurs_espece[[i]]), 
                                    0, Observateurs_espece[[i]])
}


Total %>%
  filter(communes == "Lanvéoc")%>%
  select(nom_vernaculaire, observateurs, date)%>%
  print(n=77)

liste_cd_noms <- Total %>%
  group_by(cd_nom)%>%
  distinct(cd_nom)%>%
  select(cd_nom)

Observateurs_espece <- Observateurs[1]


cd_nom_ici <- as.integer(liste_cd_noms[i, 1])

nb_o <- Total %>%
  filter(cd_nom == cd_nom_ici) %>%
  group_by(observateurs) %>%
  summarise(nbr = n(), .groups = "drop") %>%
  mutate(nbr = ifelse(is.na(nbr), 0, nbr))

str(nb_o)

nom_espece <- Total %>%
  filter(cd_nom == cd_nom_ici) %>%
  summarise(nom = first(nom_vernaculaire))

str(nom_espece)

nom <- nom_espece$nom
nom <- paste0("nb_", gsub(" ", "", nom))
nb_o <- nb_o %>% rename({{nom}} := nbr)

str(nb_o)

Observateurs_espece <- Observateurs_espece %>%
  left_join(nb_o, by = "observateurs")



for (i in 1:nrow(liste_cd_noms)) {
  cd_nom_value <- liste_cd_noms[i, 1]
  
  nb_o <- Total %>%
    filter(cd_nom == cd_nom_value) %>%
    group_by(observateurs) %>%
    summarise(nbr = n(), .groups = "drop") %>%
    mutate(nbr = ifelse(is.na(nbr), 0, nbr))
  
  nom_espece <- Total %>%
    filter(cd_nom == cd_nom_value) %>%
    summarise(nom = first(nom_vernaculaire))
  nom <- nom_espece$nom
  nom <- paste0("nb_", gsub(" ", "_", nom))
  
  nb_o <- nb_o %>% rename(!!nom := nbr)
  
  Observateurs_espece <- Observateurs_espece %>%
    left_join(nb_o, by = "observateurs")
}



