#' Fait un barplot selon les especes. Les especes sont ranges sur le plot
#'
#' @param bdd La base de donnee
#' @param nom_ordre Le nom de l'ordre parmi lesquels ont veut...
#' @param repartition ce qu'on veut etudier
#' @param date_min  la date minimum
#'
#' @return Une liste de plots
#' @export
#'
#' @examples
#' 
repartition_espece <- function(bdd = Total, nom_ordre, 
                               repartition, date_min = params$date_min) {
  
  pour_titre1 = deparse(substitute(repartition))
  pour_titre2 = deparse(substitute(nom_ordre))
  
  bdd_filtre <- bdd %>%
    dplyr::filter(ordre == nom_ordre,
           date > params$date_min)
  
  grps <- fct_lump(bdd_filtre$nom_vernaculaire, prop = 0.03)
  
  if (length(unique(grps)) > 5) {
    bdd_filtre <- bdd_filtre %>%
      dplyr::mutate(nom_vernaculaire_grp = grps)
  } else {
    bdd_filtre <- bdd_filtre %>%
      dplyr::mutate(nom_vernaculaire_grp = nom_vernaculaire)
  }
  
  p <- bdd_filtre %>%
    dplyr::mutate(nom_vernaculaire_grp = fct_infreq(nom_vernaculaire_grp)) %>% 
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


stats_observateur <- function(nom_obs, carte){
  
  # Date
  p1 <- Total %>%
    dplyr::filter(observateurs == toupper(nom_obs),
           date > params$date_min) %>%
    ggplot(aes(date)) +
    labs(title = "Repartition des dates",
         subtitle = paste("Donnees de ", nom_obs)) + 
    theme_bw() +
    geom_line(stat = "density") +
    scale_x_date(
      breaks = seq(from = min(Total$date), 
                   to = max(Total$date), 
                   by = "2 years"),               
      labels = scales::label_date("%Y")
    )
  
  # Ordres
  p2 <- 
    Total %>%
    dplyr::filter(date > params$date_min,
           observateurs == toupper(nom_obs)) %>%
    ggplot(aes(ordre,fill=ordre, color = ordre)) +
    geom_bar() +
    labs(title = " Repartition des differents ordres",
         subtitle = paste("Donnees de ", nom_obs)) +
    theme_bw() +
    scale_fill_manual(values = couleur) +
    scale_color_manual(values = couleur) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
    coord_flip()
  
  # Carte
  geo_obsteur <- Total %>% 
    dplyr::filter(date > params$date_min) %>%
    dplyr::filter(observateurs == toupper(nom_obs)) %>%
    dplyr::select(ordre,
                  date)
  p3 <- ggplot() +
    geom_sf(data = carte) + 
    labs(title = "Carte des observations",
         subtitle = paste("Donnees de ", nom_obs)) +
    geom_sf(data = geo_obsteur, size=0.01) +
    theme_bw()
  
  
  # Affichage des plots
  p1 + p2 + p3
}

