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
