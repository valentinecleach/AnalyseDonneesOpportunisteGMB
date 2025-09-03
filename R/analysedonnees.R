#' Donne le nombre d'observation de chaque ordre pour chaque individu
#'
#' @param nom_ordre Le nom de l'ordre qu'on veut compter
#'
#' @return Un tibble observateurs et nombre
#' @export
#'
#' @examples
#' 
nb_ordre <- function(bdd = Total, nom_ordre){
  nb_o <- bdd %>%
    dplyr::filter(ordre == nom_ordre) %>%
    dplyr::group_by(observateurs) %>%
    dplyr::summarise(nbr = n(), 
                     .groups = "drop")
  
  nom = paste("nb_", 
              tolower(nom_ordre), 
              sep = "")
  nb_o <- nb_o %>% 
    dplyr::rename_at("nbr", ~nom)
  
  return(nb_o)
}

repartition_espece_cluster <- function(bdd = Total, cluster){
  bdd %>%
    dplyr::filter(clust == cluster) %>%
    dplyr::count(nom_vernaculaire, sort = TRUE) %>%
    ggplot2::ggplot(aes(x = reorder(nom_vernaculaire, n), 
               y = n)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste0("Repartition des espaces du cluster ", cluster),
      x = "Nom vernaculaire"
    ) +
    scale_x_discrete(labels = label_wrap(40)) +
    theme_bw() +
    theme(axis.text = element_text(size = 8))
}

