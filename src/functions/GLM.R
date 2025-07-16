
#' Donne la valeur dominante d'une colonne d'une bdd
#'
#' @param x la colonne 
#'
#' @return 
#' @export
#'
#' @examples
#' 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Donne le tableau sur lequel on fera les modifs
#'
#' @param espece_interet L'espece qui nous interesse 
#' @param espece_benchmark L'espece avec laquel on compare l'espece d'interet
#' @param taillegrid La colonne de maille qu'on utilise 
#'
#' @return 
#' @export
#'
#' @examples
#' 
tab_glm <- function(espece_interet, espece_benchmark, taillegrid = "Grid10km"){
  tab <- Total %>%
    st_drop_geometry() %>%
    filter(cd_nom %in% c(espece_interet, espece_benchmark)) %>%
    select(date, nom_vernaculaire, cd_nom, famille_paysage, !!sym(taillegrid)) %>%
    filter(date > as.Date("2010-01-01")) %>%
    group_by(year = year(date), !!sym(taillegrid)) %>%
    summarise(
      famille_paysage_max = Mode(famille_paysage), .groups = "drop",
      proportion_interet = sum(cd_nom == espece_interet)/n()
    )
  return(tab)  
}
