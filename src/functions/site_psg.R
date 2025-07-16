#' Ajoute les famille de paysage
#'
#' @param 
#'
#' @return La base de donnee, avec les colonne en +
#' @export
#'
#' @examples
#' 
ajout_famille_paysage <- function(bdd = Total){
  # GIVEN
  famille_paysage <- sf::st_read(
    paste0(wd$data, "masques/famille_paysage/famille_paysages.shp")
  )
  
  # WHEN/THEN
  
  # Wrong encoding for the database. We simply replace letters.
  famille_paysage$Nom <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Nom)
  famille_paysage$Nom <- gsub("\uFFFD", "e", famille_paysage$Nom)
  
  famille_paysage$Famille <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Famille)
  famille_paysage$Famille <- gsub("\uFFFD", "e", famille_paysage$Famille)
  
  
  famille_paysage <- famille_paysage%>%
    transforme_carte()%>%
    dplyr::filter(CODE_REG != 39)%>%
    sf::st_buffer(famille_paysage, dist = 200)
  
  bdd <- bdd %>%
    transforme_carte()%>%
    sf::st_join(famille_paysage, left = TRUE)
  
  # Attribution d'une famille aux points en littoral, sans familles
  bdd_sans_na <- bdd %>%
    dplyr::filter(!is.na(Nom))
  
  bdd <- bdd%>%
    dplyr::mutate(Nom = ifelse(is.na(Nom), 
                               bdd$Nom[st_nearest_feature(bdd, bdd_sans_na)], 
                               Nom),
                  Famille = ifelse(is.na(Famille), 
                                   bdd$Famille[st_nearest_feature(bdd, bdd_sans_na)], 
                                   Famille))
  
  bdd <- bdd %>%
    dplyr::rename("paysage_ID" = "CODE_REG",
                  "paysage_nom" = "Nom",
                  "famille_paysage" = "Famille")
  
  return(bdd)
}
