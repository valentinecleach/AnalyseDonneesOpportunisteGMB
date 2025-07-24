#' Crée une maille d'une taille donnée
#'
#' @param bdd La base de donnee sur laquelle faire la 
#' @param taille_en_km La longueur des cotes des mailles
#'
#' @return La base de donnee, avec la colonne en +
#' @export
#'
#' @examples
#' 
creer_maille <- function(bdd, taille_en_km){
  bdd <- transforme_carte(bdd)
  bdd <- sf::st_transform(bdd, 2154)
  
  grid_spacing <- 1000 * taille_en_km
  
  Grid <- sf::st_make_grid(bdd, 
                           cellsize = c(grid_spacing, grid_spacing), 
                           square = TRUE)
  Grid <- sf::st_sf(ID = seq_along(Grid), geometry = Grid)
  Grid <- sf::st_transform(Grid, 2154)
  
  Grid <- sf::st_make_valid(Grid)
  bdd <- sf::st_make_valid(bdd)
  
  nom_colonne <- paste0("Grid", taille_en_km,"km")
  names(Grid)[1] <- nom_colonne
  bdd <- sf::st_join(bdd, Grid, left=TRUE)
  
  return(bdd)
}

ajout_10x10_predetermine <- function(bdd = Total){
  grille_10x10 <- sf::st_read(
    paste0(wd$data, "masques/Grille_10x10/Grille_10X10.shp")
  )
  RegionBretagneConti <- sf::st_read(
    paste0(wd$data, "masques/RegionBretagneConti/RegionBretagneConti.shp")
  )
  
  bdd <- bdd %>%
    transforme_carte()
  grille_10x10 <- grille_10x10%>%
    transforme_carte()
  RegionBretagneConti <- RegionBretagneConti%>%
    transforme_carte()
  
  centroids <- sf::st_centroid(grille_10x10)
  centroid_coords <- sf::st_coordinates(centroids)
  
  grille_10x10_centroids <- grille_10x10 %>%
    dplyr::mutate(
      X_10km = centroid_coords[, "X"],
      Y_10km = centroid_coords[, "Y"]
    ) %>%
    dplyr::select(CODE_10KM, X_10km, Y_10km)
  
  grille_10x10 <- sf::st_intersection(grille_10x10, RegionBretagneConti)
  
  grille_10x10 <- grille_10x10 %>%
    dplyr::left_join(
      as.data.frame(grille_10x10_centroids),
      by = "CODE_10KM"
    )
  
  bdd <- grille_10x10 %>%
    dplyr::select(CODE_10KM, X_10km, Y_10km)%>%
    sf::st_join(bdd, left = TRUE)
  
  if("cd_nom" %in% colnames(bdd))
  {
    bdd <- bdd %>%
      dplyr::filter(!is.na(cd_nom))
  }else{
    if("cd_nom_t" %in% colnames(bdd))
    {
      bdd <- bdd %>%
        dplyr::filter(!is.na(cd_nom_t))
    } 
  }
  
  return(bdd)
}


#' Ajoute la moyenne de l'une bdd .tif a l'interieur de chaque carré d'une maille
#'
#' @param bdd_grille La base de donnee avec les mailles 
#' @param bdd_tif Une base de donnee .tif avec la variable qui nous interesse
#'
#' @return La base de donnee, avec la colonne en +
#' @export
#'
#' @examples
#' 
ajout_variable_struct <- function(bdd_grille = grille_10x10, bdd_tif){
  # Import BDD
  bdd <- terra::rast(paste0(wd$data,
                            "masques/VariablesStructurates/", 
                            bdd_tif, 
                            ".tif"))
  terra::crs(bdd) <- "EPSG:2154"
  
  # Transformation
  grille_10x10 <- sf::st_transform(grille_10x10, crs = "EPSG:2154")
  grille_vect <- terra::vect(grille_10x10)
  grille_10x10[bdd_tif] <- exactextractr::exact_extract(bdd,
                                                        grille_10x10, 
                                                        'mean')
  return(grille_10x10)
}

