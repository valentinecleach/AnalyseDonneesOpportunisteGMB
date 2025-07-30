set_wd <- function(){
  # working directory
  wd <- list()
  # commonly used paths in my working directory
  
  wd$data  <- "~/work/AnalyseDonneesOpportunisteGMB/data/"
  wd$output <- "~/work/AnalyseDonneesOpportunisteGMB/output/"
  wd$src <- "~/work/AnalyseDonneesOpportunisteGMB/src/"
  
  return(wd)
}

#' Corrige les noms et les mets en factor
#'
#' @param 
#' @param 
#'
#' @return 
#' @export
#'
#' @examples
#' 
transform_Total <- function(bdd = Total){
  bdd <- bdd %>%
    dplyr::mutate_at(c("bdd_rgn", 
                "ett_blg", "tchnq_b", "obsrvtr", 
                "ordre", 
                "nm_vrnc", "nom_vld"), 
              .funs = as.factor) %>%
    dplyr::rename(bdd_originale = bdd_rgn,
           etat_biologique = ett_blg,
           technique_observation = tchnq_b,
           nom_vernaculaire = nm_vrnc,
           nom_valide = nom_vld,
           observateurs = obsrvtr)
  bdd <- bdd %>%
    dplyr::mutate_at(c(if('Grid2km' %in% names(.)) 'Grid2km',
                if('Grid5km' %in% names(.)) 'Grid5km',
                if('Grd10km' %in% names(.)) 'Grd10km',
                if('Grd20km' %in% names(.)) 'Grd20km',
                if('pysg_nm' %in% names(.)) 'pysg_nm',
                if('pysg_ID' %in% names(.)) 'pysg_ID',
                if('fmll_py' %in% names(.)) 'fmll_py',
                if('CODE_10' %in% names(.)) 'CODE_10'),
              as.factor) %>%
    dplyr::rename_with(
      ~ case_when(
        . == "Grd10km" ~ "Grid10km",
        . == "Grd20km" ~ "Grid20km",
        . == "pysg_ID" ~ "paysage_ID",
        . == "pysg_nm" ~ "paysage_nom",
        . == "fmll_py" ~ "famille_paysage",
        . == "Indc_Dv" ~ "Indice_Diversite",
        . == "Dnst_Cl" ~ "Densite_Cultures",
        . == "Dstn_EA" ~ "Distance_EcotoneArbore",
        . == "Dstnc_L" ~ "Distance_Littoral",
        . == "Dstnc_E" ~ "Distance_Eau",
        . == "CODE_10" ~ "Code_10km",
        TRUE ~ .))
  
  return(bdd)
}

#' Change les CRS de la carte pour ce qu'on veuilles
#'
#' @param carte La carte a modifier
#'
#' @return carte
#' @export
#'
#' @examples
#' 
transforme_carte <- function(carte){
  if (is.na(sf::st_crs(carte)$epsg)) {
    carte <- sf::st_set_crs(carte, 2154)
  }
  carte <- sf::st_transform(carte, 4326)
  return(carte)
}

ajoute_clust_a_Total <- function(bdd = Total){
  wd <- set_wd()
  Observateurs <-  read.csv(paste0(wd$data, "derived/Observateurs.csv"), 
                            sep = ";")
  
  bdd <- bdd %>%
    dplyr::left_join(Observateurs %>% dplyr::select(observateurs, clust), 
                     by = "observateurs")
  
  return(bdd)
} 
