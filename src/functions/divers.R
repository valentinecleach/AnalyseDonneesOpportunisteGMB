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
transform_Total <- function(){
  Total <- Total %>%
    mutate_at(c("bdd_rgn", 
                "ett_blg", "tchnq_b", 
                "communs", "obsrvtr", 
                "famille", "ordre", 
                "nm_vrnc", "nom_vld"), 
              .funs = as.factor) %>%
    rename(bdd_originale = bdd_rgn,
           etat_biologique = ett_blg,
           technique_observation = tchnq_b,
           nom_vernaculaire = nm_vrnc,
           nom_valide = nom_vld,
           observateurs = obsrvtr)
  return(Total)
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
  if (is.na(st_crs(carte)$epsg)) {
    carte <- st_set_crs(carte, 2154)
  }
  carte <- st_transform(carte, 4326)
  return(carte)
}

ajoute_clust_a_Total <- function(){
  wd <- set_wd()
  Observateurs <-  read.csv(paste0(wd$data, "Observateurs.csv"))
  
  Total <- Total %>%
    left_join(Observateurs %>% select(observateurs, clust), by = "observateurs")
  
  return(Total)
} 
