matrice_occu_detections <- function(num_cd_nom, bdd = Total, var_site = VariablesSite){
  BDD <- bdd %>%
    sf::st_join(var_site) %>%
    dplyr::filter(cd_nom == num_cd_nom,
                  technique_observation == "Vu",
                  date >= as.Date("2010-01-01"),
                  date < as.Date("2025-01-01")) %>%
    # select(-c(insee_dept, lib_dept, lib_dept, FID, surf, CD_SIG)) %>%
    dplyr::select(-nom_valide, -nom_vernaculaire, -ordre, -technique_observation) %>%
    dplyr::mutate(year = lubridate::year(date))
  
  detection_df <- BDD %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(annee = lubridate::year(date)) %>%
    dplyr::select(Code_10km, annee) %>%
    dplyr::count(Code_10km, annee) %>%
    tidyr::complete(Code_10km, 
                    annee = tidyr::full_seq(annee, 1), 
                    fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = annee, 
                       values_from = n) %>%
    dplyr::arrange(Code_10km) %>%
    dplyr::filter(!is.na(Code_10km))
  
  detection_df[,-1] <- ifelse(detection_df[,-1]>0, 1, 0)
  
  noms <- detection_df$Code_10km
  detection_matrice <- as.matrix(detection_df[, -1])
  rownames(detection_matrice) <- noms
  
  return(detection_matrice)
}

pression_obs_bdd <- function(num_cd_nom, bdd = Total, var_site = VariablesSite){
  Autres <- bdd %>%
    sf::st_join(var_site) %>%
    dplyr::filter(cd_nom != num_cd_nom,
                  technique_observation == "Vu",
                  date >= as.Date("2010-01-01"),
                  date < as.Date("2025-01-01"))%>%
    dplyr::select(-nom_valide, -nom_vernaculaire, -ordre, -technique_observation) %>%
    dplyr::mutate(year = lubridate::year(date))
  
  detection_autres <- Autres %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(annee = lubridate::year(date)) %>%
    dplyr::select(Code_10km, annee) %>%
    dplyr::count(Code_10km, annee) %>%
    tidyr::complete(Code_10km, 
                    annee = tidyr::full_seq(annee, 1), 
                    fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = annee, 
                       values_from = n) %>%
    dplyr::arrange(Code_10km) %>%
    dplyr::filter(!is.na(Code_10km))
  
  detection_autres <- detection_autres[-1]
  detection_autres <- scale(detection_autres)
  
  return(detection_autres)
}


chisq <- function(fm) {
  umf <- fm@data
  y <- umf@y
  y[y>1] <- 1
  fv <- fitted(fm)
  sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
}


