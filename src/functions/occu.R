#' Donne une matrice de detections Oui/Non pour une espèce par site et année
#'
#' @param num_cd_nom
#' @param bdd
#' @param var_site
#'
#' @return Une matrice detection / non detection
#' 
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

#' Donne la pression d'observation sans l'espèce qu'on étudie
#'
#' @param num_cd_nom espèce a ne pas garder
#' @param Total
#' @param var_site
#'
#' @return Une base de donnée de pression d'obs année par année, site par site
#'
#' @examples
#' 
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


#' Donne le chi carree (pour les tests)
#'
#' @param 
#' @param 
#'
#' @return 
#' @export
#'
#' @examples
#' 
chisq <- function(fm) {
  umf <- fm@data
  y <- umf@y
  y[y>1] <- 1
  fv <- fitted(fm)
  sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
}



#' Donne un graph de proba d'occupancy sur les sites pour 5 saisons
#'
#' @param var_site La bdd de variables
#' @param fm
#' @param couleur
#'
#' @return plot
#' 
carte_graphique_5 <- function(var_site = VariablesSite, 
                              fm, 
                              couleur = "black"){
  
  grid_10x10 <- VariablesSite %>%
    transforme_carte()
  
  rf <- unmarked::ranef(fm)
  occ_prob <- unmarked::bup(rf, stat="mean")
  
  grid_10x10 <- cbind(grid_10x10, occ_prob)
  
  LtoM <- colorRampPalette(c('white', couleur))
  
  taille_petit_titre <- 12
  
  g1 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X1)) +
    labs(title = "De 2010 à 2012") +
    scale_fill_gradient2(low = LtoM(0),
                         high = LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  g2 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X2)) +
    labs(title = "De 2013 à 2015") +
    scale_fill_gradient2(low = LtoM(0),
                         high = LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  g3 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X3)) +
    labs(title = "De 2016 à 2018") +
    scale_fill_gradient2(low = LtoM(0),
                         high = LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  g4 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X4)) +
    labs(title = "De 2019 à 2021") +
    scale_fill_gradient2(low = LtoM(0),
                         high = LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  g5 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X5)) +
    labs(title = "De 2022 à 2024") +
    scale_fill_gradient2(low = LtoM(0),
                         high = LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilite de presence") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  plot <- ggpubr::ggarrange(
    g1 + theme(legend.position = "none"),
    g2 + theme(legend.position = "none"),
    g3 + theme(legend.position = "none"),
    g4 + theme(legend.position = "none"),
    g5, 
    nrow = 1, ncol = 5,
    common.legend = TRUE,
    legend = "bottom"
  )
  
  return(plot)
}



#' Donne un graph de proba d'occupancy sur les sites pour 3 saisons
#'
#' @param var_site La bdd de variables
#' @param fm
#' @param couleur
#'
#' @return plot
#' 
carte_graphique_3 <- function(var_site = VariablesSite, 
                              fm, 
                              couleur = "black"){
  
  grid_10x10 <- VariablesSite %>%
    transforme_carte()
  
  rf <- unmarked::ranef(fm)
  occ_prob <- unmarked::bup(rf, stat="mean")
  
  grid_10x10 <- cbind(grid_10x10, occ_prob)
  
  
  LtoM <-colorRampPalette(c('white',couleur))
  
  taille_petit_titre <- 12
  
  g1 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X1))+
    labs(title="De 2010 à 2014")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  
  g2 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X2))+
    labs(title="De 2015 à 2019")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  g3 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X3))+
    labs(title="De 2020 à 2024")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = taille_petit_titre))
  
  plot <- ggpubr::ggarrange(
    g1 + theme(legend.position = "none"),
    g2 + theme(legend.position = "none"),
    g3,
    nrow = 1, ncol = 3,
    common.legend = TRUE,
    legend = "bottom")
  
  return(plot)
}



#' Donne un graph de proba d'occupancy lissé pour 5 saisons
#'
#' @param fm
#' @param couleur
#'
#' @return plot
#' 
proba_graphique_5 <- function(fm, couleur = "black"){
  m1 <- nonparboot(fm, 
                   B = 10)
  
  predicted_occupancy <- data.frame(saison = c(1:5),
                                    smoothed_occ = smoothed(fm)[2,],
                                    SE = m1@smoothed.mean.bsse[2,])
  
  plot <- predicted_occupancy %>%
    mutate(saison = factor(
      saison,
      levels = 1:5,
      labels = c("[2010 ; 2012]", 
                 "[2013 ; 2015]", 
                 "[2016 ; 2018]", 
                 "[2019 ; 2021]", 
                 "[2022 ; 2024]")
    )) %>%
    ggplot(aes(x = saison, y = smoothed_occ, group = 1)) +
    geom_line(color = couleur) +
    geom_point(color = couleur) +
    theme_bw()+
    theme(legend.position = "none")
  
  return(plot)
}


#' Donne un graph de proba d'occupancy lissé pour 3 saisons
#'
#' @param fm
#' @param couleur
#'
#' @return plot
#' 
proba_graphique_3 <- function(fm, couleur="black" ){
  
  m1 <- nonparboot(fm, 
                   B = 10)
  
  predicted_occupancy <- data.frame(saison = c(1:3),
                                    smoothed_occ = smoothed(fm)[2,],
                                    SE = m1@smoothed.mean.bsse[2,])
  
  plot <- predicted_occupancy %>%
    mutate(saison = factor(
      saison,
      levels = 1:3,
      labels = c("[2010 ; 2014]", 
                 "[2015 ; 2019]", 
                 "[2020 ; 2024]")
    )) %>%
    ggplot(aes(x = saison, y = smoothed_occ, group = 1)) +
    geom_line(color = couleur) +
    geom_point(color = couleur) +
    theme_bw()+
    theme(legend.position = "none")
  
  return(plot)
}

#' Donne des tables de detections selon l'umf, et le type de saisons etc.
#'
#' @param 
#' @param 
#'
#' @return 
#' @export
#'
#' @examples
#' 
tables_detections <- function(umf, 
                              n_visits_par_periode, 
                              n_periodes){
  
  #Given
  y <- getY(umf)
  n_sites <- nrow(y)
  
  #When
  stopifnot(ncol(y) == n_periodes * n_visits_par_periode)
  
  #Then
  y_array <- array(y, dim = c(n_sites, n_periodes, n_visits_par_periode))
  
  # retourne le nombre de periodes par site
  detection_periodes <- apply(y_array, c(1, 2), function(x) any(x > 0))
  n_periodes_par_site <- apply(detection_periodes, 1, sum)
  n_periodes_par_site <- table(n_periodes_par_site)
  
  # retourne le nombre de sites détectés pour chaque periode
  sites_detecte_par_periode <- sapply(1:n_periodes, function(t) {
    sum(rowSums(y_array[, t, , drop = FALSE]) > 0)
  })
  
  return(list(n_periodes_par_site, sites_detecte_par_periode))
}
