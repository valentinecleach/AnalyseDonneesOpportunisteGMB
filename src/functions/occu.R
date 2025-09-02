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


carte_graphique_5 <- function(var_site = VariablesSite, fm, couleur = "black"){
  grid_10x10 <- VariablesSite %>%
    transforme_carte()
  
  rf <- unmarked::ranef(fm)
  occ_prob <- unmarked::bup(rf, stat="mean")
  
  grid_10x10 <- cbind(grid_10x10, occ_prob)
  
  
  LtoM <-colorRampPalette(c('white',couleur))
  
  
  g1 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X1))+
    labs(title="De 2010 à 2012")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  
  g2 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X2))+
    labs(title="De 2013 à 2015")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  g3 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X3))+
    labs(title="De 2016 à 2018")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  g4 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X4))+
    labs(title="De 2019 à 2021")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  
  g5 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X5))+
    labs(title="De 2022 à 2024")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilite de presence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  
  plot <- ggpubr::ggarrange(g1 + theme(legend.position = "none"),
                            g2 + theme(legend.position = "none"),
                            g3 + theme(legend.position = "none"),
                            g4 + theme(legend.position = "none"),
                            g5 + theme(legend.position = "none"),
                            common.legend = TRUE,
                            legend = "right")
  
  return(plot)
}


carte_graphique_3 <- function(var_site = VariablesSite, 
                              fm, couleur = "black"){
  grid_10x10 <- VariablesSite %>%
    transforme_carte()
  
  rf <- unmarked::ranef(fm)
  occ_prob <- unmarked::bup(rf, stat="mean")
  
  grid_10x10 <- cbind(grid_10x10, occ_prob)
  
  
  LtoM <-colorRampPalette(c('white',couleur))
  
  
  g1 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X1))+
    labs(title="De 2010 à 2014")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  
  g2 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X2))+
    labs(title="De 2015 à 2019")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  g3 <- ggplot() +
    geom_sf(data = grid_10x10, aes(fill = X3))+
    labs(title="De 2020 à 2024")+
    scale_fill_gradient2(low=LtoM(0),
                         high=LtoM(100),
                         limits = c(0, 1),
                         name = "Probabilité de présence")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  plot <- ggpubr::ggarrange(g1 + theme(legend.position = "none"),
                            g2 + theme(legend.position = "none"),
                            g3 + theme(legend.position = "none"),
                            common.legend = TRUE,
                            legend = "right")
  
  return(plot)
}


proba_graphique_5 <- function(fm, couleur = "black"){
  m1 <- nonparboot(fm, 
                   B = 100)
  
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
    ggplot(aes(x = saison, y = smoothed_occ, group = 1, col = couleur)) +
    geom_line() +
    geom_point() +
    theme_bw()+
    theme(legend.position = "none")
  
  return(plot)
}

proba_graphique_3 <- function(fm, couleur="black" ){
  m1 <- nonparboot(fm, 
                   B = 100)
  
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
    ggplot(aes(x = saison, y = smoothed_occ, group = 1, col = couleur)) +
    geom_line() +
    geom_point() +
    theme_bw()+
    theme(legend.position = "none")
  
  return(plot)
}