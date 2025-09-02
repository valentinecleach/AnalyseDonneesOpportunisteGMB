library(dplyr);library(ggplot2);library(sf);library(unmarked)
set.seed(12345)

couleur_renard = "#CC5500"

Total <- sf::st_read(paste0(wd$data, "derived/Total.shp"),
                     options = "ENCODING=UTF8")
Total <- Total%>%
  transform_Total()

VariablesSite <- sf::st_read(paste0(wd$data, "derived/VariablesSite.shp"),
                             options = "ENCODING=UTF8")
VariablesSite <- VariablesSite%>%
  transform_VarSites()

site_info <- VariablesSite %>%
  sf::st_drop_geometry() %>%
  dplyr::arrange(Code_10km)

noms <- site_info$Code_10km
rownames(site_info) <- noms

site_info <- site_info %>%
  dplyr::select(-Code_10km) %>%
  dplyr::rename(Dnst_Cultures = `Dnst_Cultures `)%>%
  dplyr::mutate(across(where(is.numeric), 
                       scale))


quali_periodes_matrice_5 <- 
  matrix(c('2010 - 2012', '2013 - 2015', 
           '2016 - 2018', '2019-2021', '2022 - 2024'),
         nrow = 317, 
         ncol = 5, 
         byrow = TRUE)

quanti_periodes_matrice_5 <- matrix(
  rep(1:5, each=317),
  nrow=317, 
  ncol=5, 
  byrow=FALSE
)

site_covs_periodes_5 <- list(
  quanti_periodes = quanti_periodes_matrice_5,
  quali_periodes = quali_periodes_matrice_5
)



detection_matrice <- matrice_occu_detections(num_cd_nom = 60585)

pression_obs <- pression_obs_bdd(num_cd_nom = 60585)

umf <- unmarked::unmarkedMultFrame(
  y = detection_matrice,
  siteCovs = site_info,
  obsCovs = list(pression_obs = pression_obs ),
  yearlySiteCovs = site_covs_periodes_5,
  numPrimary = 5
)

fm <- unmarked::colext(
  psiformula = ~ 1,     # initial occupancy
  gammaformula =  ~ 1,  # colonization
  epsilonformula = ~ 1, # extinction
  pformula = ~ pression_obs,  # detection
  data = umf, # data
  se = TRUE)

fm_quanti <- unmarked::colext(
  psiformula = ~ 1,     # initial occupancy
  gammaformula =  ~ quanti_periodes,  # colonization
  epsilonformula = ~ quanti_periodes, # extinction
  pformula = ~ pression_obs,  # detection
  data = umf, # data
  se = TRUE)

fm_quali <- unmarked::colext(
  psiformula = ~ 1,     # initial occupancy
  gammaformula =  ~ quali_periodes,  # colonization
  epsilonformula = ~ quali_periodes, # extinction
  pformula = ~ pression_obs,  # detection
  data = umf, # data
  se = TRUE)

fm; fm_quanti; fm_quali

confint((fm_quali)[2])
confint((fm_quali)[3])


plot <- proba_graphique_5(fm = fm_quali, couleur=couleur_renard)

plot +
  labs(title = "Probabilite d'occupation du renard a travers les saisons",
       x = "Saisons", y ="Probabilite d'occupation lisee")


plot <- carte_graphique_5(fm=fm, couleur=couleur_renard)
ggpubr::annotate_figure(plot, 
                        top = ggpubr::text_grob("Presence du renard"))
