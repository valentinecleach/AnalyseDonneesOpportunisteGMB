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
tab_glm <- function(espece_interet, espece_benchmark, bdd = Total){
  
  tab <- bdd %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(cd_nom %in% c(espece_interet, espece_benchmark)) %>%
    dplyr::filter(date > as.Date("2010-01-01")) %>%
    dplyr::mutate(year = lubridate::year(date))
    
  colonnes <- c("famille_paysage", "clust", 
                "Indice_Diversite", "Densite_Cultures", 
                "Distance_EcotoneArbore", "Distance_Littoral",
                "Distance_Eau")
  colonnes_presentes <- intersect(colonnes, names(tab))
  
  tab <- tab %>%
    dplyr::select(any_of(c("date", "nom_vernaculaire", "cd_nom", 
                           "Code_10km", "X_10km", "Y_10km", "year",
                           colonnes_presentes)))
  
  tab <- tab %>%
    dplyr::group_by(year, Code_10km) %>%
    dplyr::mutate(proportion_interet = sum(cd_nom == espece_interet) / dplyr::n())
  
  tab <- tab %>%
    ajoute_si_present(variable = "famille_paysage", 
                      technique = Mode) %>%
    ajoute_si_present(variable = "clust", 
                      technique = Mode) %>%
    ajoute_si_present(variable = "Indice_Diversite",
                      technique = mean) %>%
    ajoute_si_present(variable = "Densite_Cultures",
                      technique = mean) %>%
    ajoute_si_present(variable = "Distance_EcotoneArbore",
                      technique = mean) %>%
    ajoute_si_present(variable = "Distance_Littoral",
                      technique = mean) %>%
    ajoute_si_present(variable = "Distance_Eau",
                      technique = mean)
    
  tab <- tab %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Code_10km) %>%
    dplyr::mutate(nb_annee_site = dplyr::n_distinct(year)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Code_10km, year) %>%
    dplyr::mutate(proportion_total = sum(proportion_interet) / nb_annee_site)
  
    ttes_collones_sortantes <- c("year", "proportion_interet", "proportion_total",
                   "Code_10km", "nb_annee_site", "X_10km", "Y_10km")
  
    collones_sortantes_opt <- c("famille_paysage_max", "clust_max", 
                              "Indice_Diversite_m", "Densite_Cultures_m", 
                              "Distance_EcotoneArbore_m", "Distance_Littoral_m",
                              "Distance_Eau_m")
    collones_sortantes_presentes <- intersect(collones_sortantes_opt, names(tab))
    
    tab <- tab %>%
      dplyr::select(any_of(c(ttes_collones_sortantes, collones_sortantes_presentes))) %>%
      dplyr::distinct(year, Code_10km, .keep_all = TRUE)
    
    tab <- tab%>%
      dplyr::rename_with(~ case_when(
        . == "Indice_Diversite_m" ~ "Ind_Diversite",
        . == "Densite_Cultures_m" ~ "Dnst_Cultures",
        . == "Distance_EcotoneArbore_m" ~ "Dist_Ecotone",
        . == "Distance_Littoral_m" ~ "Dist_Littoral",
        . == "Distance_Eau_m" ~ "Dist_Eau",
        TRUE ~ .))
    
    tab <- tab %>%
      dplyr::group_by(Code_10km) %>%
      dplyr::arrange(year, Code_10km) %>%
      dplyr::mutate(prop_tmoins1 = dplyr::lag(proportion_interet)) %>%
      dplyr::mutate(prop_tmoins2 = dplyr::lag(prop_tmoins1)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prop_tmoins1 = dplyr::if_else(is.na(prop_tmoins1),
                                                  0, prop_tmoins1),
                    prop_tmoins2 = dplyr::if_else(is.na(prop_tmoins2),
                                                  0, prop_tmoins2),
                    year2 = year**2) %>%
      dplyr::select(-proportion_total, -nb_annee_site)
  return(tab)
}

#' Ajoute la moyenne/mean
#'
#' @param bdd La base de donnée.
#' @param variable La variable a ajouter
#' @param technique Mean ou Mode, selon les cas.
#'
#' @return La bdd modifié
#'
#' @examples
#' 
ajoute_si_present <- function(bdd, variable, technique = mean) {
  
  if (variable %in% names(bdd)) {
    new_var_name <- paste0(variable, "_m")
    
    bdd <- bdd %>%
      dplyr::group_by(Code_10km) %>%
      dplyr::mutate(
        !!new_var_name := technique(.data[[variable]])
      ) %>%
      dplyr::ungroup()
  }
  return(bdd)
}  


#' Supprime les sites qui sont toujours 0 ou 1
#'
#' @param bdd La base de donnée.
#'
#' @return La bdd modifié
#'
#' @examples
#' 
suppression_prop01 <- function(bdd = bdd_reg){
  return(dplyr::filter(bdd_reg, proportion_total %in% c(0,1)))
}

#' Fait automatiquement la glm (hyp et tout)
#'
#' @param cd_nom_interet L'espèce qu'on souhaite étudier
#' @param cd_nom_benchmark L'espèce témoin.
#' @param supprimer01 TRUE si on souhaite supprimer les sites avec 0 ou 1
#' @param bdd La base de donnée qu'on utilise
#' #'
#' @return 
#' @export
#'
#' @examples
#' 
glm_automatique <- function(cd_nom_interet, cd_nom_benchmark, 
                            supprimer01 = FALSE, bdd = Total){
  
  bdd_reg <- tab_glm(bdd, 
                     espece_interet = cd_nom_interet,
                     espece_benchmark = cd_nom_benchmark)
  
  if(supprimer01){bdd_reg = suppression_prop01(bdd = bdd_reg)}
   
   bdd_reg %>%
     ggplot2::ggplot() +
     ggplot2::geom_histogram(aes(proportion_interet)) +
     labs(title = "Repartition des proportions de l'interet par rapport au benchmark",
          y = "Proportion") + 
     theme_bw()
   
   # Interactions entre Variables quantitatives
   col <- colorRampPalette(c("#990000","#990000", 
                             "#eeeeee",
                             "#05600b","#05600b"))
   corrplot::corrplot(cor(subset(bdd_reg, select=-c(famille_paysage_max, Code_10km))),
             method="color", col=col(200),  
             order="hclust", 
             addCoef.col = "black", # Ajout du coefficient de correlation
             tl.col="black", tl.srt=90 # Rotation des etiquettes de textes
    )
  # alias(lm(data = bdd_reg, 
  #           proportion_interet ~ year+Y_10km+X_10km+famille_paysage_max+Code_10km))
  car::vif(lm(data = bdd_reg, 
          proportion_interet ~ year+famille_paysage_max+Code_10km))
  car::vif(lm(data = bdd_reg, 
              proportion_interet ~ year+Y_10km + X_10km + famille_paysage_max))

   # Interactions entre Variables qualitatives
   interaction.plot(bdd_reg$famille_paysage_max,
                    as.factor(bdd_reg$Code_10km),
                    bdd_reg$proportion_interet,
                    main = "Interaction entre la grille et la famille de paysage")
   interaction.plot(bdd_reg$famille_paysage_max,
                    as.factor(bdd_reg$clust_max),
                    bdd_reg$proportion_interet,
                    main = "Interaction entre les clusters et la famille de paysage")
   reg <- glm(data = bdd_reg, 
              proportion_interet ~ year+X_10km+Y_10km+clust_max)
   
   return(reg)
}


#' Affiche les 4 plots généréres par leaps::plot.regsubsets
#'
#' @param reg.summary Un object summary(regsubsets(...))
#'
#' @examples
#' 
plot_regsubsets <- function(reg.summary){
  #  2x2 grid 
  par(mfrow = c(2,2))
  
  # 
  plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
  
  # The red dot to indicates the model with the largest adjusted R^2 statistic.
  # Ie model to take with this statistic
  adj_r2_max = which.max(reg.summary$adjr2) 
  points(adj_r2_max, reg.summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
  
  # Same for C_p and BIC
  plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
  cp_min = which.min(reg.summary$cp) # 10
  points(cp_min, reg.summary$cp[cp_min], col = "red", cex = 2, pch = 20)
  
  plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  bic_min = which.min(reg.summary$bic) # 6
  points(bic_min, reg.summary$bic[bic_min], col = "red", cex = 2, pch = 20)
  
  par(mfrow=c(1 ,1))
}

#' Compare quali vs quanti
#' @param data
#' @return chaine de charactaire quanti ou quali
choisi_forme_year <- function(bdd) {
  # Modif des bdd
  data_quanti <- bdd %>%
    dplyr::select(-c(Code_10km))
  data_quali  <- bdd %>%
    dplyr::mutate(year=as.factor(year)) %>%
    dplyr::select(-c(year2, Code_10km))
  
  # Modelisation
  regfit_quanti <- leaps::regsubsets(
    formula = proportion_interet~. ,
    data = data_quanti,
    method = "seqrep"
  )
  regfit_quali <- leaps::regsubsets(
    formula = proportion_interet~. ,
    data = data_quali,
    method = "seqrep"
  )

  # BIC_minimum
  min_bic_quanti <- min(summary(regfit_quanti)$bic, na.rm = TRUE)
  min_bic_quali  <- min(summary(regfit_quali)$bic, na.rm = TRUE)
  
  # Choix
  if (min_bic_quanti < min_bic_quali) {
    return(list(choice = "quantitative", min_bic = min_bic_quanti))
  } else {
    return(list(choice = "qualitative", min_bic = min_bic_quali))
  }
}
