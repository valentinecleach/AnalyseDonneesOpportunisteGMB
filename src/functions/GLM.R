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
tab_glm <- function(espece_interet, espece_benchmark, bdd=Total){
  tab <- bdd %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(cd_nom %in% c(espece_interet, espece_benchmark)) %>%
      dplyr::select(date, 
                    nom_vernaculaire, cd_nom, 
                    Code_10km, X_10km, Y_10km,
                    clust, famille_paysage) %>%
      dplyr::filter(date > as.Date("2010-01-01")) %>%
      dplyr::mutate(year = lubridate::year(date)) %>%
      dplyr::group_by(year, Code_10km) %>%
      dplyr::mutate(proportion_interet = sum(cd_nom == espece_interet)/n(),
                    famille_paysage_max = Mode(famille_paysage),
                    clust_max = Mode(clust)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Code_10km) %>%
      dplyr::mutate(nb_annee_site = dplyr::n_distinct(year)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Code_10km, year) %>%
      dplyr::mutate(proportion_total = sum(proportion_interet)/nb_annee_site) %>%
      dplyr::select(year, proportion_interet, proportion_total,
                    Code_10km, nb_annee_site, X_10km, Y_10km,
                    famille_paysage_max, clust_max)%>%
      dplyr::distinct(year, Code_10km, .keep_all = TRUE)
    
  
  return(tab)  
}

tab <- tab_glm(bdd = Total, 
                   espece_interet = 61714,
                   espece_benchmark = 61057)
head(tab%>%
  filter(Code_10km == "E028N679"))


#' Supprime les sites qui sont toujours 0 ou 1
#'
#' @param bdd La base de donnée.
#'
#' @return 
#' @export
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
