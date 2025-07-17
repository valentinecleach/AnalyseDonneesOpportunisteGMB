
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
tab_glm <- function(espece_interet, espece_benchmark, taillegrid = "Grid10km"){
  tab <- Total %>%
    st_drop_geometry() %>%
    filter(cd_nom %in% c(espece_interet, espece_benchmark)) %>%
    select(date, nom_vernaculaire, cd_nom, famille_paysage, !!sym(taillegrid)) %>%
    filter(date > as.Date("2010-01-01")) %>%
    group_by(year = year(date), !!sym(taillegrid)) %>%
    summarise(
      famille_paysage_max = Mode(famille_paysage), .groups = "drop",
      proportion_interet = sum(cd_nom == espece_interet)/n()
    )
  
  return(tab)  
}

glm_automatique <- function(cd_nom_interet, cd_nom_benchmark, supprimer01 = FALSE, bdd = Total){
  
  bdd_reg <- tab_glm(bdd, 
                     espece_interet = cd_nom_interet,
                     espece_benchmark = cd_nom_benchmark,
                     taillegrid = "Grid10km")
  
  if(supprimer01){bdd_reg = suppression_prop01(bdd_reg)}
  
  # plot
  plot(data = bdd_reg,
       x = year, 
       y = proportion_interet)
  
  bdd_reg %>%
    ggplot() +
    geom_histogram(aes(proportion_interet)) +
    labs(title = "Repartition des proportions de l'interet par rapport au benchmark",
         y = "Proportion") + theme_bw()
  
  # Interactions entre Variables quantitatives
  col <- colorRampPalette(c("#990000","#990000", 
                            "#eeeeee",
                            "#05600b","#05600b"))
  
  corrplot(cor(bdd_reg[,-c(3:4)], bdd_reg[,-c(3:4)]), 
           method="color", col=col(200),  
           order="hclust", 
           addCoef.col = "black", # Ajout du coefficient de correlation
           tl.col="black", tl.srt=45, # Rotation des etiquettes de textes
           # Combiner avec le niveau de significativite
           sig.level = 0.01, insig = "blank", 
           # Cacher les coefficients de correlation sur la diagonale
           diag=FALSE 
  )
  alias(lm(data = bdd_reg, 
           proportion_interet ~ year+Y_10km+X_10km+famille_paysage_max+Grid10km))
  vif(lm(data = bdd_reg, 
         proportion_interet ~ year+X_10km+famille_paysage_max+Grid10km))
  # Interactions entre Variables qualitatives
  interaction.plot(bdd_reg$famille_paysage_max,
                   as.factor(bdd_reg$Grid10km),
                   bdd_reg$proportion_interet,
                   main = "Interaction entre la grille et la famille de paysage")
  interaction.plot(bdd_reg$famille_paysage_max,
                   as.factor(bdd_reg$clust_max),
                   bdd_reg$proportion_interet,
                   main = "Interaction entre les clusters et la famille de paysage")
  reg <- glm(data = bdd_reg, 
             proportion_interet ~ year+X_10km+famille_paysage_max+Grid10km)
  summary(reg)
}
