graph_ordre_tranche_annee <- function(ordre_voulu){
  # Creation de la BDD
  ordre_data <- Total_sf %>% filter(ordre == ordre_voulu)
  bdd_props <- prop.table(table(ordre_data$bdd_originale))
  n_bdd1 <- round(5000 * bdd_props[1])
  n_bdd2 <- 5000 - n_bdd1
  bdd1 <- ordre_data %>% 
    filter(bdd_originale == "GeoNature") %>% 
    sample_n(min(n_bdd1, n()))
  bdd2 <- ordre_data %>% 
    filter(bdd_originale == "VisioNature") %>% 
    sample_n(min(n_bdd2, n()))
  
  ordre_graph <- bind_rows(bdd1, bdd2) %>%
    mutate(
      grp_date = case_when(
        date >= as.Date("2010-01-01") & date <= as.Date("2014-12-31") ~ "1. Entre 2010 et 2015",
        date >= as.Date("2015-01-01") & date <= as.Date("2019-12-31") ~ "2. Entre 2015 et 2020",
        date > as.Date("2019-12-31") ~ "3. Apres 2020",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(date > as.Date("2010-01-01")) %>%
    sample_frac(1)
  
  # Transformation pour des donnees de la carte
  carte_ordre <- ordre_graph %>%
    select(grp_date, ordre, date) %>%
    filter(ordre == ordre_voulu)
  
  # Plot
  ggplot() +
    geom_sf(data = carte_bretagne) + 
    labs(
      title = paste("Carte des observations en Bretagne des", ordre_voulu),
      subtitle = "Toutes donnees"
    ) +
    geom_sf(data = carte_ordre, size = 0.01) +
    theme_bw() + 
    facet_grid(. ~ grp_date)
}


## Grid


#' Applique une fonction qui plot qqe chose a tous les ordres
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
  bdd <- st_transform(bdd, 2154)
  
  grid_spacing <- 1000 * taille_en_km
  
  Grid <- st_make_grid(bdd, 
                       cellsize = c(grid_spacing, grid_spacing), 
                       square = TRUE)
  Grid <- st_sf(ID = seq_along(Grid), geometry = Grid)
  Grid <- st_transform(Grid, 2154)
  
  Grid <- st_make_valid(Grid)
  bdd <- st_make_valid(bdd)
  
  nom_colonne <- paste0("Grid", taille_en_km,"km")
  names(Grid)[1] <- nom_colonne
  bdd <- st_join(bdd, Grid, left=TRUE)

  return(bdd)
}

#' ajoute et nettoie les familles de paysages
#'
#' @param 
#'
#' @return La base de donnee, avec les colonne en +
#' @export
#'
#' @examples
#' 
ajout_famille_paysage <- function(){
  Total <- transforme_carte(Total)
  
  famille_paysage <- st_read(
    paste0(wd$data, "masques/famille_paysage/famille_paysages.shp")
  )
  
  famille_paysage$Nom <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Nom)
  famille_paysage$Nom <- gsub("\uFFFD", "e", famille_paysage$Nom)
  
  famille_paysage$Famille <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Famille)
  famille_paysage$Famille <- gsub("\uFFFD", "e", famille_paysage$Famille)
  
  
  famille_paysage <- famille_paysage%>%
    transforme_carte()%>%
    filter(CODE_REG != 39)%>%
    st_buffer(famille_paysage, dist = 200)
  
  Total <- st_join(Total, famille_paysage, left = TRUE)
  
  Total_sans_na <- Total%>%
    filter(!is.na(Nom))
  
  Total <- Total%>%
    mutate(Nom = ifelse(is.na(Nom), 
                        Total$Nom[st_nearest_feature(Total, Total_sans_na)], 
                        Nom),
           Famille = ifelse(is.na(Famille), 
                            Total$Famille[st_nearest_feature(Total, Total_sans_na)], 
                            Famille))
  
  Total <- Total %>%
    rename("paysage_ID" = "CODE_REG",
           "paysage_nom" = "Nom",
           "famille_paysage" = "Famille")
  
  return(Total)
}

grid_list <- map(ordres, function(o) {
  vis_ord <- VN_sf %>% filter(ordre == o)
  grid_tmp <- grid
  grid_tmp$density <- lengths(st_intersects(grid_tmp, vis_ord))
  grid_tmp$ordre <- o
  grid_tmp
})
