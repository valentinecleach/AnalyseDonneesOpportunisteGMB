
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
        date > as.Date("2019-12-31") ~ "3. AprÃ¨s 2020",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(date > as.Date("2010-01-01")) %>%
    sample_frac(1)
  
  # Transformation pour des donnÃ©es de la carte
  carte_ordre <- ordre_graph %>%
    select(grp_date, ordre, date) %>%
    filter(ordre == ordre_voulu)
  
  # Plot
  ggplot() +
    geom_sf(data = carte_bretagne) + 
    labs(
      title = paste("Carte des observations en Bretagne des", ordre_voulu),
      subtitle = "Toutes donnÃ©es"
    ) +
    geom_sf(data = carte_ordre, size = 0.01) +
    theme_bw() + 
    facet_grid(. ~ grp_date)
}


## Grid


#' Applique une fonction qui plot qqe chose a tous les ordres
#'
#' @param bdd La base de donnée sur laquelle faire la 
#' @param taille_en_km La longueur des cotés des mailles
#'
#' @return La base de donnée, avec la colonne en +
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
