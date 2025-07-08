histogramme_maillage_pos <- function(colonne, espece, taille){
  nbr_maille_par_date <- Total%>%
    filter(cd_nom == espece)%>%
    group_by(year(date), {{colonne}})%>%
    summarise(n())
  nb_mailles_par_date <- st_drop_geometry(nbr_maille_par_date)
  names(nb_mailles_par_date)[1] <- "Annee"
   p <- nb_mailles_par_date %>%
    ggplot()+
    geom_bar(aes(x= Annee))+
    theme_bw()+
    labs(title = paste0("Maillage de ", taille , "km"),
         y="Nombre de mailles")
   return(p)
}

pour_chaque_maillage <- function(espece){
  p1 <- histogramme_maillage_pos(Grid2km, taille = 2, espece)
  p2 <- histogramme_maillage_pos(Grid5km, taille = 5, espece)
  p3 <- histogramme_maillage_pos(Grid10km, taille = 10, espece)
  p4 <- histogramme_maillage_pos(Grid20km, taille = 20, espece)
  
  p <- ggarrange(p1, p2, p3, p4) 
  
  annotate_figure(p, top = text_grob( as.character(espece), 
                                        color = "red", face = "bold", size = 14))
  
}

# Test
pour_chaque_maillage(60674)