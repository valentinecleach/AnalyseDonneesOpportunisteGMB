

maille


# La fouine


test_nb_maille <- Total %>%
  filter(cd_nom == 60674)


nombre_km <- function(colonne){
  colonnedekm <- test_nb_maille%>%
    group_by({{colonne}})%>%
    summarise(n())
  colonnedekm <- st_drop_geometry(colonnedekm)
  return(colonnedekm)
}


pour_chaque_maillage <- function(){
  
  
  ggarrange(p1, p2, p3, p4)
}

View(tab)

nombre_10km <- test_nb_maille%>%
  group_by(Grid10km)%>%
  summarise(n())

