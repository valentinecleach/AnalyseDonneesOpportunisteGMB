repartition_espece <- function(bdd = Total, nom_ordre, repartition, date_min = params$date_min) {
  
  pour_titre1 = deparse(substitute(repartition))
  pour_titre2 = deparse(substitute(nom_ordre))
  
  bdd_filtre <- bdd %>%
    filter(ordre == nom_ordre,
           date > params$date_min)
  
  grps <- fct_lump(bdd_filtre$nom_vernaculaire, prop = 0.03)
  
  if (length(unique(grps)) > 5) {
    bdd_filtre <- bdd_filtre %>%
      mutate(nom_vernaculaire_grp = grps)
  } else {
    bdd_filtre <- bdd_filtre %>%
      mutate(nom_vernaculaire_grp = nom_vernaculaire)
  }
  
  p <- bdd_filtre %>%
    mutate(nom_vernaculaire_grp = fct_infreq(nom_vernaculaire_grp)) %>% 
    ggplot(aes(x = {{ repartition }}, fill = nom_vernaculaire_grp)) +
    geom_bar(position = "dodge") +
    coord_flip()+
    scale_fill_discrete(labels = label_wrap(40)) +
    theme_bw()+
    labs(title = paste("Barplot de la ", 
                       pour_titre1, 
                       " des ", 
                       pour_titre2))
}


application_tous_ordres <- function(fonction, repartition_cherche=NA){
  p1 <- fonction(nom_ordre = "Carnivora", 
                 repartition = {{repartition_cherche}})
  p2 <- fonction(nom_ordre = "Cetartiodactyla", 
                 repartition = {{repartition_cherche}})
  p3 <- fonction(nom_ordre = "Eulipotyphla", 
                 repartition = {{repartition_cherche}})
  p4 <- fonction(nom_ordre = "Lagomorpha", 
                 repartition = {{repartition_cherche}})
  p5 <- fonction(nom_ordre = "Rodentia", 
                 repartition = {{repartition_cherche}})
  return(list(p1,p2,p3,p4,p5))
}
