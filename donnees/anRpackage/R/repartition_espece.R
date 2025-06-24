repartition_espece <-
function(bdd = Total, nom_ordre, repartition, date_min = params$date_min) {
  
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
  
  bdd_filtre %>%
    mutate(nom_vernaculaire_grp = fct_infreq(nom_vernaculaire_grp)) %>% 
    ggplot(aes(x = {{ repartition }}, fill = nom_vernaculaire_grp)) +
    geom_bar(position = "dodge") +
    coord_flip()+
    scale_fill_discrete(labels = label_wrap(40)) +
    theme_bw()
}
