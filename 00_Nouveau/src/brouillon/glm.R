#########
#### Proportion
#########

# -> préparer sur papier les différentes options, savoir bien ce qu'on veut faire

summary(as.factor(Total$cd_nom))
Total%>%
  filter(cd_nom %in% c(61587, 60674))%>%
  select(distinct(nom_vernaculaire))
Total$nom_vernaculaire

prop_espece <- function(espece, bdd){
  
  
  return()
}