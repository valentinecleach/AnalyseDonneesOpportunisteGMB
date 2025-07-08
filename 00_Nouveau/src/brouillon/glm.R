#########
#### Proportion
#########

# -> préparer sur papier les différentes options, savoir bien ce qu'on veut faire

summary(as.factor(Total$cd_nom))

test <- Total%>%
  filter(cd_nom %in% c(61587, 61678))%>%
  select(date, nom_vernaculaire, cd_nom)


test <- test %>%
  filter(date>as.Date("2010-01-01"))%>%
  group_by(year = year(date),cd_nom)%>%
  summarise(nombre = n())


test <- test %>%
  group_by(year)%>%
  mutate(nbr_total_annee = sum(nombre))%>%
  ungroup()%>%
  mutate(proportion = nombre/nbr_total_annee)


t <- test %>%
  filter(cd_nom != 61587)

t %>%
  ggplot()+
  geom_point(aes(year, proportion))

glm(data = t, proportion~year)
?ts

t <- ts(t$proportion, start = 2007)
plot(t)

acf(t)
pacf(t)

prop_espece <- function(espece, bdd){
  
  
  return()
}