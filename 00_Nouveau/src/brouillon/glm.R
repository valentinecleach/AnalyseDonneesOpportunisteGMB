#########
#### Proportion
#########

# -> préparer sur papier les différentes options, savoir bien ce qu'on veut faire

summary(as.factor(Total$cd_nom))


Total %>%
  filter(cd_nom == 61714)

Total%>%
  distinct(nom_vernaculaire, cd_nom)

t <- Total%>%
  st_drop_geometry() %>%
  filter(cd_nom %in% c(61714, 61028))%>%
  select(date, nom_vernaculaire, cd_nom, famille_paysage, Grid5km)%>%
  filter(date > as.Date("2010-01-01")) %>%
  group_by(year = year(date), Grid5km) %>%
  summarise(famille_paysage_max = case_when(
    mean(famille_paysage == "Paysage cultive avec talus")> 0.5 ~  "Paysage cultive avec talus",
    mean(famille_paysage == "Paysage de bocage e maille elargie")> 0.5 ~  "Paysage de bocage e maille elargie",
    mean(famille_paysage == "Paysage boise et de bosquets")> 0.5 ~  "Paysage boise et de bosquets",
    mean(famille_paysage == "Paysage cultive e ragosses")> 0.5 ~  "Paysage cultive e ragosses",
    mean(famille_paysage == "Paysage de littoral urbanise")> 0.5 ~  "Paysage de littoral urbanise",
    mean(famille_paysage == "Paysage de bocage dense sur collines")> 0.5 ~  "Paysage de bocage dense sur collines",
    mean(famille_paysage == "Paysage de cultures legumieres")> 0.5 ~  "Paysage de cultures legumieres",
    mean(famille_paysage == "Paysage associe e la presence d'eau")> 0.5 ~  "Paysage associe e la presence d'eau"
    ),
    proportion_lapin = mean(cd_nom == 61714), famille_paysage)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


t <- Total%>%
  st_drop_geometry() %>%
  filter(cd_nom %in% c(61714, 60585))%>%
  select(date, nom_vernaculaire, cd_nom, famille_paysage, Grid5km)%>%
  filter(date > as.Date("2010-01-01")) %>%
  group_by(year = year(date), Grid5km) %>%
  summarise(
    famille_paysage_max = Mode(famille_paysage), .groups = "drop",
    proportion_lapin = sum(cd_nom == 61714)/n()
    )

reg <- lm(data = t, proportion_lapin ~ year)
plot(reg)


