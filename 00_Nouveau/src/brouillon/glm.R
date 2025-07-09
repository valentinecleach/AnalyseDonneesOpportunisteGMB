#########
#### Proportion
#########


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tab_glm <- function(espece_interet, benchmark, taillegrid = "Grid20km"){
  
  taillegrid <- st_drop_geometry(Total)[taillegrid]

  tab <- Total%>%
    st_drop_geometry() %>%
    filter(cd_nom %in% c(espece_interet, benchmark))%>%
    select(date, nom_vernaculaire, cd_nom, famille_paysage, {{taillegrid}})%>%
    filter(date > as.Date("2010-01-01")) %>%
    group_by(year = year(date), {{taillegrid}}) %>%
    summarise(
      famille_paysage_max = Mode(famille_paysage), .groups = "drop",
      proportion_lapin = sum(cd_nom == espece_interet)/n()
    )
  
  return(tab)  
}

t2 <- tab_glm(61714, 60585)

str(t2)


t <- Total%>%
  st_drop_geometry() %>%
  filter(cd_nom %in% c(61714, 60585))%>%
  select(date, nom_vernaculaire, cd_nom, famille_paysage, Grid20km)%>%
  filter(date > as.Date("2010-01-01")) %>%
  group_by(year = year(date), Grid20km) %>%
  summarise(
    famille_paysage_max = Mode(famille_paysage), .groups = "drop",
    proportion_lapin = sum(cd_nom == 61714)/n()
    )

gridxkm <- paste0("Grid", 20, "km")
{{gridxkm}}
reg <- glm(data = t, proportion_lapin ~ year, family = binomial)
?glm
summary(reg)
plot(reg)

plot(x=t$year, y=t$proportion_lapin)

t2 <- t %>%
  mutate(grp_year = case_when(
    (year < as.Date("2018-01-01")) ~ "Avant2018",
    (year >= as.Date("2018-01-01")) ~ "Apres2018" ))

Total%>%
  st_drop_geometry()%>%
  filter(year(date)>as.Date("2020-01-01"))

class(Total$year)

summary(t2)
t2$grp_year <- as.factor(t2$grp_year)
t2$famille_paysage_max <- as.factor(t2$famille_paysage_max)
reg <- lm(data = t2, proportion_lapin ~ grp_year)


t$year <- as.factor(t$year)
t$famille_paysage_max <- as.factor(t$famille_paysage_max)


class(t$year)
class(t$famille_paysage_max)
