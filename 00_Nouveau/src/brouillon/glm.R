#########
#### Proportion
#########


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


tab_glm <- function(espece_interet, espece_benchmark, taillegrid = "Grid10km"){
  
  tab <- Total%>%
    st_drop_geometry() %>%
    filter(cd_nom %in% c(espece_interet, espece_benchmark))%>%
    select(date, nom_vernaculaire, cd_nom, famille_paysage, !!sym(taillegrid))%>%
    filter(date > as.Date("2010-01-01")) %>%
    group_by(year = year(date), !!sym(taillegrid)) %>%
    summarise(
      famille_paysage_max = Mode(famille_paysage), .groups = "drop",
      proportion_lapin = sum(cd_nom == espece_interet)/n()
    )
  
  return(tab)  
}

t2 <- tab_glm(61714, 60585)
reg <- glm(data = t2, 
           proportion_lapin ~ year)
summary(reg)

t3 <- t2 %>%
  filter(!(proportion_lapin %in% c(0, 1)))
reg <- glm(data = t3, 
           proportion_lapin ~ year)
summary(reg)
plot(reg)

t3 <- t2 %>%
  filter(!(proportion_lapin %in% c(0, 1)))
reg <- glm(data = t3, 
           proportion_lapin ~ year+famille_paysage_max)
summary(reg)
plot(reg)


t4 <- t3%>%
  mutate(year = as.factor(year))
reg <- glm(data = t4, 
           proportion_lapin ~ year)
summary(reg)



reg <- glm(data = t2, 
           proportion_lapin ~ year, 
           family = binomial)
summary(reg)

reg <- glm(data = t3, 
           proportion_lapin ~ year, 
           family = binomial)
summary(reg)

reg <- glm(data = t4, 
           proportion_lapin ~ year, 
           family = binomial)
summary(reg)
plot(reg)




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
