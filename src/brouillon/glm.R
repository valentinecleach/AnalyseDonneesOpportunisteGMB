

bdd_reg <- tab_glm(bdd = Total, 
                   espece_interet = 61714,
                   espece_benchmark = 61057)

alias(lm(data = bdd_reg, 
         proportion_interet ~ year+Y_10km+X_10km+famille_paysage_max))
alias(lm(data = bdd_reg, 
         proportion_interet ~ 1+year+famille_paysage_max+Code_10km))

car::vif(lm(data = bdd_reg, 
            proportion_interet ~ 1+year+famille_paysage_max+Code_10km))

car::vif(lm(data = bdd_reg, 
            proportion_interet ~ 1+Y_10km+X_10km+year+famille_paysage_max))

## On devra enlever Y_10km et X_10km ou Code_10km.
# Perso, je trouves + intéressant de garder les coordonnées.

library(ggplot2)
# LAPIN RAGONDIN
bdd_reg <- tab_glm(Total, 
                   espece_interet = 61714,
                   espece_benchmark = 61667)
reg <- glm(data = bdd_reg, 
           proportion_interet ~ year+X_10km)
autoplot(reg)
summary(reg)

reg <- glm_automatique(cd_nom_interet = 61714, 
                       cd_nom_benchmark = 61667)
autoplot(reg)
summary(reg)
# LAPIN CHEVREUIL
reg <- glm_automatique(cd_nom_interet = 61714, cd_nom_benchmark = 61057)
autoplot(reg)
summary(reg)

bdd_reg <- tab_glm(Total, 
                   espece_interet = 61714,
                   espece_benchmark = 61057)
reg <- glm(data = bdd_reg, 
           proportion_interet ~ year+I(year**2)+X_10km)
autoplot(reg)
summary(reg)

# LAPIN BLAIREAU
reg <- glm_automatique(cd_nom_interet = 61714, 
                       cd_nom_benchmark = 60636)
autoplot(reg)
summary(reg)

bdd_reg <- tab_glm(Total, 
                   espece_interet = 61714,
                   espece_benchmark = 60636)
reg <- glm(data = bdd_reg, 
           proportion_interet ~ year+I(year**2)+X_10km)
autoplot(reg)
summary(reg)

# LAPIN HERISSON
reg <- glm_automatique(cd_nom_interet = 61714, 
                       cd_nom_benchmark = )
autoplot(reg)
summary(reg)

bdd_reg <- tab_glm(Total, 
                   espece_interet = 61714,
                   espece_benchmark = 60015)
reg <- glm(data = bdd_reg, 
           proportion_interet ~ year+I(year**2)+X_10km)

#######

# SANGLIER RAGONDIN
reg <- glm_automatique(cd_nom_interet = 60981, 
                       cd_nom_benchmark = 61667)
autoplot(reg)
summary(reg)

bdd_reg <- tab_glm(Total, 
                   espece_interet = 61714,
                   espece_benchmark = 60015)
reg <- glm(data = bdd_reg, 
           proportion_interet ~ year+I(year**2)+X_10km)

# SANGLIER CHEVREUIL
reg <- glm_automatique(cd_nom_interet = 60981, 
                       cd_nom_benchmark = 61057)
autoplot(reg)
summary(reg)
# SANGLIER BLAIREAU
reg <- glm_automatique(cd_nom_interet = 60981, 
                       cd_nom_benchmark = 60636)
autoplot(reg)
summary(reg)
# SANGLIER HERISSON
reg <- glm_automatique(cd_nom_interet = 60981, 
                       cd_nom_benchmark = 60015)
autoplot(reg)
summary(reg)

######

# RENARD RAGONDIN
reg1 <- glm_automatique(cd_nom_interet = 60585, 
                       cd_nom_benchmark = 61667)
autoplot(reg1)
summary(reg1)
# RENARD CHEVREUIL
reg <- glm_automatique(cd_nom_interet = 60585, 
                       cd_nom_benchmark = 61057)
autoplot(reg)
summary(reg)
# RENARD BLAIREAU
reg <- glm_automatique(cd_nom_interet = 60585, 
                       cd_nom_benchmark = 60636)
autoplot(reg)
summary(reg)
# RENARD HERISSON
reg <- glm_automatique(cd_nom_interet = 60585, 
                       cd_nom_benchmark = 60015)
autoplot(reg)
summary(reg)


######

# MARTRES RAGONDIN
reg <- glm_automatique(cd_nom_interet = 60658, 
                       cd_nom_benchmark = 61667)
autoplot(reg)
summary(reg)

# MARTRES CHEVREUIL
reg1 <- glm_automatique(cd_nom_interet = 60658, 
                       cd_nom_benchmark = 61057)
autoplot(reg1)
summary(reg1)
# MARTRES BLAIREAU
reg <- glm_automatique(cd_nom_interet = 60658, 
                       cd_nom_benchmark = 60636)
autoplot(reg)+theme_bw()
summary(reg)

# MARTRES HERISSON
reg <- glm_automatique(cd_nom_interet = 60658, 
                       cd_nom_benchmark = 60015)
autoplot(reg)
summary(reg)

######### AJOUT COLLISIONS ROUTIERES

Diro <- sf::st_read(paste0(wd$data, "derived/diro.shp"))

Morts_Naturalistes <- Total%>%
  dplyr::filter(etat_biologique %in% c("Trouvé mort : impact routier", 
                                       "TrouvÃ© mort : impact routier"))

Diro%>%
  filter(deppr == 44)

View(Morts_Naturalistes)
summary(as.factor(Diro$deppr))

Morts <- rbind(Morts_Naturalistes, Diro)

summary(as.factor(Total$etat_biologique))

Total %>%
  distinct(etat_biologique)
