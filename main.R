library(renv)
renv::repair()

loadedNamespaces()

source("init.R")

library(dplyr)
library(sf)
library(ggplot2)

wd <- set_wd()
set.seed(12345)

summary(Total)

Total <- sf::st_read(paste0(wd$data, "derived/TotalComplet.shp"), 
                     options = "ENCODING=UTF8")
Total <- transform_Total()


Total %>%
  dplyr::distinct(cd_nom)%>%
  dplyr::select(nom_vernaculaire, cd_nom)


diro <- st_read(paste0(wd$data,"derived/diro.shp"))

# Total : 160991
# DIR Ouest : 26129

26129/160991

rm(list=setdiff(ls(), "Total"))

#### Nettoyage de donnees ####
# Total:
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Total_sites.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total_sites.html"))

#### Stats Desc ####
rmarkdown::render(paste0(wd$src, "finished/stats_desc/Repartitions.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Repartitions.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Geographie.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Geographie.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Observateurs.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Observateurs.html"))

#### Classification ####

rmarkdown::render(paste0(wd$src, 
                         "finished/acp/Ordres.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "acp/Ordres.html")
)

#### Regression ####

rmarkdown::render(paste0(wd$src, 
                         "finished/models/glm.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "models/glm/debuts.html"))

rmarkdown::render(paste0(wd$src, "brouillon/glm.Rmd"))
rmarkdown::render(paste0(wd$src, "brouillon/glm.Rmd"))


rmarkdown::render(paste0(wd$src, "finished/models/Regressions_Lineaires.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Regressions_Lineaire.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "brouillon/Diro_desc.Rmd"),
                  output_file = paste0(wd$output, 
                                       "stats_desc/Diro.html"),
                  encoding="UTF-8")

ggplot(grille_10x10) +
  geom_sf(aes(fill = Densite_Cultures_500m)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

ggplot(grille_10x10) +
  geom_sf(aes(fill = Distance_EcotoneArbore)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

ggplot(Total) +
  geom_sf(aes(fill = Indice_Diversite)) +
  theme_minimal()+
  scale_fill_gradientn(colors = topo.colors(6))

###############

Total <- transform_Total(bdd = Total)
summary(Total)

Donnes_Nat <- Total%>%
  filter(technique_observation == "Vu")

Donnes_Nat <- Donnes_Nat %>%
  sf::st_drop_geometry()

bdd_reg <- tab_glm(Donnes_Nat, 
                   espece_interet = 61714,
                   espece_benchmark = c(61667, 61057))

summary(bdd_reg)
cor(bdd_reg[-c("clust_max", "famille_paysage_max", "Code_10km")])
corrplot::corrplot(cor(bdd_reg[c(1:3,5:7,9:12)]))

col <- colorRampPalette(c("#990000","#990000", 
                          "#eeeeee",
                          "#05600b","#05600b"))

corrplot::corrplot(cor(subset(bdd_reg, select=-c(famille_paysage_max, Code_10km))),
                   method="color", col=col(200),  
                   order="hclust", 
                   addCoef.col = "black", # Ajout du coefficient de correlation
                   tl.col="black", tl.srt=45 # Rotation des etiquettes de textes
)

t <- bdd_reg %>% dplyr::select(-clust_max, -famille_paysage_max, )
str((bdd_reg$Code_10km))

car::vif(lm(data = bdd_reg, proportion_interet ~ year + Distance_EcotoneArbore_m + Indice_Diversite_m + X_10km + Y_10km+Densite_Cultures_m))
# Des VIF correctes pour tous. On accepte toutes les propositions.

bdd_reg[4]

t <- bdd_reg %>% 
  dplyr::select(-clust_max, -famille_paysage_max, -Code_10km)%>%
  sf::st_drop_geometry()
cor(t)
str(t)


###########

bdd_reg <- tab_glm(Donnes_Nat, 
                   espece_interet = 61714,
                   espece_benchmark = 61667)

bdd_reg <- bdd_reg %>%
  dplyr::group_by(Code_10km) %>%
  dplyr::arrange(year, Code_10km) %>%
  dplyr::mutate(prev_y = dplyr::lag(proportion_interet)) %>%
  dplyr::mutate(prev_prev_y = dplyr::lag(prev_y)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prev_y = dplyr::if_else(is.na(prev_y),
                                        0,
                                        prev_y),
                prev_prev_y = dplyr::if_else(is.na(prev_prev_y),
                                             0,
                                             prev_prev_y),
                year2 = year**2
                )

reg <- glm(data = bdd_reg, 
           proportion_interet ~ year+X_10km+Dist_Ecotone+Dnst_Cultures)
summary(reg)
autoplot(reg) # 239 c'est une proportion de 1 en 2010, talus, 11 ans du site...

bptest(reg, studentize = FALSE) # Homosedasticit? okay a 90%
bgtest(reg, type = "F") # On ne peut pas dire qu'il n'y a pas d'autocorr?lation des erreurs
shapiro.test(reg$residuals)

summary(bdd_reg)
View(bdd_reg)

acf(reg$residuals)

bdd_reg2 <- bdd_reg%>%
  dplyr::select(-c(Code_10km, famille_paysage_max))

bdd_reg3 <- bdd_reg%>%
  dplyr::mutate(year = as.factor(year))%>%
  dplyr::select(-c(Code_10km, famille_paysage_max))


car::vif(glm(data=bdd_reg2, proportion_interet~.))

regfit.full <- leaps::regsubsets(proportion_interet~., data=bdd_reg2)

reg.summary = summary(regfit.full)

par(mfrow=c(2 ,2))
plot(reg.summary$rss , xlab =" Number of Variables " , ylab =" RSS " ,type ="l")
plot(reg.summary$adjr2 , xlab =" Number of Variables " , ylab =" Adjusted RSq " , type ="l")
which.max(reg.summary$adjr2)

points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col =" red " , cex =2 , pch =20)

plot(reg.summary$cp , xlab =" Number of Variables " , ylab =" Cp " ,type = "l")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col =" red " , cex =2 , pch =20)

plot(reg.summary$bic , xlab =" Number of Variables " , ylab =" BIC " , type ="l")
which.min(reg.summary$bic)
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col =" red " , cex =2 , pch =20)

par(mfrow=c(1 ,1))
plot(regfit.full , scale ="bic") # adjr2 R^2_a, Cp, bic

#### en Facteur.

bdd_reg3<- bdd_reg3%>%
  dplyr::select(-year2)
car::vif(glm(data=bdd_reg3, proportion_interet~.))

regfit.full <- leaps::regsubsets(proportion_interet~., data=bdd_reg3)

reg.summary = summary(regfit.full)

par(mfrow=c(2 ,2))
plot(reg.summary$rss , xlab =" Number of Variables " , ylab =" RSS " ,type ="l")
plot(reg.summary$adjr2 , xlab =" Number of Variables " , ylab =" Adjusted RSq " , type ="l")
which.max(reg.summary$adjr2)

points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col =" red " , cex =2 , pch =20)

plot(reg.summary$cp , xlab =" Number of Variables " , ylab =" Cp " ,type = "l")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col =" red " , cex =2 , pch =20)

plot(reg.summary$bic , xlab =" Number of Variables " , ylab =" BIC " , type ="l")
which.min(reg.summary$bic)
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col =" red " , cex =2 , pch =20)

par(mfrow=c(1 ,1))
plot(regfit.full , scale ="adjr2") # adjr2 R^2_a, Cp, bic


bdd_reg <- tab_glm(Donnes_Nat, 
                   espece_interet = 61714,
                   espece_benchmark = 60636)

bdd_reg <- bdd_reg %>%
  dplyr::group_by(Code_10km) %>%
  dplyr::arrange(year, Code_10km) %>%
  dplyr::mutate(prev_y = dplyr::lag(proportion_interet)) %>%
  dplyr::mutate(prev_prev_y = dplyr::lag(prev_y)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prev_y = dplyr::if_else(is.na(prev_y),
                                        0,
                                        prev_y),
                prev_prev_y = dplyr::if_else(is.na(prev_prev_y),
                                             0,
                                             prev_prev_y),
                year2 = year**2
  )
reg <- glm(data = bdd_reg, 
           proportion_interet ~ year+X_10km+Dist_Ecotone+Dnst_Cultures+prev_y)
par(mfrow=c(1,2))
acf(reg$residuals)
pacf(reg$residuals)

spec.ar(reg$residuals)
spec.pgram(reg$residuals, 100)

install.packages("zoib")
library(zoib)

reg_01 <- zoib(proportion_interet ~ year,
               data = bdd_reg,
               zero.inflation = TRUE,
               one.inflation = TRUE
               )

reg_01 <- zoib(
  proportion_interet ~ year | 1 | 1 | 1,    # mean depends on year, others only intercept
  data = bdd_reg,
  zero.inflation = TRUE,
  one.inflation = TRUE
)
sample1 <- reg_01$coeff
summary(sample1)
# check convergence on the regression coefficients
traceplot(sample1);
autocorr.plot(sample1);
check.psrf(sample1)

paraplot(reg_01)
str(bdd_reg$proportion_interet)
str(bdd_reg$year)

?zoib
### Non parametrique.

Kendall::MannKendall(bdd_reg2$proportion_interet)
?MannKendall

bdd_reg2$proportion_interet

bdd_tif = "Distance_Littoral"
# Import BDD
bdd <- terra::rast(paste0(wd$data,
                          "masques/VariablesStructurates/", 
                          bdd_tif, 
                          ".tif"))
terra::crs(bdd) <- sf::st_transform(bdd, "EPSG:2154")

t <- sf::st_transform(bdd, "EPSG:2154")

b
# Transformation
grille_10x10 <- sf::st_transform(grille_10x10, crs = "EPSG:2154")
grille_vect <- terra::vect(grille_10x10)
grille_10x10[bdd_tif] <- exactextractr::exact_extract(bdd,
                                                      grille_10x10, 
                                                      'mean')
