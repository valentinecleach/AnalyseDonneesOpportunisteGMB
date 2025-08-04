library(renv)
renv::repair()

loadedNamespaces()

options(encoding = 'UTF-8')
source("init.R")

library(dplyr)
library(sf)
library(ggplot2)

wd <- set_wd()
set.seed(12345)

Total <- sf::st_read(paste0(wd$data, "derived/TotalComplet.shp"), 
                     options = "ENCODING=UTF8")
Total <- transform_Total()



rm(list=setdiff(ls(), "Total"))

#### Nettoyage de donnees ####
# Total:
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/cleaning/Total_sites.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total_sites.html"),
                  encoding="UTF-8")

#### Stats Desc ####
rmarkdown::render(paste0(wd$src, "finished/stats_desc/Repartitions.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Repartitions.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Geographie.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Geographie.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Observateurs.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Observateurs.html"),
                  encoding="UTF-8")

#### Classification ####

rmarkdown::render(paste0(wd$src, 
                         "finished/acp/Ordres.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "acp/Ordres.html",
                  encoding="UTF-8")
                  )

#### Regression ####

rmarkdown::render(paste0(wd$src, 
                         "finished/models/glm.Rmd"), 
                  output_file = paste0(wd$output, 
                                       "models/glm/debuts.html"),
                  encoding="UTF-8")


rmarkdown::render(paste0(wd$src, "brouillon/Diro_desc.Rmd"),
                  output_file = paste0(wd$output, 
                                       "stats_desc/Diro.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lin_CollisionsRoutieres.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_CollisionsRoutieres.html"),
                  encoding="UTF-8")

knitr::knit(paste0(wd$src, "finished/models/Reg_Lin_CollisionsRoutieres.Rmd"))

rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lin_ToutesDonnes.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_ToutesDonnes.html"),
                  encoding="UTF-8")
knitr::knit(paste0(wd$src, "finished/models/Reg_Lin_ToutesDonnes.Rmd"))

###############

lines <- readLines(paste0(wd$src, "finished/models/Reg_Lin_CollisionsRoutieres.Rmd", 
                          encoding = "UTF-8"))
cat(lines[1:10], sep = "\n")
tools::file_test("-f", paste0(wd$src, "finished/models/Reg_Lin_CollisionsRoutieres.Rmd"))
file.info(paste0(wd$src, "finished/models/Reg_Lin_CollisionsRoutieres.Rmd"))$Encoding

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

bdd_reg <- tab_glm(Total_et_Diro, 
                   espece_interet = 61714,
                   espece_benchmark = 61667)


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
  dplyr::select(-c(proportion_total, nb_annee_site, 
                   Code_10km, famille_paysage_max))

bdd_reg3 <- bdd_reg%>%
  dplyr::mutate(year = as.factor(year))%>%
  dplyr::select(-c(Code_10km, famille_paysage_max,
                   proportion_total, nb_annee_site))
car::vif(glm(data=bdd_reg2, proportion_interet~.))

regfit.full <- leaps::regsubsets(proportion_interet~., 
                                 data=bdd_reg3,
                                 method ="seqrep")

reg.summary = summary(regfit.full)

plot_regsubsets(reg.summary)

par(mfrow=c(1 ,1))
plot(regfit.full , scale ="bic")

reg <- lm(data = bdd_reg2, proportion_interet~year+proportion_total+prop_t_minus_1+prop_t_minus_2+year2)
summary(reg)

#### en Facteur.

bdd_reg3<- bdd_reg3%>%
  dplyr::select(-year2)
car::vif(glm(data=bdd_reg3, proportion_interet~.))

regfit.full <- leaps::regsubsets(proportion_interet~., data=bdd_reg3)

reg.summary = summary(regfit.full)

plot_regsubsets(reg.summary)

par(mfrow=c(1 ,1))
plot(regfit.full , scale ="bic") # adjr2 R^2_a, Cp, bic


par(mfrow=c(1,2))
acf(reg$residuals)
pacf(reg$residuals)

spec.ar(reg$residuals)
spec.pgram(reg$residuals, 100)

############
### ZOIB ###
############

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

###############################
### Step by step quoi faire ###
###############################
# Dans Bash
ls ~/.ssh/id_ed25519.pub
ssh-keygen -t ed25519 -C "valentine.cleach@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
cat ~/.ssh/id_ed25519.pub
# Ajouter clef ssh a github:
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICqg4sS84WPnDkdq+gXGx3d/ryAZak4IeZleKT3NbOsP valentine.cleach@gmail.com
# Dans bash a nouveau.
ssh -T git@github.com
git remote -v


###########################
#####   BROUILLONS    #####
###########################

