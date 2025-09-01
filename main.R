rm(list=setdiff(ls(), "Total"))
options(encoding = 'UTF-8')

source("init.R")
wd <- set_wd()

library(dplyr);library(sf);library(ggplot2)
library(unmarked)

wd <- set_wd()
set.seed(12345)

Total <- sf::st_read(paste0(wd$data, "derived/Total.shp"),
                     options = "ENCODING=UTF8")
Total <- Total%>%
  transform_Total()

VariablesSite <- sf::st_read(paste0(wd$data, "derived/VariablesSite.shp"),
                             options = "ENCODING=UTF8")
VariablesSite <- VariablesSite%>%
  transform_VarSites()

################################
######## KNIT / RENDER #########
################################

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

rmarkdown::render(paste0(wd$src, "finished/cleaning/Sites.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Sites.html"),
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

rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lineaire/Reg_Lin_CollisionsRoutieres.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_CollisionsRoutieres.html"),
                  encoding="UTF-8")

knitr::knit(paste0(wd$src, "finished/models/Reg_Lineaire/Reg_Lin_CollisionsRoutieres.Rmd"))

rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lineaire/Reg_Lin_ToutesDonnes.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_ToutesDonnes.html"),
                  encoding="UTF-8")

knitr::knit(paste0(wd$src, "finished/models/Reg_Lin_ToutesDonnes.Rmd"))

rmarkdown::render(paste0(wd$src, "finished/models/Reg_Lineaire/Reg_Lin_CollisionsRoutieres_DIRO.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/glm/Reg_Lin_DIRO.html"),
                  encoding="UTF-8")

##### occupancy #####

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/OccupancyLapin.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancyLapin.html"),
                  encoding="UTF-8")


rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/OccupancyPutois.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancyPutois.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/OccupancyHerisson.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancyHerisson.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/temoins/OccuSanglier.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/OccupancySanglier.html"),
                  encoding="UTF-8")

rmarkdown::render(paste0(wd$src, "finished/models/Occupancy/SiteCovariates.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/Occupancy/SiteCovariates.html"),
                  encoding="UTF-8")

################################
######## Brouillons GLM ########
################################
bdd_reg <- tab_glm(Total_et_Diro, 
                   espece_interet = 61714,
                   espece_benchmark = 61667)


reg <- glm(data = bdd_reg, v 
           proportion_interet ~ year+X_10km+Dist_Ecotone+Dnst_Cultures)
sumoisary(reg)
autoplot(reg) # 239 c'est une proportion de 1 en 2010, talus, 11 ans du site...

bptest(reg, studentize = FALSE) # Homosedasticit? okay a 90%
bgtest(reg, type = "F") # On ne peut pas dire qu'il n'y a pas d'autocorr?lation des erreurs
shapiro.test(reg$residuals)

sumoisary(bdd_reg)
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

reg.sumoisary = sumoisary(regfit.full)

plot_regsubsets(reg.sumoisary)

par(mfrow=c(1 ,1))
plot(regfit.full , scale ="bic")

reg <- lm(data = bdd_reg2, proportion_interet~year+proportion_total+prop_t_minus_1+prop_t_minus_2+year2)
sumoisary(reg)

#### en Facteur.

bdd_reg3<- bdd_reg3%>%
  dplyr::select(-year2)
car::vif(glm(data=bdd_reg3, proportion_interet~.))

regfit.full <- leaps::regsubsets(proportion_interet~., data=bdd_reg3)

reg.sumoisary = sumoisary(regfit.full)

plot_regsubsets(reg.sumoisary)

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
sumoisary(sample1)
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

########################
### Unkown key steps ###
########################
# Dans Bash
ls ~/.ssh/id_ed25519.pub 
ssh-keygen -t ed25519 -C "valentine.cleach@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
cat ~/.ssh/id_ed25519.pub
# Ajouter clef ssh a github:
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJaWSroBxK0IQswBQrxYPR6rUpm1IzNoVu5FM3j43nMx valentine.cleach@gmail.com
# Dans bash a nouveau.
ssh -T git@github.com
git remote -v


###########################
#####   BROUILLONS    #####
###########################

library(unmarked);library(dplyr);library(sf);library(ggplot2)

# Map des lapins

grille_10x10 <- sf::st_read(
  paste0(wd$data, "masques/Grille_10x10/Grille_10X10.shp")
)
RegionBretagneConti <- sf::st_read(
  paste0(wd$data, "masques/RegionBretagneConti/RegionBretagneConti.shp")
)

tab <- Total %>%
  dplyr::filter(cd_nom == 61714)%>%
  transforme_carte()
grille_10x10 <- grille_10x10%>%
  transforme_carte()
RegionBretagneConti <- RegionBretagneConti%>%
  transforme_carte()

grille_10x10$density <- lengths(sf::st_intersects(grille_10x10, 
                                                  tab))
grille_10x10 <- sf::st_intersection(grille_10x10, 
                                    RegionBretagneConti)

ggplot() +
  geom_sf(data = RegionBretagneConti) + 
  labs(title = paste0(". Densite des observations du lapin de garenne en bretagne")) +
  geom_sf(data = grille_10x10, aes(fill = density)) +
  scale_fill_gradient(low="white", high="orangered3") +
  theme_bw()

#############################################
#############################################
#############################################

  