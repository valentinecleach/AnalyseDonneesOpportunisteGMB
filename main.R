library(renv)
renv::repair()
loadedNamespaces()

rm(list=setdiff(ls(), "Total"))

options(encoding = 'UTF-8')
source("init.R")

library(dplyr)
library(sf)
library(ggplot2)

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

Total <- sf::st_read(paste0(wd$data, "derived/TotalComplet.shp"), 
                     options = "ENCODING=UTF8")
Total <- transform_Total()

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

rmarkdown::render(paste0(wd$src, "finished/models/OccupancyLapin.Rmd"),
                  output_file = paste0(wd$output, 
                                       "models/OccupancyLapin.html"),
                  encoding="UTF-8")

#### Recap Semaines ####

rmarkdown::render(paste0(wd$src, "finished/Recap semaine/S9.Rmd"),
                  output_file = paste0(wd$output, 
                                       "recap/S9.html"),
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
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGkbmhO7R0CRFQUs4wbsT+6z+fU/JMwAlCxRqsuX2rix valentine.cleach@gmail.com
# Dans bash a nouveau.
ssh -T git@github.com
git remote -v


###########################
#####   BROUILLONS    #####
###########################

library(unmarked)
library(dplyr)
library(sf)
library(ggplot2)

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



###############################
##### Méthode grenouilles #####
###############################


mois <- c('Jan', 'Fev', 'Mars', 'Avr', 'Mai', 'Juin', 
          'Juil', 'Aout', 'Sept', 'Oct', 'Nov', 'Dec') # extract month with data
indyear <- c(2006:2015, 2015:2024) # define two periods
ids <- unique(Total$Code_10km)
detections <- list()
for (k in 1:length(indyear)){
  detections[[k]] <- matrix(0,
                            nrow=length(ids),
                            ncol=length(mois))
  data <- filter(Lapins,
                 year==indyear[k])
  ind <- 1
  for (i in mois){
    temp <- filter(data, mois==i)
    utm_temp <- unique(temp$utm)
    for (j in utm_temp){
      detections[[k]][ids==j,ind] <- 1
    }
    ind <- ind + 1
  }
}
det <- do.call(cbind, detections) # bind data from all year in columns
# convert the occupancy dataset in a 3D array:
y <- list()
ind <- 0
for (i in 1:length(indyear)){
  mask <- (ind + i):(ind + i + length(mois) - 1)
  y[[i]] <- det[,mask]
  ind <- ind + length(mois) - 1
}

y <- array(unlist(y), 
           dim = c(nrow(y[[1]]), ncol(y[[1]]), length(y)))

new_y <- NULL
for (i in 1:dim(y)[3]){ # loop over years
  new_y <- cbind(new_y,apply(y[,1:12,i],1,sum))
}
new_y <- (new_y > 0) 
annee <- apply(new_y[,1:10],1,sum)
y2 <- apply(new_y[,11:20],1,sum)
y <- cbind(annee,y2)

dim(y)
summary(y)
range(y)


####################
###### Lapins ######
####################

Total <- sf::st_read(paste0(wd$data, "derived/Total.shp"),
                     options = "ENCODING=UTF8")
Total <- Total%>%
  transform_Total()

VariablesSite <- sf::st_read(paste0(wd$data, "derived/VariablesSite.shp"),
                             options = "ENCODING=UTF8")
VariablesSite <- VariablesSite%>%
  transform_VarSites()

Lapins <- Total%>%
  sf::st_join(VariablesSite)%>%
  filter(cd_nom == 61714,
         technique_observation == "Vu",
         date > as.Date("2005-01-01"))%>%
  # select(-c(insee_dept, lib_dept, lib_dept, FID, surf, CD_SIG)) %>%
  select(-nom_valide, -nom_vernaculaire, -ordre, -technique_observation)%>%
  mutate(mois = lubridate::month(date),
         mois = case_when(mois == 1 ~ "Jan", 
                          mois == 2 ~ "Fev",
                          mois == 3 ~ "Mars",
                          mois == 4 ~ "Avr",
                          mois == 5 ~ "Mai",
                          mois == 6 ~ "Juin",
                          mois == 7 ~ "Juil",
                          mois == 8 ~ "Aout",
                          mois == 9 ~ "Sept",
                          mois == 10 ~ "Oct",
                          mois == 11 ~ "Nov",
                          mois == 12 ~ "Dec"),
         year = lubridate::year(date))

## Matrice de detection

t_lapins <- Lapins %>%
  sf::st_drop_geometry()%>%
  dplyr::mutate(annee = lubridate::year(date))%>%
  dplyr::select(Code_10km, annee)%>%
  dplyr::count(Code_10km, annee) %>%
  tidyr::complete(Code_10km, 
                  annee = tidyr::full_seq(annee, 1), 
                  fill = list(n = 0)) %>%
  tidyr::pivot_wider(names_from = annee, 
                     values_from = n) %>%
  dplyr::arrange(Code_10km) %>%
  filter(!is.na(Code_10km))

t_lapins[,-1] <- ifelse(t_lapins[,-1]>0, 1, 0)

noms <- t_lapins$Code_10km
t_lapins <- as.matrix(t_lapins[, -1])
rownames(t_lapins) <- noms


## Matrice des covariables des observations (periode et mois) 

annees <- as.numeric(colnames(t_lapins))
periodes_vector <- ifelse(annees < 2015, "periode1", "periode2")

periodes_matrix <- matrix(rep(periodes_vector, each = nrow(t_lapins)), 
                          nrow = nrow(t_lapins), 
                          byrow = FALSE)
colnames(periodes_matrix) <- colnames(t_lapins)
rownames(periodes_matrix) <- rownames(t_lapins)

periodes_df <- as.data.frame(periodes_matrix)


# Matrice des covariables du site (Dist_Eau, etc..)

site_info <- VariablesSite%>%
  dplyr::arrange(Code_10km)
  

noms <- site_info$Code_10km
rownames(site_info) <- noms

site_info <- site_info%>%
  dplyr::select(-Code_10km)%>%
  sf::st_drop_geometry()

# Join the site descriptor to your t_lapins table
t_lapins <- t_lapins %>%
  dplyr::left_join(site_info, by = "Code_10km")

View(t_lapins)


# Modelisation

View(periodes_df)
View(site_info)
umf <- unmarked::unmarkedFrameOccu(y = t_lapins, 
                                   obsCovs = list(periode = periodes_df),
                                   siteCovs = site_info
                                   )
                                   
# Null model (no effect of era)
model_null <- unmarked::occu(~1 ~1, data = umf)
model_era <- unmarked::occu(~periode ~1, data = umf)
model_eau <- unmarked::occu(~periode ~Dist_Eau, data = umf)

unmarked::summary(model_null)
unmarked::summary(model_era)
unmarked::summary(model_eau)

# Ici, on compare les 2 modèles a l'aide de l'AIC
unmarked::modSel(unmarked::fitList(aucune_variable = model_null, 
                                   deux_periodes = model_era,
                                   dp_et_eau = model_eau))


#######################################
################# ESSAIS ##############
#######################################
