
# Test
liste_cd_nom <- Total%>%
  st_drop_geometry()%>%
  distinct(cd_nom)
liste_cd_nom[[1]]

for (i in 1:23){
  pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][i]))
}

pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][6]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][7]))

source(paste0(wd$src,"functions/comparaison_dans_mailles.R"), encoding="utf-8")

as.numeric(liste_cd_nom[[1]][7])

pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][1])) # renard -> 10
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][2])) # Martre -> 10
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][3])) # Sanglier -> nsp 10/20?
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][4])) 
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][5]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][6])) # Lapin de garenne
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][7])) # Fouine -> Problème
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][8]))
######
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][9]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][10]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][11]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][12]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][13]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][14]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][15]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][16]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][17]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][18]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][19]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][20]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][21]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][22]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][23]))

######


library(caret)

jdd <- Total%>%
  select(cd_nom, g__4326, Grid2km, Grid5km, Grid10km, Grid20km)

train_index <- createDataPartition(
  jdd$cd_nom,
  times = 1,
  p = 0.8,
  list = FALSE
)
test_index <- setdiff(1:nrow(jdd), train_index)
train <- jdd[train_index, ]
test <- jdd[test_index, ]

library("class") # fonction knn
library("e1071") # librairie nécessaire pour la fonction tune.knn

train <- train%>%
  st_drop_geometry()

sum(is.na(train))

knn_cross_results <- tune.knn(
  x = train,
  y = train$Grid2km,
  k = 1:50,
  tunecontrol = tune.control(sampling = "cross"),
  cross = 10
)

str(train)
knn_cross_results
best_k <- knn_cross_results$best.parameters[[1]]
cat(paste("Nombre de plus proches voisins optimal :",best_k))

knn_pred <- knn(
  train = train_scaled,
  test = test_scaled,
  cl = train$Class,
  k = best_k
)