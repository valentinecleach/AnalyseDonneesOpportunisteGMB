
# Test
liste_cd_nom <- Total%>%
  st_drop_geometry()%>%
  distinct(cd_nom)
liste_cd_nom[[1]]

for (i in 1:23){
  pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][i]))
}

source(paste0(wd$src,"functions/comparaison_dans_mailles.R"), encoding="utf-8")

pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][1]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][2]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][3]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][4]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][5]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][6]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][7]))
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][8]))
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
pour_chaque_maillage(as.numeric(liste_cd_nom[[1]][23s]))
