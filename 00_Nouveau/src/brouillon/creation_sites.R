library(sf)          #For working with spatial data
library(tidyverse)   #For data wrangling and ggplot2
library(ggspatial)   #For spatial data with ggplot2
library(tigris)

rm(list=setdiff(ls(), "Total"))

source("~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/src/functions/divers.R")
wd <- set_wd()
set.seed(12345)
Total <- st_read(paste0(wd$data,"Total.shp"))
Total <- transform_Total()


#####
##### Quadrillage
#####

Total <- transforme_carte(Total)
Total <- Total %>% st_transform(2154) 

st_is_longlat(Total) # devrait être FALSE pour les mètres.

grid_spacing <- 20000  # size of squares, in units of the CRS (i.e. meters for 5514)

quadrillage <- st_make_grid(Total, square = T, 
                            cellsize = c(grid_spacing, grid_spacing))
  
Grid  <- st_as_sf(Total) %>%
  st_make_grid(square = T, 
               cellsize = c(grid_spacing, grid_spacing))%>%
  cbind(data.frame(ID = sprintf(paste("GRID%0",nchar(length(.)),"d",sep=""), 
                                1:length(.))))

Grid <- st_as_sf(Grid)

ggplot() +
  geom_sf(data = quadrillage)+
  geom_sf(data = Total)

Total <- st_join(Total, Grid, left=TRUE)

Total <- transforme_carte(Total)

t <- Total%>%
  filter(ID == "GRID007")

ggplot() +
  geom_sf(data = Grid)+
  geom_sf(data = t)

Total <- Total %>%
  rename("grid_ID" = "ID",
         "grid_geom" = "geometry")%>%
  select(-"grid_geom")

#####
##### Paysages Bretons 
#####

source("~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/src/functions/divers.R")
wd <- set_wd()
set.seed(12345)
Total <- transforme_carte(Total)


st_crs(Total)

famille_paysage <- st_read(
  paste0(wd$data, "masques/famille_paysage/famille_paysages.shp")
)

famille_paysage$Nom <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Nom)
famille_paysage$Nom <- gsub("\uFFFD", "e", famille_paysage$Nom)

famille_paysage$Famille <- gsub("\uFFFD\uFFFD", "a", famille_paysage$Famille)
famille_paysage$Famille <- gsub("\uFFFD", "e", famille_paysage$Famille)


famille_paysage <- famille_paysage%>%
  transforme_carte()%>%
  filter(CODE_REG != 39)%>%
  st_buffer(famille_paysage, dist = 200)

Total <- st_join(Total, famille_paysage, left = TRUE)

ggplot() +
  geom_sf(data = famille_paysage) +
  geom_sf_text(
    data = famille_paysage,
    aes(label = Nom),
    size = 3,
    color = "forestgreen"
  )

ggplot() +
  geom_sf(data = (Total%>%filter(is.na(Nom))))+
  geom_sf(data=famille_paysage)
  
Total_sans_na <- Total%>%
  filter(!is.na(Nom))

Total <- Total%>%
  mutate(Nom = ifelse(is.na(Nom), 
                      Total$Nom[st_nearest_feature(Total, Total_sans_na)], 
                      Nom),
         Famille = ifelse(is.na(Famille), 
                      Total$Famille[st_nearest_feature(Total, Total_sans_na)], 
                      Famille))
  
Total <- Total %>%
  rename("paysage_ID" = "CODE_REG",
         "paysage_nom" = "Nom",
         "famille_paysage" = "Famille")
