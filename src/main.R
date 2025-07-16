wd <- set_wd()

source("~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/functions/divers.R")
Total <- st_read(paste0(wd$data,"Total.shp"))
Total <- transform_Total()

#### Nettoyage de données ####
# Total:
rmarkdown::render(paste0(wd$src, "finished/cleaning/Total.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Total.html"))

rmarkdown::render(paste0(wd$src, "finished/cleaning/Diro.Rmd"), 
                  output_file = paste0(wd$output, "cleaning/Diro.html"))

#### Stats Desc ####
rmarkdown::render(paste0(wd$src, "finished/stats_desc/Repartitions.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Repartitions.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Geographie.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Geographie.html"))

rmarkdown::render(paste0(wd$src, "finished/stats_desc/Observateurs.Rmd"), 
                  output_file = paste0(wd$output, "stats_desc/Observateurs.html"))


library(formatR)
?tidy.source()


#### Classification ####

rmarkdown::render(paste0(wd$src, "finished/acp/Ordres.Rmd"), 
                  output_file = paste0(wd$output, "acp/Ordres.html"))



#### Regression ####





#  scale_fill_gradient(low="#fbf0d1", high="#daa702") +
