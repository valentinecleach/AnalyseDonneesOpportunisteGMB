library(renv)
renv::restore()
renv::install("Factoshiny")

wd <- set_wd()
set.seed(12345)

Total <- sf::st_read(paste0(wd$data,"derived/Total.shp"))
Total <- transform_Total()

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



#  scale_fill_gradient(low="#fbf0d1", high="#daa702") +
