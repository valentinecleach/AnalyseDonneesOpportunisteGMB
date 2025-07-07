set_wd <- function(){
  # working directory
  wd <- list()
  # commonly used paths in my working directory
  
  wd$data  <- "~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/data/"
  wd$output <- "~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/output/"
  wd$src <- "~/work/AnalyseDonneesOpportunisteGMB/00_Nouveau/src/"
  
  return(wd)
}

transform_Total <- function(){
  Total <- Total %>%
    mutate_at(c("bdd_rgn", 
                "ett_blg", "tchnq_b", 
                "communs", "obsrvtr", 
                "famille", "ordre", 
                "nm_vrnc", "nom_vld"), 
              .funs = as.factor)%>%
    rename(bdd_originale = bdd_rgn,
           etat_biologique = ett_blg,
           technique_observation = tchnq_b,
           nom_vernaculaire = nm_vrnc,
           nom_valide = nom_vld,
           observateurs = obsrvtr)
  return(Total)
}

transforme_carte <- function(carte){
  carte <- carte%>%
    st_set_crs(2154)%>%
    st_transform(4326)
}


