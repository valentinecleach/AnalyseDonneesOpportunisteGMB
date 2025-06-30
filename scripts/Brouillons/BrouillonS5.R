#############
## Road VN ##
#############


setwd("~/work/AnalyseDonneesOpportunisteGMB/donnees_brutes")

donnees_VisioNature_FB <- read_delim("VisioN_FB_2025-06-05T09_19_40.789Z.csv",
                                     delim = ";", 
                                     escape_double = FALSE,
                                     trim_ws = TRUE)

VM <- donnees_VisioNature_FB%>%
  mutate(etat_biologique = ifelse(champs_additionnels %in% c("{'death_cause': 'ROAD_VEHICLE'}",
                                                             "{'death_cause': 'OTHER_TRANSPORT'}",
                                                             "{'death_cause': 'UNKNOWN_TRANSPORT'}"),
                                  "Trouvé mort : impact routier",
                                  etat_biologique),
         etat_biologique = ifelse(grepl("ROUT", toupper(comment_occurrence)), 
                                  "Trouvé mort : impact routier",
                                  etat_biologique)
         )%>%
  select(comment_occurrence, champs_additionnels, etat_biologique)
