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
         )

########################
## Répartition UNKOWN ##
########################

rm(list=setdiff(ls(), "Total"))


t1 <- Total%>%
  filter(bdd_originale == "VisioNature",
         champs_additionnels == "{'death_cause': 'UNKNOWN'}")%>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+coord_flip()+labs(title = "Death_cause : UNKNOWN")

t2 <- Total%>%
  filter(bdd_originale == "VisioNature",
         etat_biologique == "Trouvé mort : impact routier")%>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+coord_flip()+labs(title = "Impacte routier")

ggarrange(t1, t2)

Total%>%
  filter(bdd_originale == "VisioNature",
         champs_additionnels == "{'death_cause': 'UNKNOWN'}")%>%
  group_by(observateurs)%>%
  filter(n()>30)%>%
  ggplot(aes(observateurs))+
  geom_bar()+coord_flip()+labs(title = "Death_cause : UNKNOWN")
