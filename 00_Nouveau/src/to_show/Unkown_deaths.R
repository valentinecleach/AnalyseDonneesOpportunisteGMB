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
         chmps_d == "{'death_cause': 'UNKNOWN'}")%>%
  group_by(observateurs)%>%
#  filter(n()>30)%>%
  ggplot(aes(observateurs))+
  geom_bar()+coord_flip()+labs(title = "Death_cause : UNKNOWN")

Total%>%
  filter(bdd_originale == "VisioNature",
         chmps_d == "{'death_cause': 'UNKNOWN'}")%>%
  group_by(observateurs)%>%
  #  filter(n()>30)%>%
  ggplot(aes(observateurs))+
  geom_bar()+coord_flip()+labs(title = "Death_cause : UNKNOWN")

# Un des grands contribueurs au death_cause: unkown est daniel bellier
# Comme on l'enlève c'est plus simple


t1 <- Total%>%
  filter(bdd_originale == "VisioNature",
         champs_additionnels == "{'death_cause': 'UNKNOWN'}",
         observateurs != "BELLIER_DANIEL")%>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+coord_flip()+labs(title = "Death_cause : UNKNOWN")

t2 <- Total%>%
  filter(bdd_originale == "VisioNature",
         etat_biologique == "Trouvé mort : impact routier",
         observateurs != "BELLIER_DANIEL")%>%
  ggplot(aes(nom_vernaculaire))+
  geom_bar()+coord_flip()+labs(title = "Impacte routier")

ggarrange(t1, t2)
