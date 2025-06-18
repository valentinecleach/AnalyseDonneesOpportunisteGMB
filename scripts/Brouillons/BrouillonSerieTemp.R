VisioN_FB %>%
  ggplot(aes(date_debut, nombre_min))+
  geom_point()+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red") + 
  labs(title="Le nombre d'individus enregistré par observation reparti dans le temps",
       subtitle="Données de VisioNature",
       y="Nombre d’individus")

VisioN_FB%>%
  filter(nombre_min>99)%>%
  select(nom_vernaculaire, nombre_min, date_debut, communes, observateurs)

summary(GeoN%>%
          filter(nombre_min>10)%>%
          select(nom_vernaculaire, nombre_min, date_debut, communes, observateurs)%>%
          mutate(nom_vernaculaire = as.factor(nom_vernaculaire),
                 observateurs = as.factor(observateurs),
                 communes = as.factor(communes))
) 

summary(VisioN_FB%>%
          filter(nombre_min>10)%>%
          select(nom_vernaculaire, nombre_min, date_debut, communes, observateurs)%>%
          mutate(nom_vernaculaire = as.factor(nom_vernaculaire),
                 observateurs = as.factor(observateurs),
                 communes = as.factor(communes))
) 



summary(VisioN_FB%>%
          filter(nombre_min>30)%>%
          select(nom_vernaculaire, nombre_min, date_debut, communes, observateurs)%>%
          mutate(nom_vernaculaire = as.factor(nom_vernaculaire),
                 observateurs = as.factor(observateurs),
                 communes = as.factor(communes))
) 


GeoN%>%
  ggplot(aes(date_debut, nombre_min))+
  geom_point()+
  theme_bw()+
  scale_fill_grey(start = 0.2,
                  end = 0.8, 
                  na.value = "red") + 
  labs(title="Le nombre d'individus enregistré par observation reparti dans le temps",
       subtitle="Données de GeoNature",
       y="Nombre d’individus")


ts_hermine <- GeoN%>%
  filter(ordre=="Carnivora")%>%
  filter(nom_vernaculaire == "Hermine" )%>%
  select(date_debut)%>%
  mutate(presence=1) %>%
  complete(date_debut = seq.Date(min(date_debut), max(date_debut), by="day"))%>%
  mutate(presence = ifelse(is.na(presence), 0, presence))

#####                      #####
##### BROUILLON SERIE TEMP #####
#####                      #####

library(lubridate)
library(tidyr)
library(forecast)

#####            #####
##### Hermine GN #####
#####            #####

ts_hermine <- GeoN %>%
  filter(ordre == "Carnivora") %>%
  filter(nom_vernaculaire == "Hermine") %>%
  select(date_debut) %>%
  mutate(presence = 1) %>%
  complete(date_debut = seq.Date(min(date_debut), max(date_debut), by = "day")) %>%
  mutate(presence = ifelse(is.na(presence), 0, presence)) %>%
  mutate(year = year(date_debut), 
         month = month(date_debut)) %>%
  group_by(year, month) %>%
  summarise(monthly_avg_presence = mean(presence), .groups = "drop")

str(ts_hermine)

ts <- ts(ts_hermine$monthly_avg_presence, start = c(1973,8), frequency=12)

plot(ts)
acf(ts, lag=40)
pacf(ts, lag=40)
auto.arima()
grid()
spec.ar(ts)
spec.pgram(ts, spans=16)

#####            #####
##### Hermine VN #####
#####            #####
ts_hermine <- VisioN_FB %>%
  filter(date_debut> as.Date("2010-01-01"))%>%
  filter(ordre == "Carnivora") %>%
  filter(nom_vernaculaire == "Hermine") %>%
  select(date_debut) %>%
  mutate(presence = 1) %>%
  complete(date_debut = seq.Date(min(date_debut), max(date_debut), by = "day")) %>%
  mutate(presence = ifelse(is.na(presence), 0, presence)) %>%
  mutate(year = year(date_debut), 
         month = month(date_debut)) %>%
  group_by(year, month) %>%
  summarise(monthly_avg_presence = mean(presence), .groups = "drop")

str(ts_hermine)

ts <- ts(ts_hermine$monthly_avg_presence, start = c(2010,8), frequency=12)

plot(ts, main="serie temp de l'hermine")
acf(ts, lag=40) # rien de significatif
pacf(ts, lag=40) # Pareil. Bruit blanc
auto.arima(ts)
grid()
spec.ar(ts)
spec.pgram(ts, spans=4)

model <- arima(ts, order=c(2,0,1))
model <- arima(ts, order=c(1,0,0))

tsdiag(model, lags=20)

tsdiag(model, lag=20)
qqnorm(model$residuals)
# On ne veut pas la moyenne mais une autre valeurs utilisant:
# - pression d'observation -> abondance relative?
# - 


