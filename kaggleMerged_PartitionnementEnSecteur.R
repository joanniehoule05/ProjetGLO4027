library(dplyr)
library(tidyr)
library(lubridate)
#library(visdat)
library(magrittr)
library(ggplot2)
library(stringr)
library(dbscan)
library(broom)

BASE = "/Users/jhoule/Dropbox/Big data/projet/database/"


# Importation des donnees ####
source(paste(PATH_DATABASE, "load_others.R", sep="/"))
source(paste(PATH_DATABASE, "load_dataworld.R", sep="/"))
# Load le fichier des stations de 2011 et 2012 (le merging et nettoyage est fait dans le fichier doStationsFile.R)


# Algorithme de partionnement DBSCAN avec les parametres eps=0.009 et minPts = 6)
dbscanClust<-dbscan(df.stations2 %>% dplyr::select(LATITUDE,LONGITUDE) %>% na.omit(), eps=0.009, minPts = 6)


# Fusion de la base de donnee et des groupes obtenu par l'algo
data.dbscanClust<-cbind(secteur=as.factor(dbscanClust$cluster), df.stations2 %>% na.omit()) %>% mutate(secteur=as.numeric(secteur))



# Ajout de la variable secteur a dataworld
df.dataworld %<>% left_join(. ,
                            data.dbscanClust %>% dplyr::select(start.secteur=secteur, start.station=station), by="start.station")


# Fonction qui calcul la proportion de location pour chaque 
# secteur pour chaque heure.
prop.secteur<-function(df){
  return(df %<>% count(start.secteur) %>%
           mutate(prop=n/sum(n,na.rm=TRUE), n=NULL)%>%
           spread(key = start.secteur, prop))
}

parse_date_time("2012-03-13 04:00:00", "Y/%m/%d %H%M%S")
df.dataworld %>% filter(date(start.date)=="2012-03-13")
parse_date_time("2012-03-13 04:00:00", "Y/%m/%d %H%M%S")
date(df.dataworld$start.date)=="2012-03-13"
df.dataworld %<>%mutate(dateDep=date(start.date)) 

df.dataworldex<-df.dataworld %>% filter(dateDep=="2012-03-13")

# On regroupe les observations par heure et on calcul la proportion de
# de depart par secteur a l'aide de la fonction prop.secteur
df.dataworld3<-df.dataworld %>%
  filter(!is.na(start.secteur))%>%  # On enleve les alta bike stations
  group_by(datetime = floor_date(start.date, unit = "hour")) %>% 
  do(prop.secteur(df=.)) 

# Les NA d'une colonne associe q un secteur implique 
# qu'il y a une proportion de 0% du a la transformation
# de la base de données, donc les NA sont remplacés
# par des 0 dans dans df.dataworld3
df.dataworld3[is.na(df.dataworld3)] <- 0.000000000000

# Vérification que la somme d'une ligne pour les secteur 
# donne bien 1. OK
df.dataworld3 %<>% mutate(tot = sum(`1`,`2`, `3`,`4`,`5`))
sum(df.dataworld3$tot)


# Création de la base de donnees fusionnee 
# de df.kaggle et df.dataworld3.
df.kaggleMerged <-left_join(df.kaggle, df.dataworld3, by="datetime") %>% dplyr::select(-tot)

df.kaggleMerged %<>% rename(secteur1 = `1`,
                           secteur2 = `2`,
                           secteur3 = `3`,
                           secteur4 = `4`,
                           secteur5 = `5`)

write.csv(df.kaggleMerged %>% select(-date), paste(PATH_DATABASE,"kaggleMerged.csv",sep="/"), quote = FALSE, row.names = FALSE)


# Pour avoir des facteurs associe aux pourcentages par secteur.
# Ici, les intervals qui classe les 5 facteurs sont :
# [0, 0.2], (0.2, 0.4] , (0.4, 0.6], (0.6, 0.8], (0.8, 1]
df.kaggleMerged2<-df.kaggleMerged %>% mutate(
                            secteur1 = cut(round(secteur1, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, right = TRUE, labels = FALSE),
                            secteur2 = cut(round(secteur2, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, right = TRUE, labels = FALSE),
                            secteur3 = cut(round(secteur3, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, right = TRUE, labels = FALSE),
                            secteur4 = cut(round(secteur4, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, right = TRUE, labels = FALSE),
                            secteur5 = cut(round(secteur5, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, right = TRUE, labels = FALSE))


df.kaggleMerged2<-df.kaggleMerged %>% mutate(
  secteur1 = cut(round(secteur1, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, labels = c(0.2, 0.4, 0.6, 0.8, 1)),
  secteur2 = cut(round(secteur2, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, labels = c(0.2, 0.4, 0.6, 0.8, 1)),
  secteur3 = cut(round(secteur3, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, labels = c(0.2, 0.4, 0.6, 0.8, 1)),
  secteur4 = cut(round(secteur4, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, labels = c(0.2, 0.4, 0.6, 0.8, 1)),
  secteur5 = cut(round(secteur5, 3), breaks= seq(0.000000000, 1.00000000000,by=0.2), include.lowest = TRUE, labels = c(0.2, 0.4, 0.6, 0.8, 1)))


write.csv(df.kaggleMerged2 %>% dplyr::select(-date), paste(PATH_DATABASE,"kaggleMerged2.csv",sep="/"), quote = FALSE, row.names = FALSE)







