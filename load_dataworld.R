
library(dplyr)
library(tidyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(stringr)
library(readr)

PATH_DATABASE = ""

###  setter le dossier de base de donneees selon vince ou jo
PATH_DATABASE = "C:\\Users\\Joannie\\Dropbox\\Big data\\projet\\database"

#### Importation des fichiers de location Data.World  #####

### Importer directement de url
df.2011.Q1 <- read_csv("https://query.data.world/s/6MJ01e1UXj5jLzmedZSidL0RxJq8uM")
df.2011.Q2 <- read_csv("https://query.data.world/s/fBAXyKLtlBVu92P9Ql7bw6OyqZThrq")
df.2011.Q3 <- read_csv("https://query.data.world/s/rjAA8CATPz3-LRxVI5-ZA1mnFiNCDA")
df.2011.Q4 <- read_csv("https://query.data.world/s/aPropfvPFnHEz_rw6X-7Oas8gaT6Yj")

df.2012.Q1 <- read_csv("https://query.data.world/s/q2IYDHWUALa0B8b_XzQfOWxUbIpyZO")
df.2012.Q2 <- read_csv("https://query.data.world/s/Z7D5w0IzYQQaTj1cFSKATaXpIfDiU-")
df.2012.Q3 <- read_csv("https://query.data.world/s/dWyaF_YYkSTCR2JG7ywUVx9jbCJYg4")
df.2012.Q4 <- read_csv("https://query.data.world/s/k0CPwiDWrMtqeOnzssUvgdT9aBR55V")



##### Mettre les noms d'attribut Data.world#####

## Les noms des attributs sont mis en miniscule 
names(df.2011.Q1) %<>% tolower %>% str_replace( " ", ".")
names(df.2011.Q2) %<>% tolower %>% str_replace( " ", ".")
names(df.2011.Q3) %<>% tolower %>% str_replace( " ", ".")
names(df.2011.Q4) %<>% tolower %>% str_replace( " ", ".")

names(df.2012.Q1) %<>% tolower %>% str_replace( " ", ".")
names(df.2012.Q2) %<>% tolower %>% str_replace( " ", ".")
names(df.2012.Q3) %<>% tolower %>% str_replace( " ", ".")
names(df.2012.Q4) %<>% tolower %>% str_replace( " ", ".")


## Les attributs associée au type d'utilisateur est renommé lorsqu'elle n'est pas appellé Member.Type
df.2011.Q1 %<>% dplyr::rename(bike = `bike#`)
df.2011.Q2 %<>% dplyr::rename(bike = `bike#`)
df.2011.Q3 %<>% dplyr::rename(bike = `bike#`)
df.2011.Q4 %<>% dplyr::rename(bike = `bike#`)

df.2012.Q1 %<>% dplyr::rename(bike = `bike#`, member.type = type)
df.2012.Q2 %<>% dplyr::rename(member.type = bike.key, start.station = start.station, bike = `bike#`)
df.2012.Q3 %<>% dplyr::rename(member.type = subscriber.type, start.station = start.station, bike = `bike#`)
df.2012.Q4 %<>% dplyr::rename(member.type = subscription.type, bike = `bike#`)

### Merge des données en un data.frame

df.dataworld<-rbind(df.2011.Q1, df.2011.Q2, df.2011.Q3, df.2011.Q4, df.2012.Q1, df.2012.Q2, df.2012.Q3, df.2012.Q4)

df.dataworld %<>% data.frame()

## Pour enlever les autres data.frame de l'espace memoire
rm(df.2011.Q1, df.2011.Q2, df.2011.Q3, df.2011.Q4, df.2012.Q1, df.2012.Q2, df.2012.Q3, df.2012.Q4)


## Transformations des variables de la base de donnée de dataworld ####
df.dataworld  %<>% mutate(start.date = parse_date_time(start.date, "%m/%d/%Y %H%M"),
                          end.date = parse_date_time(end.date, "%m/%d/%Y %H%M"),
                          durationD = as.duration(hms(duration)),
                          member.type = as.factor(member.type),
                          start.station = as.factor(start.station),
                          end.station = as.factor(end.station),
                          start.station=as.factor(str_replace_all(start.station,"\\s\\([^()]+\\)", "")),
                          start.station=as.factor(str_replace_all(start.station,"\\s$", "")),
                          end.station=as.factor(str_replace_all(end.station,"\\s\\([^()]+\\)", "")),
                          end.station=as.factor(str_replace_all(end.station,"\\s$", "")))


### Pour exporter le df_all en .csv

#write_csv(df.dataworld, paste(PATH_DATABASE,"locations_data.world.csv",sep="/"))



