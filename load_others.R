library(dplyr)
library(tidyr)
library(lubridate)
#library(visdat)
library(magrittr)
library(stringr)
library(readr)


PATH_DATABASE = "C:\\Users\\Joannie\\Dropbox\\Big data\\projet\\database"


#### Importation des data  de Kaggle ##### 
df.kaggle<- read.csv(paste(PATH_DATABASE,"Bike_train.csv",sep="/"))


df.kaggle %<>%  mutate(date = as_date(datetime),
                            year = as.factor(year(datetime)),
                            month = as.factor(month(datetime,label=TRUE)),
                            datetime = parse_date_time(datetime, c('%m/%d/%Y %I:%M','%Y/%m/%d %H%M%S')),
                            workingday=as.factor(workingday),
                            weather=as.factor(weather),
                            season=as.factor(case_when(season==1~"winter",season==2~"spring",
                                                       season==3~"summer",season==4~"fall")),
                            holiday=as.factor(holiday),
                            wday = wday(date,label=TRUE) )


#### Importation du fichier de stations de 201x #####
df.stations <- read.csv(paste(PATH_DATABASE, "Capital_Bike_Share_Locations.csv",sep="/"))

#### Importation des fichiers des stations de Data.world #####
df.stations2<-read.csv(paste(PATH_DATABASE, "stations2011-2012.csv",sep="/"))  %>%
  mutate(station=as.factor(station))%>% dplyr::select(-X)
