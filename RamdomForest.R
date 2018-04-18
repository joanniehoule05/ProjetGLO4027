

library(qpcR)
library(magrittr)
library(dplyr)
library(lubridate)
library(glmulti)
library("rpart")
library(rpart.plot)
library(rms)
library(caret)
library("rpart")
library(rpart.plot)
library(ggplot2)
library(rms)
library(RColorBrewer)
library(caret)
library(cvTools)
library(stringr)
library(randomForest)
library(Metrics)


divFreqTable<-function(tab, bucketSize=10){
  intervalPoints<-c(-1,0)
  add=0
  # On commence a l'indice 1. count =0 est deja inclut
  for(i in seq(2:dim(tab)[1]-1)){
    add=add+tab[i]
    if(add > bucketSize){
      intervalPoints=append(values = as.numeric(names(tab[i])), intervalPoints)
      add=0 # on remet le compteur a 0
    }
  }
  # Si le maximum n'est pas dans le vecteur, on l'inclut
  if(!(max(as.numeric(names(tab))) %in% intervalPoints)){
    intervalPoints <- append(values = max(as.numeric(names(tab))), intervalPoints)
  }
  # on retourne les valeurs unique (0 peut se trouvé 2 fois sinon) 
  return(unique(intervalPoints))
}

getBorne<-function(x){
  return(as.numeric((str_extract_all(x, "\\-*\\d+\\.*\\d*"))[[1]]))
}
median2<-function(x){
  return(sum(x)/2)
}
intervalToNum<-function(intervalLevels){
  n=sapply(intervalLevels, FUN=getBorne)
  n[1,]<-n[1,]+1
  #Arrondissement des mediane a l'entier superieur
  return(ceiling(apply(n, FUN=median2,MARGIN = 2)))
}


Sys.setenv(LANG = "en")

df.kaggleMerged<-read.csv("C:/Users/Joannie/Dropbox/Big data/projet/database/kaggleMerged.csv")


#2012-03-13 04 valeur manquante pour secteurs
df.kaggleMerged=df.kaggleMerged[-c(6622), ]
df.kaggleMerged$season=as.factor(df.kaggleMerged$season)
df.kaggleMerged$workingday=as.factor(df.kaggleMerged$workingday)
df.kaggleMerged$holiday=as.factor(df.kaggleMerged$holiday)
df.kaggleMerged$weather=as.factor(df.kaggleMerged$weather)
df.kaggleMerged$year=as.factor(df.kaggleMerged$year)
df.kaggleMerged$month=as.factor(month(df.kaggleMerged$datetime))
df.kaggleMerged$hour=as.factor(hour(df.kaggleMerged$datetime))

summary(df.kaggleMerged)

##### Les 5 bases de donnees des secteurs #######
df.Secteur1<-df.kaggleMerged %>% dplyr::select(-secteur2, -secteur3, -secteur4, -secteur5) %>% 
  mutate(Count=round(secteur1*count,digits = 0))

df.Secteur2<-df.kaggleMerged %>% dplyr::select(-secteur1, -secteur3, -secteur4, -secteur5) %>% 
  mutate(Count=round(secteur2*count,digits = 0))

df.Secteur3<-df.kaggleMerged %>% dplyr::select(-secteur1, -secteur2, -secteur4, -secteur5) %>% 
  mutate(Count=round(secteur3*count,digits = 0))

df.Secteur4<-df.kaggleMerged %>% dplyr::select(-secteur1, -secteur2, -secteur3, -secteur5) %>% 
  mutate(Count=round(secteur4*count,digits = 0))

df.Secteur5<-df.kaggleMerged %>% dplyr::select(-secteur1, -secteur2, -secteur3, -secteur4) %>% 
  mutate(Count=round(secteur5*count,digits = 0))

df.Secteur1$tempTrans=cut(df.Secteur1$temp, breaks = 5*(0.00:9.00))
df.Secteur2$tempTrans=cut(df.Secteur2$temp, breaks = 5*(0.00:9.00))
df.Secteur3$tempTrans=cut(df.Secteur3$temp, breaks = 5*(0.00:9.00))
df.Secteur4$tempTrans=cut(df.Secteur4$temp, breaks = 5*(0.00:9.00))
df.Secteur5$tempTrans=cut(df.Secteur5$temp, breaks = 5*(0.00:9.00))



##### Secteur 1 #######
df.Secteur1$weather=ifelse(df.Secteur1$weather ==4,3,df.Secteur1$weather)
df.Secteur1$weather=as.factor(df.Secteur1$weather)
df.Secteur1$wday=as.factor(as.numeric(df.Secteur1$wday))
df.Secteur1$CountSp=cut(df.Secteur1$Count, breaks = divFreqTable(table(df.Secteur1$Count), bucketSize=544.3))


set.seed(2)
train <- sample(nrow(df.Secteur1), 0.7*nrow(df.Secteur1), replace = FALSE)
TrainSet1 <- df.Secteur1[train,]
ValidSet1 <- df.Secteur1[-train,]

train.randomForest1 <- randomForest(CountSp ~ humidity+
                                      season+
                                      tempTrans+
                                      holiday+
                                      workingday+
                                      weather+
                                      windspeed+
                                      wday+
                                      hour+
                                      month+
                                      year,
                                    data=TrainSet1,
                                    ntree = 500,
                                    mtry =6,
                                    maxnodes=2286,
                                    do.trace = 100,
                                    importance = TRUE)



rmsleSecteur1=rmsle(ValidSet1$Count, intervalToNum(predict(train.randomForest1, ValidSet1,type = "class" )))
pred.prob1  <- predict(train.randomForest1, ValidSet1, type = "class")
#2e argument en haut premier argument a gauche
conf.secteur1 <- table( pred.prob1, ValidSet1$CountSp )

recal.secteur1=sum(diag(conf.secteur1))/sum(colSums(conf.secteur1))
Precision.secteur1=sum(diag(conf.secteur1))/sum(rowSums(conf.secteur1))
F.secteur1=(2*recal.secteur1*Precision.secteur1)/(recal.secteur1+Precision.secteur1)

importance(train.randomForest1)        
varImpPlot(train.randomForest1)  

##### Secteur 2 #######
df.Secteur2$weather=ifelse(df.Secteur2$weather ==4,3,df.Secteur2$weather)
df.Secteur2$weather=as.factor(df.Secteur2$weather)
df.Secteur2$wday=as.factor(as.numeric(df.Secteur2$wday))

df.Secteur2$CountSp=cut(df.Secteur2$Count, breaks = divFreqTable(table(df.Secteur2$Count), bucketSize=544.3))

set.seed(2)
train <- sample(nrow(df.Secteur2), 0.7*nrow(df.Secteur2), replace = FALSE)
TrainSet2 <- df.Secteur2[train,]
ValidSet2 <- df.Secteur2[-train,]

train.randomForest2 <- randomForest(CountSp ~ humidity+
                                      season+
                                      tempTrans+
                                      holiday+
                                      workingday+
                                      weather+
                                      windspeed+
                                      wday+
                                      hour+
                                      month+
                                      year,
                                    data=TrainSet2,
                                    ntree = 500,
                                    mtry =6,
                                    maxnodes=2286,
                                    do.trace = 100,
                                    importance = TRUE)


pred.prob2  <- predict(train.randomForest2, ValidSet2, type = "class")
mean(pred.prob2 == ValidSet2$CountSp) 
#2e argument en haut premier argument a gauche
conf.secteur2 <- table( pred.prob2, ValidSet2$CountSp )

recal.secteur2=sum(diag(conf.secteur2))/sum(colSums(conf.secteur2))
Precision.secteur2=sum(diag(conf.secteur2))/sum(rowSums(conf.secteur2))
F.secteur2=(2*recal.secteur2*Precision.secteur2)/(recal.secteur2+Precision.secteur2)

importance(train.randomForest2)        
varImpPlot(train.randomForest2)  

rmsleSecteur2=rmsle(ValidSet2$Count, intervalToNum(predict(train.randomForest2, ValidSet2,type = "class" )))

##### Secteur 3  #######
df.Secteur3$CountSp<-cut(df.Secteur3$Count,breaks=seq(0, 250,by=25), include.lowest = TRUE)
table(df.Secteur3$Count)
df.Secteur3$weather=ifelse(df.Secteur3$weather ==4,3,df.Secteur3$weather)
df.Secteur3$weather=as.factor(df.Secteur3$weather)
df.Secteur3$wday=as.factor(as.numeric(df.Secteur3$wday))
df.Secteur3$CountSp=cut(df.Secteur3$Count, breaks = divFreqTable(table(df.Secteur3$Count), bucketSize=544.3))

set.seed(2)
train <- sample(nrow(df.Secteur3), 0.7*nrow(df.Secteur3), replace = FALSE)
TrainSet3 <- df.Secteur3[train,]
ValidSet3 <- df.Secteur3[-train,]

train.randomForest3 <- randomForest(CountSp ~ humidity+
                                      season+
                                      tempTrans+
                                      holiday+
                                      workingday+
                                      weather+
                                      windspeed+
                                      wday+
                                      hour+
                                      month+
                                      year,
                                    data=TrainSet3,
                                    ntree = 500,
                                    mtry =6,
                                    maxnodes=2286,
                                    do.trace = 100,
                                    importance = TRUE)


pred.prob3  <- predict(train.randomForest3, ValidSet3, type = "class")
conf.secteur3 <- table( pred.prob3, ValidSet3$CountSp )

recal.secteur3=sum(diag(conf.secteur3))/sum(colSums(conf.secteur3))
Precision.secteur3=sum(diag(conf.secteur3))/sum(rowSums(conf.secteur3))
F.secteur3=(2*recal.secteur3*Precision.secteur3)/(recal.secteur3+Precision.secteur3)

importance(train.randomForest3)        
varImpPlot(train.randomForest3)  

rmsleSecteur3=rmsle(ValidSet3$Count, intervalToNum(predict(train.randomForest3, ValidSet3,type = "class" )))

##### Secteur 4 #######
df.Secteur4$weather=ifelse(df.Secteur4$weather ==4,3,df.Secteur4$weather)
df.Secteur4$weather=as.factor(df.Secteur4$weather)
df.Secteur4$wday=as.factor(as.numeric(df.Secteur4$wday))
df.Secteur4$CountSp=cut(df.Secteur4$Count, breaks = divFreqTable(table(df.Secteur4$Count), bucketSize=544.3))

set.seed(2)
train <- sample(nrow(df.Secteur4), 0.7*nrow(df.Secteur4), replace = FALSE)
TrainSet4 <- df.Secteur4[train,]
ValidSet4 <- df.Secteur4[-train,]

train.randomForest4 <- randomForest(CountSp ~ humidity+
                                      season+
                                      tempTrans+
                                      holiday+
                                      workingday+
                                      weather+
                                      windspeed+
                                      wday+
                                      hour+
                                      month+
                                      year,
                                    data=TrainSet4,
                                    ntree = 500,
                                    mtry =6,
                                    maxnod=2886,
                                    do.trace = 100,
                                    importance = TRUE)


pred.prob4  <- predict(train.randomForest4, ValidSet4, type = "class")
mean(pred.prob4 == ValidSet4$CountSp) 
#2e argument en haut premier argument a gauche
conf.secteur4 <- table( pred.prob4, ValidSet4$CountSp )

recal.secteur4=sum(diag(conf.secteur4))/sum(colSums(conf.secteur4))
Precision.secteur4=sum(diag(conf.secteur4))/sum(rowSums(conf.secteur4))
F.secteur4=(2*recal.secteur4*Precision.secteur4)/(recal.secteur4+Precision.secteur4)

importance(train.randomForest4)        
varImpPlot(train.randomForest4)  

rmsleSecteur4=rmsle(ValidSet4$Count, intervalToNum(predict(train.randomForest4, ValidSet4,type = "class" )))


##### Secteur 5 test #######
df.Secteur5$weather=ifelse(df.Secteur5$weather ==4,3,df.Secteur5$weather)
df.Secteur5$weather=as.factor(df.Secteur5$weather)
df.Secteur5$wday=as.factor(as.numeric(df.Secteur5$wday))
df.Secteur5$CountSp=cut(df.Secteur5$Count, breaks = divFreqTable(table(df.Secteur5$Count), bucketSize=544.3))


set.seed(2)
train <- sample(nrow(df.Secteur5), 0.7*nrow(df.Secteur5), replace = FALSE)
TrainSet5 <- df.Secteur5[train,]
ValidSet5 <- df.Secteur5[-train,]

train.randomForest5 <- randomForest(CountSp ~ humidity+
                                      season+
                                      tempTrans+
                                      holiday+
                                      workingday+
                                      weather+
                                      windspeed+
                                      wday+
                                      hour+
                                      month+
                                      year,
                                    data=TrainSet5,
                                    ntree = 500,
                                    mtry =6,
                                    maxnodes=2286,
                                    do.trace = 100,
                                    importance = TRUE)



pred.prob5  <- predict(train.randomForest5, ValidSet5, type = "class")
mean(pred.prob5 == ValidSet5$CountSp) 
#2e argument en haut premier argument a gauche
conf.secteur5 <- table( pred.prob5, ValidSet5$CountSp )

recal.secteur5=sum(diag(conf.secteur5))/sum(colSums(conf.secteur5))
Precision.secteur5=sum(diag(conf.secteur5))/sum(rowSums(conf.secteur5))
F.secteur5=(2*recal.secteur5*Precision.secteur5)/(recal.secteur5+Precision.secteur5)

importance(train.randomForest5)        
varImpPlot(train.randomForest5)  


rmsleSecteur5=rmsle(ValidSet5$Count, intervalToNum(predict(train.randomForest5, ValidSet5,type = "class" )))


################ Soumission Kaggle #######################################
test1<-read.csv("C:/Users/Joannie/Dropbox/Big data/projet/database/test.csv")
test=test1
Sys.setenv(LANG = "en")

test %<>% mutate(date = as_date(datetime),
                 year = as.factor(year(datetime)),
                 month = as.factor(month(datetime,label=TRUE)),
                 datetime = parse_date_time(datetime, c('%m/%d/%Y %I:%M','%Y/%m/%d %H%M%S')),
                 workingday=as.factor(workingday),
                 season=as.factor(case_when(season==1~"winter",season==2~"spring",
                                            season==3~"summer",season==4~"fall")),
                 holiday=as.factor(holiday),
                 wday = as.factor(as.numeric(wday(date,label=TRUE))))

test$tempTrans=cut(test$temp, breaks = 5*(0.00:9.00))
test$weather=ifelse(test$weather ==4,3,test$weather)
test$weather=as.factor(test$weather)
test$hour=as.factor(hour(test$datetime))
test$month=as.factor(month(test$datetime))


rep1=predict(train.randomForest1, test, type = "class")
rep2=predict(train.randomForest2, test, type = "class")
rep3=predict(train.randomForest3, test, type = "class")
rep4=predict(train.randomForest4, test, type = "class")
rep5=predict(train.randomForest5, test, type = "class")

rep=intervalToNum(rep1)+intervalToNum(rep2)+intervalToNum(rep3)+intervalToNum(rep4)+intervalToNum(rep5)

result<-data.frame(test1$datetime, rep)
rep=round(rep,0)
rep=as.data.frame(rep)
rep=cbind(test1$datetime, rep)
names(rep)=c("datetime","count")
write.csv(rep,"C:/Users/Joannie/Dropbox/Big data/projet/database/ResultTree_classe20_mtry6_maxmodes2286.csv",row.names =FALSE, quote = FALSE )

predTest <- intervalToNum(predict(train.randomForest1, ValidSet1))+
  intervalToNum(predict(train.randomForest2, ValidSet2))+
  intervalToNum(predict(train.randomForest3, ValidSet3))+
  intervalToNum(predict(train.randomForest4, ValidSet4))+
  intervalToNum(predict(train.randomForest5, ValidSet5))

ValidSetCount=ValidSet1$Count+
  ValidSet2$Count+
  ValidSet3$Count+
  ValidSet4$Count+
  ValidSet5$Count
rmsleGlobal=rmsle(ValidSetCount, predTest)
