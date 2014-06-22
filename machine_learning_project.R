#####wczytuję dane#############
require(caret)
setwd("/home/kacper/datasciencecoursera")
getwd()
dane_treningowe<-read.csv("pml-training.csv")
dane_testowe<-read.csv("pml-testing.csv")
#########sprawdzam korelacje
cor(dane_treningowe)

summary(dane_treningowe)
head(dane_treningowe)
summary(dane_treningowe$classe)

########wykresy przykładowe#######
require(ggplot2)

ggplot(data=dane_treningowe,aes(x=pitch_dumbbell,y=roll_dumbbell,group=classe,colour=classe))+geom_point()

dane2<-data.frame(X=dane_treningowe$X)
nazwy<-c(rep(NA,160))

for(i in 2:160){
 if (sum(is.na(dane_treningowe[,i]))==0){ dane2<-data.frame(dane2,dane_treningowe[,i])
 nazwy[i]<-colnames(dane_treningowe)[i]
 
 
 
 
 }}
  gc()
nazwy<-c("x",na.omit(nazwy))
colnames(dane2)<-nazwy
dane2<-dane2[,-1]
i=11
dane3<-data.frame(rep(NA,19622))
nazwy2<-c(rep(NA,92))
for(i in 1:92){
  if (sum(dane2[,i]=="")==0){ dane3<-data.frame(dane3,dane2[,i])
                                           nazwy2[i]<-colnames(dane2)[i]
  }}
dane3<-dane3[,-1]

nazwy2<-na.omit(nazwy2)
colnames(dane3)<-nazwy2
############koniec czyszczenia wchodzi model do zrobienia############
require(caret)
rm(dane_treningowe,dane2)
gc()
sapply(dane3,class)
modFit <- train(classe ~ .,method="rpart",data=dane3)

########wykres
library(rattle)
fancyRpartPlot(modFit$finalModel)

###############ocena modelu######################
pred<-predict(modFit,newdata=dane3)
confusionMatrix(pred, dane3$classe)
library(rpart)
#############3
probs <- treeresponse(modFit, newdata=dane3)
pred <- do.call(rbind, pred)
summary(pred)
#########krzywa ROC#########
table(pred,dane3$classe)
gc()


#######las losowy--2 model########## 
#######najważniejsze zmienne według drzewa
gc()
dane4<-data.frame(classe=dane3$classe,roll_belt=dane3$roll_belt,pitch_forearm=dane3$pitch_forearm,
                  cvtd_timestamp=dane3$cvtd_timestamp,magnet_dumbbell_z=dane3$magnet_dumbbell_z,
                  raw_timestamp_part_1=dane3$raw_timestamp_part_1)
rm(dane3)
gc()
dane4$rzad<-1:19622

dane5<-dane4[sample(dane4$rzad,1000),]
dane5<-dane5[,-7]
gc()
?sample
gc()
modFit2 <- train(classe~.,data=dane5,method="rf",prox=TRUE)
modFit2$results
pred <- predict(modFit2,dane4); 
pred2 <- predict(modFit2,dane_testowe)


table(pred,dane4)
table(pred2,dane_testowe$problem_id)








############najpierw drzewo###########
#modFit <- train(classe ~ .,method="rpart",data=dane_treningowe)

############teraz inne metody ze stronki###############
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
dane4<-dane4[,-7]
gbmFit1 <- train(classe ~ ., data = dane4,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

gbmFit1 <- train(classe ~ ., data = dane4,
                 method = "gbm",
                 #trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = T)


gbmFit1$finalModel





