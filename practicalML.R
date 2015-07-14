rm(list = ls())
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library(kernlab)

############
##data sliciing
############
data(spam)

##cross-validation method
folds<-createFolds(y=spam$type,k=10,list=TRUE, returnTrain = TRUE)#folds for training set 
sapply(folds,length)#check lens for each fold
folds[[2]][1:10]

##resampling method
folds.res<-createResample(y=spam$type,times=10,list=TRUE)
folds[[2]][1:10]

##time slices for timeseries
tme<-1:1000
folds.tme<-createTimeSlices(y=tme,initialWindow = 20,horizon = 10)
names(folds.tme)
folds.tme$train[[1]]
folds.tme$test[[1]]

#########
##Spam example
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
trainSet<-spam[inTrain,]
testSet<-spam[-inTrain,]
dim(trainSet)
dim(testSet)

#using train function in training model
args(train.default)#show default setting for train function
args(trainControl)

set.seed(1)#set seed for consistant random samples
Train_mod1<-train(type~.,data=trainSet,method="glm")
Train_mod1$finalModel

pred<-predict(Train_mod1,newdata=testSet)

confusionMatrix(pred,testSet$type)



#########
##plotting vars example:Wage data
rm(list = ls())
library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
data(Wage);str(Wage)

#split by index
indexTrain<-createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training<-Wage[indexTrain,]
testing<-Wage[-indexTrain,]

featurePlot(x=training[,c('age','year','logwage')],y=training$wage,plot='pairs')
#using ggplot for vivi plot
qq<-qplot(age,logwage,colour=education,data=training)
#geometric smoothing
qq+ geom_smooth(method='lm',formula=y~x)

#categoricalize numercial veriables: gen new var
wage.c<-cut2(training$wage,g=3)






