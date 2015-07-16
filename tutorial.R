rm(list = ls())
library(caret)
library(kernlab)

############
##Data Splitting functions
############
data(spam)

##cross-validation method
folds<-createFolds(y=spam$type,k=10,list=TRUE, returnTrain = TRUE)#folds for training set 
sapply(folds,length)#check lens for each fold
folds[[2]][1:10]

##resampling method
folds.res<-createResample(y=spam$type,times=10,list=TRUE)#it's bootstrap sampling
folds[[2]][1:10]

##time slices for timeseries
tme<-1:1000
folds.tme<-createTimeSlices(y=tme,initialWindow = 20,horizon = 10)
names(folds.tme)
folds.tme$train[[1]]
folds.tme$test[[1]]

###########################################################
##Spam example with basic data partition
inTrain<-createDataPartition(y=spam$type,p=0.6,list=FALSE)
trainSet<-spam[inTrain,]
testSet<-spam[-inTrain,]
dim(trainSet)
dim(testSet)

#using train function in training model
args(train.default)#show default setting for train function, eg random forest default
args(trainControl)

set.seed(1)#set seed for consistant random samples
Train_mod1<-train(type~.,data=trainSet,method="glm")
Train_mod1$finalModel

pred<-predict(Train_mod1,newdata=testSet)
confusionMatrix(pred,testSet$type)


###########################################################
##Visilization example: plotting vars:Wage data
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

#categoricalize numercial veriables without creating a new variable
wage.c<-cut2(training$wage,g=3)#g is the number of quantile groups
summary(wage.c)
plot.wa<-qplot(wage.c,age,data=training,fill=wage.c,geom=c('boxplot'))
plot.wa2<-qplot(wage.c,age,data=training,fill=wage.c,geom=c('boxplot','jitter'))
plot.wa;plot.wa2

t1<-table(wage.c,training$jobclass)
prop.table(t1,margin = 2)# 2 for column

qplot(wage,colour=education,data=training,geom='density')



###########################################################
##preprocessing part-one : standarizing vars 
rm(list = ls())
library(caret);library(kernlab);data(spam)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
trainSet<-spam[inTrain,]
testSet<-spam[-inTrain,]
#highly skewed data 
hist(trainSet$capitalAve,main='skewness',xlab = 'ave capital run length' )

########preprocess method 1: eg. capitalAve
#standarized to a 'new variable'
trainSet$capaveSTD<-(trainSet$capitalAve-mean(trainSet$capitalAve))/sd(trainSet$capitalAve)
mean(trainSet$capaveSTD);mean(trainSet$capitalAve)
#standarized the testSet
testSet$capaveSTD<-(testSet$capitalAve-mean(trainSet$capitalAve))/sd(trainSet$capitalAve)
mean(testSet$capaveSTD);mean(testSet$capitalAve)
########preprocess method 2:#using pre-Process function: preProcess
?preProcess
preSTD<-preProcess(trainSet[,-58],method=c("center","scale"))# create a standard
trainSet$capaveSTD<-predict(preSTD,trainSet[,-58])$capitalAve# 
testSet$capaveSTD<-predict(preSTD,testSet[,-58])$capitalAve#applied to testSet using training set standard
mean(testSet$capaveSTD);mean(trainSet$capaveSTD);
sd(testSet$capaveSTD);sd(trainSet$capaveSTD)
####### preprocess method 3: standarized all variables at once by turning preProcess parameter in model function
set.seed(2)
trainMod2<-train(type~.,data=trainSet,preProcess=c("center","scale"),method="glm")

###########################################################
##preprocessing part-two: Box-Cox transforms
preSTD<-preProcess(trainSet[,-58],method=c("BoxCox"))
trainSet$capaveSTD<-predict(preSTD,trainSet[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainSet$capaveSTD);qqnorm(trainSet$capaveSTD)
#comment: doesn't take care of values repeated

###########################################################
##preprocessing part-three: imputing data, assuming there is missing data 
set.seed(3)
#create a new var with random missing data, NA, for practicing purpose
trainSet$capAVE<-trainSet$capitalAve
selectNA<-rbinom(dim(trainSet)[1],size=1,prob = 0.05)==1# trans to true or false values
trainSet$capAVE[selectNA]<-NA# filled in NA

#impute and 'standardize'
#preprocessing with knnImpute silently removes any columns containing NA values
preInp.STD<-preProcess(trainSet[,-58],method = 'knnImpute')#using k-nearest neighbor imput, then standarized
capAVE.P<-predict(preInp.STD,trainSet[,-58])$capAVE

#Standarized true values to compare with previous results
capAVE.T<-trainSet$capitalAve
capAVE.T<-(capAVE.T-mean(capAVE.T))/sd(capAVE.T)
plot((capAVE.T-capAVE.P))
#comment: most predict value closed to true values(majority of the difference/error close to zero)
#or check
quantile(capAVE.P-capAVE.T)
quantile((capAVE.P-capAVE.T)[selectNA])#error for imputed only values
quantile(capAVE.P-capAVE.T)
par(mfrow=c(1,2))
plot((capAVE.T-capAVE.P)[!selectNA],main='non-impute value');plot((capAVE.T-capAVE.P)[selectNA],main='imputed value')





