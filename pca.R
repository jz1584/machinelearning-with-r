#############################
#############################
#ex
library(caret);library(AppliedPredictiveModeling)
set.seed(3433);data(AlzheimerDisease)
adData=data.frame(diagnosis,predictors)
inTrain=createDataPartition(adData$diagnosis,p=3/4,list=FALSE)
training=adData[inTrain,]
testing=adData[-inTrain,]

il<-training[,58:69]
Pca_obj<-preProcess(il,method = 'pca',thresh = .8)
Pca_obj

#non-PCA model
train2<-training[,c(1,58:69)]
test2<-testing[,c(1,58:69)]
mod<-train(diagnosis~.,data=train2,method="glm")#create a non-PCA model/object using training data
pred1<-predict(mod,newdata=test2)#predict using testing data without target
confusionMatrix(pred1,test2$diagnosis)

#PCA model
Pca<-preProcess(train2[,-1],method = 'pca',thresh = 0.8)#create a pca object 
PcaTrain.data<-predict(Pca,train2[,-1])#generate pca training data from original data 
mod.pca<-train(train2$diagnosis~.,data=PcaTrain.data,method="glm")#training model using pca data

PcTest.data<-predict(Pca,test2[,-1])# preprocess testing data using same object from training 
pred2<-predict(mod.pca,PcTest.data)#fit model using test data
confusionMatrix(pred2,test2$diagnosis)
