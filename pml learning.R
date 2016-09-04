
# input data
library(data.table)

traintable<- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = T, sep = ',',na.strings = c("NA","","#DIV/0!"))
traintable<-as.data.frame.matrix(traintable) 



# Validation set

valtable<- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header = T, sep = ',',na.strings = c("NA","","#DIV/0!"))
valtable<-as.data.frame.matrix(valtable) 


# Only keep vars within the 

traintemp<-traintable[-(1:7)]

colnames(traintemp)

# Throw away Near-zero variables 
library(caret)


# Exclude variabels with over 80% NA variables 

traindata<-traintemp[,colSums(is.na(traintemp)) <= nrow(traintemp)*0.9]

colnames(traindata)
dim(traindata)

traindata[, 1:52] <- sapply(traindata[, 1:52], as.numeric)
traindata$classe<- as.factor(traindata$classe)




# remove missing variabels with NA rate over 50% 

set.seed(100)

intrain<-createDataPartition(y=traindata$classe,p=0.6,list=FALSE)

trainsample<-traindata[intrain,]
testsample<-traindata[-intrain,]

head(trainsample)



#randome forest 


rfmodel <- randomForest(classe~., trainsample)

pred_rf<- predict(rfmodel,testsample,type='class')

confusionMatrix(pred_rf, testsample$classe)

# Decision tree

library(rpart)
library(rattle)
set.seed(100)
dtmodel <- rpart(classe ~ ., data=trainsample, method="class")

pred_dt<-predict(dtmodel,testsample,type='class')
confusionMatrix(pred_dt, testsample$classe)



# 
install.packages('e1071', dependencies=TRUE)
library('e1071')

svmmodel<-svm(classe~.,data=trainsample,method='class')



library('gbm')

gbmmodel<-gbm(classe~.,data=trainsample,distribution='multinomial')

help(gbm)

best.iter <- gbm.perf(gbmmodel,method="OOB")
print(best.iter)

pred_gbm<- predict(gbmmodel, testsample,best.iter,type='response')

head(pred_gbm)

pred_gbm <- as.factor(colnames(pred_gbm)[max.col(pred_gbm)]) ##!


confusionMatrix(pred_gbm, testsample$classe)


write.csv(pred_gbm,


p.pred_gbm <- apply(pred_gbm, 1, which.max)


pred_gbm[1:6,,]

predBST[1:6,,]

confusionMatrix(pred_gbm, testsample$classe)



