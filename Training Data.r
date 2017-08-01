#SPAM Example

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,data=training, method="glm")


args(train.default)

# Train Control
args(trainControl)


#trainControl resampling

#method
#boot = bootstrapping
#boot632 = bootstrapping with adjustment
#cv = cross validation
#repeatedcv = repeated cross validation
#LOOCV = leave one out cross validation
#number
#For boot/cross validation
#Number of subsamples to take
#repeats
#Number of times to repeate subsampling
#If big this can slow things down



#seed example


set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2



#seed example


set.seed(1235)
modelFit3 <- train(type ~.,data=training, method="glm")
modelFit3

















