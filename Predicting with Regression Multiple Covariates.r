#Example: Wage data

library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)


#Get training/test sets

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)



#Feature plot

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

#Plot age versus wage

qplot(age,wage,data=training)


#Plot age versus wage colour by jobclass

qplot(age,wage,colour=jobclass,data=training)


#Plot age versus wage colour by education

qplot(age,wage,colour=education,data=training)

#Fit a linear model

#$$ ED_i = b_0 + b_1 age + b_2 I(Jobclass_i="Information") + \sum_{k=1}^4 \gamma_k I(education_i= level k) $$
        
 modFit<- train(wage ~ age + jobclass + education,
 method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

#Education levels: 1 = HS Grad, 2 = Some College, 3 = College Grad, 4 = Advanced Degree

#Diagnostics

plot(finMod,1,pch=19,cex=0.5,col="#00000010")


#Color by variables not used in the model

qplot(finMod$fitted,finMod$residuals,colour=race,data=training)


#Plot by index

plot(finMod$residuals,pch=19)

#Predicted versus truth in test set

pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

# A Way to determine if your analysis worked or not. 

#If you want to use all covariates, model fit with all of the variables in the data set.

modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)

#Often useful in combination with other models
