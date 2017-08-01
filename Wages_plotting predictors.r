# Plotting the Predictors

# Wages. 


library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)





# Get training/test sets

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)


#Feature plot (caret package)

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

#Qplot (ggplot2 package)

qplot(age,wage,data=training)


#Qplot with color (ggplot2 package)

qplot(age,wage,colour=jobclass,data=training)


#Add regression smoothers (ggplot2 package)

qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)

#cut2, making factors (Hmisc package)

cutWage <- cut2(training$wage,g=3)
table(cutWage)

#Boxplots with cut2

p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p1


#Boxplots with points overlayed

p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)


#Tables

t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)


#Density plots

qplot(wage,colour=education,data=training,geom="density")


# Made with and for training of Data on :

#plots only in the training set
#useing the test set for exploration!

#Imbalance in outcomes/predictors
#Outliers
#Groups of points not explained by a predictor
#Skewed variables

## in dept ploting provided by:
#ggplot2 tutorial
#caret visualizations




