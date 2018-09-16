
#===================================Decision Trees================================================

#------------------------------Letter Recognition-----------------------------------------------------

#Creating Environment for decision trees

list.of.packages <- c("caret", "e1071","ggplot2","rpart", "rpart.plot","pROC","ROCR","randomForest","caTools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")


library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)

#Importing dataset
path<- "C:/Users/Stark/Documents/ivy files/R/Case studies/dECISION TREE CASE STUDY/IVY_Decision Trees_Case Study"
setwd(path)
letters<- read.csv("letters_ABPR.csv")
str(letters)
summary(letters)


# Checking null values

colSums(is.na(letters))

#Dividing into train and test set

set.seed(1000)

spl <- sample.split(letters$letter, SplitRatio = 0.7)

train<- subset(letters, spl==TRUE)
str(train)
summary(train)

test<- subset(letters,spl==FALSE)
str(test)
summary(test)

#Creating Decision Tree
colnames(train)
CART1<- rpart(letter~. , data = train, method = "class" )
prp(CART1)

CART1


#Predicting on train set

predictCART1.train<- predict(CART1, newdata = train, type = 'class')
predictCART1.train

confusionMatrix(data = predictCART1.train, reference = train$letter)


#Predicting on test data
predictCART1 <- predict(CART1, newdata = test, type = 'class')  
predictCART1

table(actual= test$letter,predicted =  predictCART1)

confusionMatrix(data = predictCART1, reference = test$letter)


#Creating random forest

lettersForest<- randomForest(letter~. , data = train)
plot(lettersForest)

#predicting on train set

predictForest.train<- predict(lettersForest, newdata = train, type = 'class')
predictForest.train


confusionMatrix(data = predictForest.train, reference = train$letter)

###########################100% accuracy!!!###########################################

#Predicting on test set

predictForest.test<- predict(lettersForest, newdata = test, type = 'class')
predictForest.test



confusionMatrix(data = predictForest.test, reference = test$letter)

###################### 98.2% ACCURACY!!! ######################################################



















