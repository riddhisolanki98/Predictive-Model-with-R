#loading the necessary libraries
library(gains)
library(caret)
library(ROCR)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)


#reading excel
airlines.df <- read_excel("test.xlsx")
View(airlines.df)


#data cleansing
airlines1.df <-  airlines.df[ , -c(1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
View(airlines1.df)


#converting to factors
sapply(airlines1.df,class)

cols<-c('Gender','Customer Type','Type of Travel','Class','satisfaction')
airlines1.df[cols]<-lapply(airlines1.df[cols], factor)

sapply(airlines1.df,class)

new_col_names<-c('Gender','Customer_Type','Age','Type_of_Travel','Class','Flight_Distance',
                 'satisfaction')
colnames(airlines1.df)<-new_col_names
View(airlines1.df)

boxplot(airlines1.df$satisfaction ~ airlines1.df$Type_of_Travel)

#partitioning the data
numberOfRows <- nrow(airlines1.df)
set.seed(1)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.df <- airlines1.df[train.index, ]
valid.df <- airlines1.df[-train.index, ]
View(train.df)
View(valid.df)

ggplot(airlines1.df, aes(y = Age, x = Flight_Distance)) +
  geom_point(alpha = 0.5)

#classification tree

.ct <- rpart(satisfaction ~., data = train.df, method = "class", cp = 0, maxdepth = 4, minsplit = 20)
printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)


#random forest
rf <- randomForest(satisfaction ~ ., data = train.df, 
                   ntree = 5000, mtry = 11, nodesize = 1, importance = TRUE, sampsize = 2000) 

#plot the variables by order of importance
varImpPlot(rf, type = 1)

#create a confusion matrix

rf.pred <- predict(rf, valid.df)
rf.pred
confusionMatrix(rf.pred, valid.df$satisfaction)

#==========================================================================

#preparing the data for logistic regression
airlines1.df$satisfied <- ifelse(airlines1.df$satisfaction == "satisfied", 1,0)
View(airlines1.df)
airlines2.df<-airlines1.df[-7]
View(airlines2.df)


set.seed(19)
numberOfRows2 <- nrow(airlines2.df)
train2.index <- sample(numberOfRows2, numberOfRows2*0.7)
train2.df <- airlines2.df[train2.index, ]
validation2.df <- airlines1.df[-train2.index, ]

#Logistic regression
airlines1.df.glm <- glm(satisfied ~., data = train2.df, family = "binomial")
summary(airlines1.df.glm)
options(scipen=999)

confusionMatrix(table(predict(airlines1.df.glm, newdata = validation2.df, 
                              type="response") >= 0.1, validation2.df$satisfied == 1))