library(dplyr)
library(irr)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)

#data prep
creditcard<-read.csv("D:/creditcard.csv")
head(creditcard,6)
summary(creditcard)
str(creditcard)
summary(creditcard)
length(which(creditcard$Class==1))

#data partitioning
set.seed(100)
index <- sample(1:nrow(creditcard), nrow(creditcard)*0.7)
training<- creditcard[index,]
validation<- creditcard[-index,]

# Decision Tree
mod<-rpart(Class~.,data=training, method="class")
predicted <- predict(mod ,validation, type="prob")
area_under_curve<-auc(validation$Class, predicted[,2])
area_under_curve

#Random Forest
n <- names(training)
rf.form <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))
trainset.rf <- randomForest(rf.form, training ,ntree=100,importance=T)
predicted0<- predict(trainset.rf ,validation, type="prob")
area_under_curve0<-auc(validation$Class, predicted0[,2])
area_under_curve0


# Synthetically Generating data using rose function in ROSE package for better estimate of original data
training$Class<-as.factor(training$Class)
data.rose <- ROSE(Class ~ ., data = training, seed = 100)$data
table(data.rose$Class)

# Applying Decision Tree model on This synthetically generated data
mod1<-rpart(Class~.,data=data.rose, method="class")
predicted1 <- predict(mod1 ,validation, type="prob")
area_under_curve1<-auc(validation$Class, predicted1[,2])
area_under_curve1


# Applying Random Forest model on This synthetically generated data
n <- names(data.rose)
rf.form1 <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))
trainset.rf1 <- randomForest(rf.form1,data.rose,ntree=100,importance=T)
predicted2<- predict(trainset.rf1 ,validation, type="prob")
area_under_curve2<-auc(validation$Class, predicted2[,2])
area_under_curve2

#sampling
library(ROSE)
training$Class<-as.factor(training$Class)
data_balanced_under <- ovun.sample(Class ~ ., data = training, method = "both" ,N=10000 , seed = 300)$data
table(data_balanced_under$Class)

# Applying Decision Tree model on sampled data
mod2<-rpart(Class~.,data=data_balanced_under, method="class")
predicted3 <- predict(mod2 ,validation, type="prob")
area_under_curve3<-auc(validation$Class, predicted3[,2])
area_under_curve3


# Applying Random Forest on This sampled data
n <- names(data_balanced_under)
rf.form2<- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))
trainset.rf2 <- randomForest(rf.form2, data_balanced_under,ntree=500,importance=T)
predicted4 <- predict(trainset.rf2 ,validation, type="prob")
area_under_curve4<-auc(validation$Class, predicted4[,2])
area_under_curve4

