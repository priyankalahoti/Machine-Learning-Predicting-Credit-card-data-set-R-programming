##xgboost 
library(xgboost)
library(dplyr)
library(magrittr)
library(Matrix)


#data prep
creditcard<-read.csv("D:/creditcard.csv")
head(creditcard,10)
summary(creditcard$Class)
str(creditcard)
summary(creditcard)
length(which(creditcard$Class==1))


#data partitioning
set.seed(100)
index <- sample(1:nrow(creditcard), nrow(creditcard)*0.7)
training<- creditcard[index,]
validation<- creditcard[-index,]


#one hot encoding
train_label<-training[,"Class"]
xgb.data.train <- xgb.DMatrix(as.matrix(training[, colnames(training) != "Class"]), label = train_label)
test_label<-validation[,"Class"]
xgb.data.test <- xgb.DMatrix(as.matrix(validation[, colnames(validation) != "Class"]), label = test_label)


#parameters
nc<-length(unique(train_label))
xgb_params<-list("objective"="binary:logistic",
                 "eval_metric"="auc",
                 "numclass"=nc)
watchlist<-list(train= xgb.data.train, test= xgb.data.test)


##eXtreme gradient boosting model
bst_model<-xgb.train(params=xgb_params,
                     data=xgb.data.train,
                     nrounds=500,
                     watchlist=watchlist)


##training and test error plot
e<-data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_auc ,col='blue')


##feature importance
xgb.feature.imp = xgb.importance(model = bst_model)
plot(xgb.feature.imp$Feature,xgb.feature.imp$Gain)


##validation of model
xgb.test.hist = predict(bst_model
                        , newdata = as.matrix(validation[, colnames(validation) != "Class"])
                        , ntreelimit = bst_model$bestInd)
library(pROC)
auc.xgb.hist = roc(validation$Class, xgb.test.hist, plot = TRUE, col = "blue")
print(auc.xgb.hist)
