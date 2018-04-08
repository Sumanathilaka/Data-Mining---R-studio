#read the data from a csv file
kidney <- read.csv("/home/deshan/Desktop/datamining2/clean_kidney.csv")



#################################################################
#                   START OF ASSIGNMENT                         #
#################################################################

### 1A Case 1 ###
library(rpart)
library(rpart.plot)

#set seed to reproduce results
set.seed(123)
#sampling, 2 sets, all rows of dataset, with replacement, 80:20 split
ind <- sample(2, nrow(kidney), replace=TRUE, prob=c(0.8, 0.2))
#80% for training...
trainData <- kidney[ind==1,]
#...and 20% for testing
testData <- kidney[ind==2,]
summary(trainData)

#consider all attributes to predict class attribute
myFormula <- class ~ age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sod+pot+hemo+pcv+wbcc+rbcc+htn+dm+cad+appet+pe+ane
#now build the tree
gini_tree <- rpart(myFormula, data = trainData, method="class", parms = list(split = "gini"), control = rpart.control(maxdepth = 30))
#plotting the tree
plot(gini_tree)
rpart.plot(gini_tree)

# predict using the tree on the test data
testPred <- predict(gini_tree, newdata = testData, type = "class")
# only confustion matrix
table(testPred, testData$class)
# more stats
library("caret")
stats <- confusionMatrix(data = testPred, testData$class)
# all are there except fmeasure
precision <- stats$byClass['Pos Pred Value']    
recall <- stats$byClass['Sensitivity']
fmeasure <- 2 * ((precision * recall) / (precision + recall))

# print values
precision
recall
fmeasure

# ROC curve
pred<-predict(gini_tree,testData,type='prob')

library(ROCR)
library(pROC)
auc<-auc(testData$class,pred[,2])
auc
auc<-auc(testData$class,pred[,1])
auc
plot(roc(testData$class,pred[,2]))



### 1A Case 2 ###
# 10-fold cross validation
library(caret)
library(e1071)

ind=createDataPartition(kidney$class, p=0.8,list=FALSE)
trainData<-kidney[ind,]
testData<-kidney[-ind,]
trctrl <- trainControl(method = "cv", number = 10)
set.seed(3333)
gini_tree <- train(myFormula, data = trainData, method = "rpart",
                   parms = list(split = "gini"),
                   trControl=trctrl,
                   tuneLength = 10)
plot(gini_tree)


# confusion matrix
testPred <- predict(gini_tree, newdata = testData)
table(testPred, testData$class)
# more stats
stats <- confusionMatrix(data = testPred, testData$class)
# all are there except fmeasure
precision <- stats$byClass['Pos Pred Value']    
recall <- stats$byClass['Sensitivity']

fmeasure <- 2 * ((precision * recall) / (precision + recall))

# print values
precision
recall
fmeasure


# ROC curve
pred<-predict(gini_tree,testData,type='prob')

library(ROCR)
library(pROC)
auc<-auc(testData$class,pred[,2])
auc


auc<-auc(testData$class,pred[,1])
auc
plot(roc(testData$class,pred[,2]))



### 1B Case 1 ###
library(rpart)
library(rpart.plot)

#set seed to reproduce results
set.seed(456)
#sampling, 2 sets, all rows of dataset, with replacement, 80:20 split
ind <- sample(2, nrow(kidney), replace=TRUE, prob=c(0.8, 0.2))
#80% for training...
trainData <- kidney[ind==1,]
#...and 20% for testing
testData <- kidney[ind==2,]
summary(trainData)

#consider all attributes to predict class attribute
myFormula <- class ~ age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sod+pot+hemo+pcv+wbcc+rbcc+htn+dm+cad+appet+pe+ane
#now build the tree
entropy_tree <- rpart(myFormula, data = trainData, method="class", parms = list(split = "information"), control = rpart.control(maxdepth = 30))
#plotting the tree
plot(entropy_tree)
rpart.plot(entropy_tree)

# predict using the tree on the test data
testPred <- predict(entropy_tree, newdata = testData, type = "class")
# only confustion matrix
table(testPred, testData$class)
# more stats
stats <- confusionMatrix(data = testPred, testData$class)
# all are there except fmeasure
precision <- stats$byClass['Pos Pred Value']    
recall <- stats$byClass['Sensitivity']
fmeasure <- 2 * ((precision * recall) / (precision + recall))

# print values
precision
recall
fmeasure

# ROC curve
pred<-predict(entropy_tree,testData,type='prob')

library(ROCR)
library(pROC)
auc<-auc(testData$class,pred[,2])
auc
plot(roc(testData$class,pred[,2]))



### 1B Case 2 ###
# 10-fold cross validation
library(caret)
library(e1071)

ind=createDataPartition(kidney$class, p=0.8,list=FALSE)
trainData<-kidney[ind,]
testData<-kidney[-ind,]
trctrl <- trainControl(method = "cv", number = 10)
set.seed(4444)
entropy_tree <- train(myFormula, data = trainData, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
plot(entropy_tree)


# confusion matrix
testPred <- predict(entropy_tree, newdata = testData)
table(testPred, testData$class)
# more stats
stats <- confusionMatrix(data = testPred, testData$class)
# all are there except fmeasure
precision <- stats$byClass['Pos Pred Value']    
recall <- stats$byClass['Sensitivity']
fmeasure <- 2 * ((precision * recall) / (precision + recall))

# print values
precision
recall
fmeasure


# ROC curve
pred<-predict(entropy_tree,testData,type='prob')

library(ROCR)
library(pROC)
auc<-auc(testData$class,pred[,2])
auc
plot(roc(testData$class,pred[,2]))


### 1C Case 1 ###
library(rpart)
library(rpart.plot)

#set seed to reproduce results
set.seed(456)
#sampling, 2 sets, all rows of dataset, with replacement, 80:20 split
ind <- sample(2, nrow(kidney), replace=TRUE, prob=c(0.8, 0.2))
#80% for training...
trainData <- kidney[ind==1,]
#...and 20% for testing
testData <- kidney[ind==2,]
summary(trainData)

#consider all attributes to predict class attribute
myFormula <- class ~ age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sod+pot+hemo+pcv+wbcc+rbcc+htn+dm+cad+appet+pe+ane
# building the model
nb <-naiveBayes(class ~., data = trainData)
nb
pred <- predict(nb,testData)


# confusion matrix
testPred <- predict(nb, newdata = testData)
table(testPred, testData$class)
# more stats
stats <- confusionMatrix(data = testPred, testData$class)
# all are there except fmeasure
precision <- stats$byClass['Pos Pred Value']    
recall <- stats$byClass['Sensitivity']
fmeasure <- 2 * ((precision * recall) / (precision + recall))

# print values
precision
recall
fmeasure

# ROC curve
pred<-predict(nb,testData,type="raw")

library(ROCR)
library(pROC)
auc<-auc(testData$class,pred[,2])
auc
plot(roc(testData$class,pred[,2]))


### 1C Case 2 ###
library(caret)
library(e1071)

ind=createDataPartition(kidney$class, p=0.8,list=FALSE)
trainData<-kidney[ind,]
testData<-kidney[-ind,]
trctrl <- trainControl(method = "cv", number = 10)
nb_c <- train(class~ age+hemo, data = trainData,trControl=trctrl,  method = "nb")
nb_c


# confusion matrix
testPred <- predict(nb_c, newdata = testData)
table(testPred, testData$class)
# more stats
stats <- confusionMatrix(data = testPred, testData$class)
# all are there except fmeasure
precision <- stats$byClass['Pos Pred Value']    
recall <- stats$byClass['Sensitivity']
fmeasure <- 2 * ((precision * recall) / (precision + recall))

# print values
precision
recall
fmeasure


# ROC curve
library(ROCR)
library(pROC)
trctrl <- trainControl(method = "cv", number = 10)
model<-train(class~ age+hemo , data=trainData, trControl=trctrl,method="nb")
pred <- predict(model, testData)



### 1D Case 1 With ###

ind <- sample(2, nrow(kidney), replace = TRUE, prob=c(0.8, 0.2))
trainset <- kidney[ind == 1,]
testset <- kidney[ind == 2,]
trainset$ckd= trainset$class == "ckd"
trainset$notckd= trainset$class == "notckd"
library(neuralnet)
network = neuralnet(ckd ~ age+hemo+pcv+wbcc+rbcc, trainset, hidden=c(3,2))
network = neuralnet(notckd ~ age+hemo+pcv+wbcc+rbcc, trainset, hidden=c(3,2))
network = neuralnet(ckd +notckd ~ age+hemo+pcv+wbcc+rbcc, trainset, hidden=c(3,2))
network
network$result.matrix
head(network$generalized.weights[[1]])
plot(network)


                    
                    
### 1D Case 1 Without ### 
ind <- sample(2, nrow(kidney), replace = TRUE, prob=c(0.8, 0.2))
trainset <- kidney[ind == 1,]
testset <- kidney[ind == 2,]
trainset$ckd= trainset$class == "ckd"
trainset$notckd= trainset$class == "notckd"
library(neuralnet)
network = neuralnet(ckd + notckd ~ age+hemo+pcv+wbcc+rbcc, trainset)
network = neuralnet(ckd ~ age+hemo+pcv+wbcc+rbcc, trainset)
network = neuralnet(notckd ~ age+hemo+pcv+wbcc+rbcc, trainset)

network
network$result.matrix
head(network$generalized.weights[[1]])
plot(network)








### 2 ###
kidney_noc <- read.csv("/home/deshan/Desktop/datamining2/clean_kidney_kmean.csv")


### 2A ###


cols <- c(1:5, 10:18)
ans <- kmeans(kidney_noc[, cols], 2)
ans

library(cluster)
D<- daisy(kidney_noc[, cols])
plot(silhouette(ans$cluster, D), col=1:2, border=NA)
plot(silhouette(ans$cluster, D), col=1:1, border=NA)
plot(silhouette(ans$cluster, D), col=2:2, border=NA)



### 2B ###

 
cols <- c(1:5, 10:18)
ans <- kmeans(kidney_noc[, cols], 1)
ans


### 2C ###

cols <- c(1:5, 10:18)
ans <- kmeans(kidney_noc[, cols], 3)
ans


### 2D ###
library (cluster)
library (dbscan)
cols <- c(1:5, 10:18)
ans <- clara (x=kidney_noc[, cols], k=2)
ans
ans[["clusinfo"]]

dbscan(kidney_noc[, cols], eps=11047, minPts = 121 )

ans[["silinfo"]]

library(cluster)
D<- daisy(kidney_noc[, cols])
plot(silhouette(ans$cluster, D), col=1:1, border=NA)
plot(silhouette(ans$cluster, D), col=2:2, border=NA)

