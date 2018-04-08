
deshan <- read.csv("/home/deshan/Desktop/des.csv")

attributes(deshan)

library(rpart)

library(rpart.plot)

set.seed(1234)

ind<-sample(2,nrow(deshan),replace = TRUE,prob = c(0.7,0.3))

deshantrain<-deshan[ind==1,]

deshantest<-deshan[ind==2,]


myformula <- mpg ~cylinders+displacement+horsepower+weight+acceleration+modelyear+origin

deshan_rpart<- rpart(myformula,data =deshantrain,parms = list(split = "gini"),    
                     control=rpart.control(minsplit = 10))

plot(deshan_rpart)

rpart.plot(deshan_rpart)

pred<- predict(deshan_rpart,deshantest,type='prob')

PredictionsWithClass<- predict(deshan_rpart,deshantest,type='prob')

ROC_logit <- roc(deshantrain$mpg, predictions_logit)
library(ROCR)
library(pROC)
pred<-predict(deshan_rpart,deshantest,type='prob')
Predict <- predict(deshan_rpart, newdata = deshantrain)
pred<-prediction(predict ,deshantrain$mpg)

library(e1071)
ind <- sample(2, nrow(deshan), replace=TRUE, prob=c(0.8, 0.2))
train <- deshan[ind==1,]
 test <- deshan[ind==2,]
e1071model <-naiveBayes(mpg ~., data = train)

e1071modelprediction <- predict(e1071model,test)
e1071model
library(ROCR)
confusionMatrix(data = e1071modelprediction, test$mpg)
result <- confusionMatrix(data = e1071modelprediction, test$mpg)
 precision <- result$byClass['Pos Pred Value']

   recall <- result$byClass['Sensitivity']

   f_measure <- 2 * ((precision * recall) / (precision + recall))
   
   
   
   ind <- sample(2, nrow(deshan), replace=TRUE, prob=c(0.8, 0.2))
   
      train <- deshan[ind==1,]
    test <- deshan[ind==2,]
    e1071modelprediction <- predict(e1071model,test,type="raw")
    auc<-auc(test$mpg,e1071modelprediction[,2])
    
    
    library(caTools)
    set.seed(88)
split <- sample.split(deshan$mpg,SplitRatio = 0.75)
train <-subset(deshan,split ==TRUE)
test <- subset(deshan,split ==FALSE)

model <-glm(myformula,data=train ,family = binomial)




















#################################################

