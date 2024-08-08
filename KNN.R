rm(list=ls())

library(caret)
library(gains)
library(pROC)

library(readxl)
data<-read_excel("insurance.xlsx")

maxCost<-max(data$charges)

data$charges = cut(data$charges, breaks=c(0, 13000, maxCost), labels=c("0", "1"))
data$charges
data$charges=as.factor(data$charges)


data$age=scale(data$age)
data$bmi=scale(data$bmi)
data$region=as.factor(data$region)

set.seed(1)
myIndex<- createDataPartition(data$charges, p=0.8, list=FALSE)
trainSet <- data[myIndex,]
validationSet <- data[-myIndex,]

myCtrl <- trainControl(method="cv", number=100)
myGrid <- expand.grid(.k=c(1:100))

set.seed(1)
KNN_fit <- train(charges ~ ., data=trainSet, method = "knn", trControl=myCtrl, tuneGrid = myGrid)
KNN_fit

KNN_Class <- predict(KNN_fit, newdata = validationSet)
confusionMatrix(KNN_Class, validationSet$charges, positive = '1')

KNN_Class_prob <- predict(KNN_fit, newdata = validationSet, type='prob')
KNN_Class_prob


confusionMatrix(as.factor(ifelse(KNN_Class_prob[,2]>0.25, '1', '0')), 
                validationSet$charges, positive = '1')

validationSet$charges <- as.numeric(validationSet$charges)
gains_table <- gains(validationSet$charges, KNN_Class_prob[,2])
gains_table

plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$charges))~c(0, gains_table$cume.obs), 
     xlab = "# of cases", 
     ylab = "Cumulative", 
     main="Cumulative Lift Chart", 
     type="l")
lines(c(0, sum(validationSet$charges))~c(0, dim(validationSet)[1]), 
      col="red", 
      lty=2)

barplot(gains_table$mean.resp/mean(validationSet$charges), 
        names.arg=gains_table$depth, 
        xlab="Percentile", 
        ylab="Lift", 
        ylim=c(0,3), 
        main="Decile-Wise Lift Chart")

roc_object <- roc(validationSet$charges, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)

ogData<-read_excel("insurance.xlsx")

View(ogData)
##scale
ogData1<- scale(ogData)
##run the prediction
KNN_Score <- predict(KNN_fit, newdata=ogData)

ScoreData <- data.frame(ogData, KNN_Score)
View(ScoreData)