rm(list=ls())

##libraries
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)

data<-read_excel("insurance.xlsx")
head(data)
maxCost<-max(data$charges)
data$charges = cut(data$charges, breaks=c(0, 10680, 13968, 18180, maxCost), labels = c("Bronze", "Silver", "Gold", "Platinium"))
data$charges=as.factor(data$charges)
data$region=as.factor(data$region)
data$charges

set.seed(1)
myIndex <- createDataPartition(data$charges, p=0.7, list=FALSE)
trainSet <- data[myIndex,]
validationSet <- data[-myIndex,]

set.seed(1)
default_tree <- rpart(charges ~., data = trainSet, method = "class")
summary(default_tree)

prp(default_tree, type = 1, extra = 1, under = TRUE)

set.seed(1)
full_tree <- rpart(charges ~ ., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)

printcp(full_tree)

pruned_tree <- prune(full_tree, cp = 7.5219e-02)
summary(pruned_tree)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

predicted_class<- predict(pruned_tree, validationSet, type="class")
cm<-confusionMatrix(predicted_class, validationSet$charges, positive="1")
cm


predicted_prob<- predict(pruned_tree, validationSet, type="prob")
predicted_prob

data3<-read_excel("insurance.xlsx")
head(data3)
predicted_value_score <- predict(pruned_tree, data3)
predicted_value_score

validationSet$charges <- as.numeric(validationSet$charges)
gains_table <- gains(validationSet$charges, predicted_prob[,2])
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

roc_object <- roc(validationSet$charges, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)
