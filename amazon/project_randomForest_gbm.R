install.packages("gbm")
install.packages("randomForest")
install.packages("verification")
library(randomForest)
library(verification)
library(gbm)

test = read.csv("C:/Users/pshah/Documents/school/project/test.csv", header = TRUE)
train.data = read.csv("C:/Users/pshah/Documents/school/project/train.csv", header = TRUE)
attach(train.data)

train.SMOTE = SMOTE(ACTION ~ ., data = train.data)

rf.model = randomForest(formula = as.factor(ACTION) ~ .,
              data = crs$dataset[crs$sample, c(crs$input, crs$target)],
              ntree = 500, mtry = 3, importance = TRUE, replace = FALSE, na.action = na.omit)

rf.model.strat = randomForest(formula = as.factor(ACTION) ~ .,
              data = train.data, strata = as.factor(ACTION),
		  sampsize=c(1700,8000),
              ntree = 700, importance = TRUE, 
		  replace = FALSE, na.action = na.omit)

rf.model.strat = randomForest(formula = as.factor(ACTION) ~ .,
              data = train.data, strata = as.factor(ACTION),
              ntree = 700, mtry = 4, importance = TRUE, 
		  replace = FALSE, na.action = na.omit)

summary(rf.model.strat)
varImpPlot(rf.model.strat)
##test it
test.rf.predicted = predict(rf.model.strat, 
	newdata=test[,-1], type="prob")
train.rf.predicted = predict(rf.model.strat, 
	newdata=train.data[,-1], type="prob")

test.rf.data = cbind(test[,1], test.rf.predicted[,2])
roc.plot(train.data[,1], train.rf.predicted[,2])
roc.area(train.data[,1], train.rf.predicted[,2])
colnames(test.rf.data) = c("id", "ACTION")
write.csv(test.rf.data, "C:/Users/pshah/Documents/school/project/amazon_predicted_rf.csv")
##AUC = 0.870

##AUC on train 0.9981177
      OOB estimate of  error rate: 4.67%
Confusion matrix:
    0     1 class.error
0 739  1158  0.61043753
1 372 30500  0.01204975


#########################################GBM#############################3

train.gbm = gbm(formula = as.factor(ACTION) ~ .,
              data = train.data, distribution = "bernoulli",
		  class.stratify.cv = TRUE,
		  n.trees = 500, interaction.depth = 2,cv.folds=20, keep.data=TRUE,
		  n.cores = 1)

best.iter <- gbm.perf(train.gbm,method="cv")
##test it
train.gbm.predicted = predict(train.gbm, 
	newdata=train.data[,-1], type="response", n.trees = best.iter)

roc.plot(train.data[,1], train.gbm.predicted)
roc.area(train.data[,1], train.gbm.predicted)

test.gbm.predicted = predict(train.gbm, 
	newdata=test[,-1], type="response")

test.gbm.data = cbind(test[,1], test.gbm.predicted)
colnames(test.gbm.data) = c("id", "ACTION")
write.csv(test.gbm.data, "C:/Users/pshah/Documents/school/project/amazon_predicted_rf.csv")
##AUC = 0.846

##AUC = 0.864
