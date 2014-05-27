install.packages("caret")
install.packages("e1071")
install.packages("party")
install.packages("doParallel")
install.packages("glmnet")
install.packages("ipred")
install.packages("C50")
install.packages("verification")
install.packages("ada")
library(ada)
library(svmpath)
library(verification)
library(C50)
library(ipred)
library(doParallel)
library(glmnet)
library(caret)
library(e1071)
library(party)


train = read.csv("C:/Users/parag/Documents/data_mining/project/train.csv", header = TRUE)
test = read.csv("C:/Users/parag/Documents/data_mining/project/test.csv", header = TRUE)
attach(train)
columnNames = colnames(train)
Y = as.factor(ACTION)

x.factors = apply(train[,-1], 2, as.factor)
cl = makeCluster(detectCores())
registerDoParallel(cl)

#####################################KNN############################################################
model.train = train(
    Y~RESOURCE+MGR_ID+ROLE_ROLLUP_1+ROLE_ROLLUP_2+ROLE_DEPTNAME
	+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY, 
    data=train, 
    method='knn',
    tuneGrid=expand.grid(.k=20:30),
    metric='ROC',
    trControl=trainControl(
        method='repeatedcv', 
        number=10, 
        repeats=10,
	  allowParallel=TRUE))

model.train
plot(model.train)

confusionMatrix(model.train)
models = list(model.train)
ncol(train)
test.knn.prob = extractProb(models = models, testX = train[, c(2:9)], testY = train[, 1])
test.knn.prob = predict(model.train, newdata = train[, 2:9], type="prob")
summary(model.train)
model.train$tuneValue

#####################################Random Forest############################################################
set.seed(47)
model.train.rf = train(
    Y~RESOURCE+MGR_ID+ROLE_ROLLUP_1+ROLE_ROLLUP_2+ROLE_DEPTNAME
	+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY, 
    data=train, 
    method='rf',
    importance=TRUE, proximity=TRUE,
    metric='Accuracy',
    trControl=trainControl(
        method='repeatedcv',
	  number=5,
	  repeats=5, 
	  allowParallel=TRUE))

cforestGrid = expand.grid(.mtry = 3)
model.train.cforest = train(
    Y~RESOURCE+MGR_ID+ROLE_ROLLUP_1+ROLE_ROLLUP_2+ROLE_DEPTNAME
	+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY, 
    data=train, 
    method='cforest',
    tuneGrid=cforestGrid,
    metric='Accuracy',
    trControl=trainControl(
        method='oob', 
	  number = 2,
	  repeats = 1,
	  allowParallel=TRUE))


##############################C5.0#######################################

model.train.C5.0 = C5.0(Y~RESOURCE+MGR_ID+ROLE_ROLLUP_1+ROLE_ROLLUP_2
	+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY, 
	data = train, trials = 100, 
	control = C5.0Control(winnow = FALSE, earlyStopping=FALSE))
summary(model.train.C5.0)

model.train.C5.0.imp = C5.0(Y~RESOURCE+MGR_ID+ROLE_ROLLUP_1+ROLE_ROLLUP_2
	+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY, 
	data = train, trials = 100, 
	control = C5.0Control(winnow = TRUE, CF = 0.95, earlyStopping=FALSE))

summary(model.train.C5.0.imp)
##error rate on training


# (a)   (b)    <-classified as
#----  ----
#683  1214    (a): class 0
#33 30839    (b): class 1


C5imp(model.train.C5.0.imp, metric = "splits")

###ROC AUC value
train.C50.predicted = predict(model.train.C5.0.imp, 
	newdata=train[,-1], type="prob")
train.C50.predicted.prob = matrix(nrow = nrow(train.C50.predicted), ncol = 1)
nrow(train.C50.predicted.prob)
for (i in 1:nrow(train.C50.predicted)) {
	prob = 0
	if (train.C50.predicted[i, 1] < train.C50.predicted[i, 2])
		prob = train.C50.predicted[i, 2]
	else
		prob = train.C50.predicted[i, 1]
	train.C50.predicted.prob[i] = prob 
}

auc = roc.area(train[,1], train.C50.predicted.prob)
##score on train = 0.968

roc.plot(train[,1], train.C50.predicted.prob, main = "ROC for C5.0 on training data")

##test it
test.C50.predicted = predict(model.train.C5.0.imp, 
	newdata=test[,-1], type="prob")
test.C50.predicted.prob = matrix(nrow = nrow(test.C50.predicted), ncol = 1)
nrow(test.C50.predicted.prob)
for (i in 1:nrow(test.C50.predicted)) {
	prob = 0
	if (test.C50.predicted[i, 1] < test.C50.predicted[i, 2])
		prob = test.C50.predicted[i, 2]
	else
		prob = test.C50.predicted[i, 1]
	test.C50.predicted.prob[i] = prob 
}

test.C50.data = cbind(test[,1], test.C50.predicted.prob)
roc.plot(train[,1], train.C50.predicted.prob)
colnames(test.C50.data) = c("id", "ACTION")
write.csv(test.C50.data, "C:/Users/parag/Documents/data_mining/project/amazon_predicted_4.csv")

##############################GLMNET##################################################
set.seed(47)
data.matrix = as.matrix(train[,-1])
cv.train.glmnet = cv.glmnet(x=data.matrix, y = ACTION, family="binomial", 
	type.measure="auc", nfolds=20)
summary(cv.train.glmnet)
cv.train.glmnet$lambda.min
title("Binomial Family",line=2.5)
mode.train.glmnet = glmnet(x=data.matrix, y = Y, family = "binomial", alpha=1
	, lambda = 0.0001733454)
summary(mode.train.glmnet)
plot(mode.train.glmnet)
##compute error rate
train.glmnet.predicted = predict(mode.train.glmnet, newx=data.matrix, 
	exact=FALSE, s = mode.train.glmnet$lambda, type="response")

auc = roc.area(train[,1], train.glmnet.predicted)
roc.plot(train[,1], train.glmnet.predicted)
##0.05

##test it
data.test.matrix = as.matrix(test[,-1])
test.glmnet.predicted = predict(mode.train.glmnet, newx=data.test.matrix, 
	exact=TRUE, s = cv.train.glmnet$lambda.min, type="response")
class(test.glmnet.predicted)
dim(test.glmnet.predicted)
test.glmnet.data = cbind(test[,1], test.glmnet.predicted)
colnames(test.glmnet.data) = c("id", "ACTION")
write.csv(test.glmnet.data, "C:/Users/parag/Documents/data_mining/project/amazon_predicted.csv")

####error rate using AUC was 0.52 (might as well toss the coin)

##############################SVM###################################

########using SVM

model.train.svm = svm(Y ~ RESOURCE+MGR_ID+ROLE_ROLLUP_1+
	ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY,
	data=train, type = "C", kernel = "sigmoid", cross=5, probability = TRUE)

model.train.svm.20 = svm(Y ~ RESOURCE+MGR_ID+ROLE_ROLLUP_1+
	ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY,
	data=train, type = "C", kernel = "sigmoid", cross=10, probability = TRUE)


summary(model.train.svm.20)
plot(model.train.svm.20)
##Total Accuracy: 89.47481

###ROC AUC value on training
train.SVM.predicted = predict(model.train.svm.20, 
	newdata=train[,-1], probability = TRUE)
train.SVM.predicted.probMatrix = attr(train.SVM.predicted, "probabilities")
train.SVM.predicted.prob = 
	matrix(nrow = nrow(train.SVM.predicted.probMatrix), ncol = 1)

for (i in 1:nrow(train.SVM.predicted.probMatrix)) {
	prob = 0
	if (train.SVM.predicted.probMatrix[i, 1] < 
	    train.SVM.predicted.probMatrix[i, 2])
		prob = train.SVM.predicted.probMatrix[i, 1]
	else
		prob = train.SVM.predicted.probMatrix[i, 2]
	train.SVM.predicted.prob[i] = prob 
}

auc = roc.area(train[,1], train.SVM.predicted.prob)

###AUC score = 0.514

###########15fold CV
model.train.svm.15 = svm(Y ~ RESOURCE+MGR_ID+ROLE_ROLLUP_1+
	ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY,
	data=train, type = "C", kernel = "sigmoid", C = 3, gamma = 0.2,
	cross=5, probability = TRUE)
summary(model.train.svm.15)

###ROC AUC value on training
train.SVM.predicted = predict(model.train.svm.15, 
	newdata=train[,-1], probability = TRUE)
train.SVM.predicted.probMatrix = attr(train.SVM.predicted, "probabilities")
train.SVM.predicted.prob = 
	matrix(nrow = nrow(train.SVM.predicted.probMatrix), ncol = 1)

for (i in 1:nrow(train.SVM.predicted.probMatrix)) {
	prob = 0
	if (train.SVM.predicted.probMatrix[i, 1] < 
	    train.SVM.predicted.probMatrix[i, 2])
		prob = train.SVM.predicted.probMatrix[i, 1]
	else
		prob = train.SVM.predicted.probMatrix[i, 2]
	train.SVM.predicted.prob[i] = prob 
}

auc = roc.area(train[,1], train.SVM.predicted.prob)

###AUC = 0.47



###find best tuned values for sigmoid and refit the model

mode.train.svm.best = best.tune(method=svm, train.x = train[,-1], 
	train.y = Y, data = train, 
	ranges = list(gamma = (0.5:2), cost = (2:5), type = "C", coef0 = (0:2),
		kernel = c("sigmoid")),tunecontrol = 
		tune.control(nrepeat = 2, cross = 2, sampling = "cross"))

levels(Y)
mode.train.svm.best1 = best.tune(method=svm, train.x = train[,-1], 
	train.y = Y, data = train, 
	ranges = list(gamma = (0.1:1.5), cost = (3:6), type = "C", kernel = c("sigmoid")),
	tunecontrol = tune.control(nrepeat = 1, cross = 1, 
			nboot = 50, sampling = "bootstrap"))


train.SVM.predicted = predict(mode.train.svm.best, 
	newdata=train[,-1], probability = TRUE)
train.SVM.predicted.probMatrix = attr(train.SVM.predicted, "probabilities")
train.SVM.predicted.prob = 
	matrix(nrow = nrow(train.SVM.predicted.probMatrix), ncol = 1)

for (i in 1:nrow(train.SVM.predicted.probMatrix)) {
	prob = 0
	if (train.SVM.predicted.probMatrix[i, 1] < 
	    train.SVM.predicted.probMatrix[i, 2])
		prob = train.SVM.predicted.probMatrix[i, 1]
	else
		prob = train.SVM.predicted.probMatrix[i, 2]
	train.SVM.predicted.prob[i] = prob 
}

auc = roc.area(train[,1], train.SVM.predicted.prob)

##AUC = 0.5
model.train.svm.15.tuned = svm(Y ~ RESOURCE+MGR_ID+ROLE_ROLLUP_1+
	ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY,
	data=train, type = "C", kernel = "sigmoid", C = 4, gamma = 0.5,
	cross=15, probability = TRUE)
summary(model.train.svm.15)



#########################################Logistic Loss with boosting#############################################

model.train.adaLogistic = ada(Y ~ RESOURCE+MGR_ID+ROLE_ROLLUP_1+
	ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY, 
	iter = 200, max.iter=1000, nu = 0.1, loss = "logistic", type = "real", data = train)

model.train.adaLogistic2 = ada(Y ~ RESOURCE+MGR_ID+ROLE_ROLLUP_1+
	ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE+ROLE_FAMILY_DESC+ROLE_FAMILY, 
	iter = 500, max.iter=1000, nu = 0.1, loss = "logistic", type = "gentle", data = train)


summary(model.train.adaLogistic)
model.train.adaLogistic$confusion

###ROC AUC value on training
train.ADALog.predicted = predict(model.train.adaLogistic, 
	newdata=train[,-1], type = "probs")
train.ADALog.predicted.prob = 
	matrix(nrow = nrow(train.ADALog.predicted), ncol = 1)

for (i in 1:nrow(train.ADALog.predicted)) {
	prob = 0
	if (train.ADALog.predicted[i, 1] < 
	    train.ADALog.predicted[i, 2])
		prob = train.ADALog.predicted[i, 2]
	else
		prob = train.ADALog.predicted[i, 1]
	train.ADALog.predicted.prob[i] = prob 
}

auc = roc.area(train[,1], train.ADALog.predicted.prob)
roc.plot(train[,1], train.ADALog.predicted.prob)


###AUC = 0.80

####Test it

test.ADALog.predicted = predict(model.train.adaLogistic, 
	newdata=test[,-1], type = "probs")

test.ADALog.predicted.prob = 
	matrix(nrow = nrow(test.ADALog.predicted), ncol = 1)

for (i in 1:nrow(test.ADALog.predicted)) {
	prob = 0
	if (test.ADALog.predicted[i, 1] < 
	    test.ADALog.predicted[i, 2])
		prob = test.ADALog.predicted[i, 2]
	else
		prob = test.ADALog.predicted[i, 1]
	test.ADALog.predicted.prob[i] = prob 
}

test.ADALog.data = cbind(test[,1], test.ADALog.predicted.prob)
colnames(test.ADALog.data) = c("id", "ACTION")
write.csv(test.ADALog.data, "C:/Users/parag/Documents/data_mining/project/amazon_predicted_3.csv")

###AUC = 0.74759


##########################
stopCluster(cl)
