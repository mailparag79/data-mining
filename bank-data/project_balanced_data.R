repeated.vals <- function(fit, method, B, r) {
	z <- 0
	r <- max(r, 1)
	for(i in 1:r) z <- z + validate(fit,method=method,B=B, rule="aic", 
			type="residual", aic=0, pr = FALSE)
	z/r
}

misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,3)
}

install.packages("randomForest")
install.packages("ipred")
install.packages("e1071")
install.packages("kernlab")
install.packages("DMwR")
install.packages("Design")
install.packages("glmnet")
install.packages("bestglm")
install.packages("ROCR")
install.packages("DAAG")

library(nnet)
library(DMwR)
library(e1071)
library(bestglm)
library(glmnet)
library(Design)
library(rpart)
library(ipred)
library(randomForest)
library(boot)
library(kernlab)
library(ROCR)
library(DAAG)

BankData = read.csv("C:\\Users\\parag\\Documents\\project\\bank_mod.csv", header=TRUE)
head(BankData[,-(17)])
Y <- as.factor(BankData$y)
X <- BankData[,-(17)]

###Resolve class imbalance
BankData.smote = SMOTE(Formula, data=cbind(X, Y), perc.over = 600)
Y.balanced <- as.factor(BankData.smote$Y)
X.balanced <- BankData.smote[,-(17)]

#select N from data of Total
Sample <- sample(1:length(Y),ceiling(2*length(Y)/3))

#balanced test and training data
#test 
Y.balanced.test <- Y.balanced[-Sample]
X.balanced.test <- X.balanced[-Sample,]

#training
Y.balanced.train <- Y.balanced[Sample]
X.balanced.train <- X.balanced[Sample,]

covnames <- names(BankData)[-(17)]
Formula <-as.formula(paste("Y~",paste(covnames,collapse="+"),sep=""))

##################Naive bayes with balanced data##########################################
BankData.balanced.naiveBayes = 
	naiveBayes(Formula, data=X.balanced.train, type="class", laplace=1)
summary(BankData.balanced.naiveBayes)
BankData.balanced.naiveBayes$table
BankData.balanced.naiveBayes.predict = 
	predict(BankData.balanced.naiveBayes, newdata=X.balanced.test, type="class")
obs = BankData.balanced.naiveBayes.predict
exp = as.factor(Y.balanced.test)
tab = table(obs,exp) 
misclassification.rate(tab)
##EPE: 0.5308741
#######ROC curve to plot performance of binary classifier
BankData.balanced.naiveBayes.predict = 
	predict(BankData.balanced.naiveBayes, newdata=X.balanced.test, type="class")
BankData.balanced.naiveBayes.rocr.pred = 
	prediction(as.numeric(BankData.balanced.naiveBayes.predict), as.numeric(Y.balanced.test))
BankData.balanced.naiveBayes.rocr.perf = 
	performance(BankData.balanced.naiveBayes.rocr.pred, measure="tpr",x.measure="fpr") 
plot(BankData.balanced.naiveBayes.rocr.perf, col=rainbow(10))

table(BankData.balanced.naiveBayes.predict, Y.balanced.test)
#                                    Y.balanced.test
#BankData.balanced.naiveBayes.predict  no yes
#                                 no  524 618
#                                 yes  44  61
#####################################################################################

##################rpart using balanced data##########################################
#set.seed(1)
BankData.balanced.rpart = rpart(Formula , data=X.balanced.train, method="class", x=TRUE, 
			y=TRUE, model=TRUE, control = rpart.control(cp = 0.0005))
summary(BankData.balanced.rpart)
plot(BankData.balanced.rpart); text(BankData.balanced.rpart)
plotcp(BankData.balanced.rpart)
printcp(BankData.balanced.rpart)
BankData.rpart.balanced.prune = prune(BankData.balanced.rpart, cp=0.0044)
BankData.rpart.balanced.prune
plot(BankData.rpart.balanced.prune); text(BankData.rpart.balanced.prune)
summary(BankData.rpart.balanced.prune)
printcp(BankData.rpart.balanced.prune)
obs = predict(BankData.rpart.balanced.prune,newdata=X.balanced.test,type="class")
exp = factor(Y.balanced.test)
######EPE:
tab = table(obs,exp) 
misclassification.rate(tab)
##EPE = 0.5292665
#######ROC curve to plot performance of binary classifier
BankData.rpart.balanced.rocr.pred = 
	prediction(as.numeric(BankData.rpart.balanced.predict), as.numeric(Y.balanced.test))
BankData.rpart.balanced.rocr.perf = 
	performance(BankData.rpart.balanced.rocr.pred, measure = "tpr", x.measure = "fpr") 
plot(BankData.rpart.balanced.rocr.perf, col=rainbow(10))


######################################################################################################

#########################bagging on balanced data#####################################################
BankData.balanced.bagging = bagging(Formula, data=X.balanced.train, nbagg=50, coob=TRUE)
BankData.balanced.bagging$err
summary(BankData.balanced.bagging)
#Error rate/mis classification rate/MSE: [1] 0.1532847
BankData.bagging.balanced.predict = 
	predict(BankData.balanced.bagging,newdata=X.balanced.test, type="class")
exp.bagging = factor(Y.balanced.test)
######EPE:
tab = table(BankData.bagging.balanced.predict,exp.bagging) 
misclassification.rate(tab)
#EPE: 0.5503268
#######ROC curve to plot performance of binary classifier
BankData.bagging.balanced.rocr.pred = 
	prediction(as.numeric(BankData.bagging.balanced.predict), as.numeric(Y.balanced.test))
BankData.bagging.balanced.rocr.perf = 
	performance(BankData.bagging.balanced.rocr.pred, measure = "tpr", x.measure = "fpr") 
plot(BankData.bagging.balanced.rocr.perf, col=rainbow(10))
######################################################################################################

###############################random forest on balanced data#########################################
BankData.balanced.randomForest = randomForest(Formula, data=X.balanced.train, importance=TRUE)
BankData.balanced.randomForest$importance 
varImpPlot(BankData.balanced.randomForest)
plot(BankData.balanced.randomForest)
min(BankData.balanced.randomForest$err.rate)
#min error rate of any tree in forest: 0.0552009, max: 0.9430199
BankData.forest.balanced.predict = 
	predict(BankData.balanced.randomForest,newdata=X.balanced.test, type="response")
obs.bagging = factor(Y.balanced.test)
######EPE:
tab = table(BankData.forest.balanced.predict,obs.bagging) 
misclassification.rate(tab)
#EPE: 0.5600581
#######ROC curve to plot performance of binary classifier 
BankData.forest.balanced.rocr.pred = 
	prediction(as.numeric(BankData.forest.balanced.predict), as.numeric(Y.balanced.test))
BankData.forest.balanced.rocr.perf = 
	performance(BankData.forest.balanced.rocr.pred, measure = "tpr", x.measure = "fpr") 
plot(BankData.forest.balanced.rocr.perf, col=rainbow(10))



