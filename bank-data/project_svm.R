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
#test 
Y.test <- Y[-Sample]
X.test <- X[-Sample,]

#training
Y <- Y[Sample]
X <- X[Sample,]
#p <- ncol(X)

#balanced test and training data
#test 
Y.balanced.test <- Y.balanced[-Sample]
X.balanced.test <- X.balanced[-Sample,]

#training
Y.balanced.train <- Y.balanced[Sample]
X.balanced.train <- X.balanced[Sample,]

covnames <- names(BankData)[-(17)]
Formula <-as.formula(paste("Y~",paste(covnames,collapse="+"),sep=""))


#############svm########################################
BankData.ksvm.degree2 = ksvm(Formula, data=X, type="C-svc", kernel="polydot", 
		kpar=list(degree=2), C=5, cross=10)
BankData.ksvm.degree1 = ksvm(Formula, data=X, type="C-svc", kernel="polydot", 
		kpar=list(degree=1), C=5, cross=10)
BankData.ksvm.degree2.prd = predict(BankData.ksvm.degree2, newdata=X.test, type="response", 
					coupler="minpair")
BankData.ksvm.degree1.prd = predict(BankData.ksvm.degree1, newdata=X.test, type="response", 
					coupler="minpair")

####EPE: 
tab = table(BankData.ksvm.degree2.prd,Y.test) 
misclassification.rate(tab)
##[1] 0.1207697
tab = table(BankData.ksvm.degree1.prd,Y.test) 
misclassification.rate(tab)
##[1] 0.0995355

#######################SVM using Nu-SVC######################################
BankData.ksvm.degree2 = ksvm(Formula, data=X, type="nu-svc", kernel="polydot", 
		kpar=list(degree=2), C=5, cross=10)
BankData.ksvm.degree1 = ksvm(Formula, data=X, type="nu-svc", kernel="polydot", 
		kpar=list(degree=1), C=5, cross=10)
BankData.ksvm.degree2.prd = predict(BankData.ksvm.degree2, newdata=X.test, type="response", 
					coupler="minpair")
BankData.ksvm.degree1.prd = predict(BankData.ksvm.degree1, newdata=X.test, type="response", 
					coupler="minpair")
####EPE:
tab = table(BankData.ksvm.degree2.prd,Y.test) 
misclassification.rate(tab)
##[1] 0.09482415
tab = table(BankData.ksvm.degree1.epe,Y.test) 
misclassification.rate(tab)
##[1] 0.1003318

######svm on balanced data######################################################

BankData.balanced.ksvm.degree2 = 
	ksvm(Formula, data=X.balanced.train, type="C-svc", kernel="polydot", 
		kpar=list(degree=2), C=5, cross=10)
BankData.balanced.ksvm.degree1 = 
	ksvm(Formula, data=X.balanced.train, type="C-svc", kernel="polydot", 
		kpar=list(degree=1), C=5, cross=10)
BankData.balanced.ksvm.degree2.prd = 
	predict(BankData.balanced.ksvm.degree2, newdata=X.balanced.test, 
		type="response", coupler="minpair")
BankData.balanced.ksvm.degree1.prd = 
	predict(BankData.balanced.ksvm.degree1, newdata=X.balanced.test, 
		type="response", coupler="minpair")

tab = table(BankData.balanced.ksvm.degree2.prd,Y.balanced.test) 
misclassification.rate(tab)
##EPE: [1] 0.4662309

tab = table(BankData.balanced.ksvm.degree1.prd,Y.balanced.test) 
misclassification.rate(tab)
##EPE: 0.5297023
