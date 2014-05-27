repeated.vals <- function(fit, method, B, r) {
	z <- 0
	r <- max(r, 1)
	for(i in 1:r) z <- z + validate(fit,method=method,B=B, rule="aic", 
			type="residual", aic=0, pr = FALSE)
	z/r
}

repeated.glm <- function(fit, X, K, r) {
	z <- 0
	r <- max(r, 1)
	for(i in 1:r) z <- z + cv.glm(X, fit,K=K)$delta
	z/r
}

misclassification.rate=function(tab){
  num1=sum(diag(tab))
  denom1=sum(tab)
  signif(1-num1/denom1,5)
}

install.packages("klaR")
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
install.packages("caret")
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
library(caret)
library(klaR)

BankData = read.csv("C:\\Users\\parag\\Documents\\project\\bank-full.csv", 
				stringsAsFactors = TRUE, header=TRUE)
head(BankData[,-(17)])
Y <- as.factor(BankData$y)
X <- BankData[,-(17)]

covnames <- names(BankData)[-(17)]
Formula <-as.formula(paste("Y~",paste(covnames,collapse="+"),sep=""))
options(error=recover)

#############################LDA######################################
BankData.lda = lda(Formula, data=cbind(X, Y), method="mle", CV=TRUE)
summary(BankData.lda)

BankData.lda.epe = mean( BankData.lda$class != Y )
BankData.lda.epe
#EPE = 0.09951118
tab = table(BankData.lda$class,Y) 
misclassification.rate(tab)
###Mis-classification rate:0.0995

##QDA
BankData.qda = qda(Formula, data=cbind(X, Y), method="mle", CV=TRUE)
summary(BankData.qda)
####EPE: 
tab = table(BankData.qda$class,Y) 
misclassification.rate(tab)
###Mis-classification rate:0.13008


####################partioning for classification learning methods###########

#select N from data of Total
Sample <- sample(1:length(Y),ceiling(2*length(Y)/3))
#test 
Y.test <- Y[-Sample]
X.test <- X[-Sample,]

#training
Y <- Y[Sample]
X <- X[Sample,]
#p <- ncol(X)

#############################################################################

#############using glm logit regression######################################

BankData.glm.lrm = glm(Formula, data=X, family=binomial)
BankData.glm.cross = repeated.glm(BankData.glm.lrm
				, cbind(X, Y), 10, 10)
##EPE = Kfold = 0.07113783, Leave-one-out = 0.07112328

##glm logit to get truepositive and false negative graphs
BankData.glm.lrm.pred = predict(BankData.glm.lrm, newdata=X.test
					,type="response", se.fit=FALSE)
#######ROC curve to plot performance of binary classifier
BankData.lrm.rocr.pred = prediction(BankData.glm.lrm.pred, Y.test)
BankData.lrm.rocr.perf = performance(BankData.lrm.rocr.pred, measure = "tpr", x.measure = "fpr") 
plot(BankData.lrm.rocr.perf, col=rainbow(10))

limit = 0.5
obs = ifelse(BankData.glm.lrm.pred<limit,"no","yes") 
exp = factor(Y.test)
mean(as.numeric(exp!=obs))
###EPE: 0.08825481

tab = table(obs,exp) 
misclassification.rate(tab)
###Mis-classification rate:0.0883

###########################################################################
####################Naive bayes####################################
BankData.naiveBayes = naiveBayes(Formula, data=X, type="class")
summary(BankData.naiveBayes)
BankData.naiveBayes.predict = predict(BankData.naiveBayes, newdata=X.test, type="class")
obs = BankData.naiveBayes.predict
exp = as.factor(Y.test)
mean(as.numeric(obs!=exp))
##EPE: 0.1783013

tab = table(BankData.naiveBayes.predict, Y.test)
misclassification.rate(tab)
###Mis-classification rate:0.1783013
######################################################################

########################rpart##################################
BankData.rpart = rpart(Formula , data=X, method="class", x=TRUE, 
			y=TRUE, model=TRUE, control = rpart.control(cp = 0.0005))
#summary(BankData.rpart)
plot(BankData.rpart); text(BankData.rpart)
plotcp(BankData.rpart)
printcp(BankData.rpart)
BankData.rpart.prune = prune(BankData.rpart, cp=0.0012)
BankData.rpart.prune
plot(BankData.rpart.prune); text(BankData.rpart.prune)
summary(BankData.rpart.prune)
printcp(BankData.rpart.prune)
exp = predict(BankData.rpart.prune,newdata=X.test, type="class")
obs = factor(Y.test)
mean(as.numeric(exp!=obs))
#EPE: [1] mean = 0.09535501
tab = table(exp, Y.test)
misclassification.rate(tab)
###Mis-classification rate:0.0954

####################################################################

###########################bagging##################################
BankData.bagging = bagging(Formula, data=X, nbagg=50, coob=TRUE)
BankData.bagging$err
summary(BankData.bagging)
#Error rate/mis classification rate on training data: 0.09598222
BankData.bagging.predict = 
	predict(BankData.bagging,newdata=X.test, type="class")
obs.bagging = factor(Y.test)
mean(as.numeric(exp.bagging!=obs.bagging))
#EPE: 0.1406105
tab = table(BankData.bagging.predict, Y.test)
misclassification.rate(tab)
###Mis-classification rate:0.0944

#######################################################################

#####################Random forest##################################
BankData.randomForest = randomForest(Formula, data=X, importance=TRUE)
BankData.randomForest$importance 
varImpPlot(BankData.randomForest)
plot(BankData.randomForest)
max(BankData.randomForest$err.rate)
min(BankData.randomForest$err.rate)

#min error rate of any tree in training forest: 0.03641141, max: 0.5395601
BankData.forest.predict = 
	predict(BankData.randomForest,newdata=X.test, type="response")
obs.forest = factor(Y.test)
mean(as.numeric(BankData.forest.predict!=obs.forest))
#EPE: 0.09190445
tab = table(BankData.forest.predict, Y.test)
misclassification.rate(tab)
##misclassification rate:0.0919

#####################################################################

####################nnet#############################
set.seed(1234)
BankData.nnet = nnet(Formula, data=X, size=10, decay=0.01, maxit=1000, 
				linout=FALSE, trace=FALSE)
BankData.nnet.predict = 
	predict(BankData.nnet,newdata=X.test, type="class")
obs.nnet = factor(Y.test)
mean(as.numeric(BankData.nnet.predict!=obs.nnet))
#EPE: 0.09555408
tab = table(BankData.nnet.predict, Y.test)
misclassification.rate(tab)
##misclassification rate:0.0956
#######################################################

################################glmnet################################
BankData.model.frame = model.frame(Formula, X)
BankData.model.matrix = 
	scale(model.matrix(Formula, BankData.model.frame)[,-1])

BankData.glmnet = cv.glmnet(x=BankData.model.matrix, 
				y=Y, family="binomial", standardize = FALSE,
				nfolds=10, type.measure = "deviance", alpha = 1)
histogram(BankData.glmnet$lambda)
plot(BankData.glmnet) 
BankData.glmnet$cvm

BankData.glmnet$lambda.min
log(BankData.glmnet$lambda.min)

BankData.glmnet$lambda.1se
coef(BankData.glmnet$glmnet.fit,s=BankData.glmnet$lambda.1se)

##from coef matrix, previous successful outcome, duration, month of oct/march is most significant
#poutcomesuccess     0.374468999
#duration            0.973738333
#monthmar            0.186420180

Formula.test <-as.formula(paste("Y.test~",paste(covnames,collapse="+"),sep=""))

BankData.model.test.frame = model.frame(Formula.test, cbind(Y.test,X.test))
BankData.model.test.matrix = 
	model.matrix(Formula.test, BankData.model.test.frame)[,-1]

BankData.glmnet.predict = predict(BankData.glmnet, newx=BankData.model.test.matrix, 
					s=BankData.glmnet$lambda.1se, type="response")
BankData.glmnet.predict = unscale(BankData.glmnet.predict,BankData.model.matrix)

obs = ifelse(BankData.glmnet.predict<0.5,"no","yes") 
exp = factor(Y.test)

tab = table(obs, Y.test)
misclassification.rate(tab)
##misclassification rate: 0.1172
