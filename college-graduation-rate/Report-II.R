install.packages("HH")
install.packages("rgl")
install.packages("lmtest")
install.packages("leaps")
install.packages("ggplot2")
install.packages("car")
install.packages("sos")
install.packages("outliers")
library(outliers)
library(leaps)
library(rgl)
library(lmtest)
library(HH)
library(ggplot2)
library(car)

names = c("FICE", "NAME", "STATE", "PUBLIC.PRIVATE", 
	"AVG.MATH.SAT", "AVG.VERBAL.SAT", "AVG.COMBINED.SAT", 
	"AVG.SAT", "FIRST.QTL.MATH", "THIRD.QTL.MATH", "FIRST.QTL.VERBAL", 
	"THIRD.QTL.VERBAL", "FIRST.QTL.SAT", "THIRD.QTL.SAT", "APPLICANTS", 
	"ACCEPTED", "ENROLLED", "HS.TOP.10", "HS.TOP.25", "FT.UNDERGRAD", 
	"PT.UNDERGRAD", "IN.STATE.TUTION", "OUT.STATE.TUTION", "ROOM.BOARD.COST", 
	"ROOM.COST", "BOARD.COST", "ADD.FEES", "BOOKS.COST", 
	"PERS.SPENDING", "PHD.FACT", "TERM.DEG.FACT", "STUDENT.FACT", "ALUM.DONAT", 
	"EXPEND.PER.STUDENT", "GRAD.RATE")

names.matrix = cbind("FICE", "NAME", "STATE", "PUBLIC.PRIVATE", 
	"AVG.MATH.SAT", "AVG.VERBAL.SAT", "AVG.COMBINED.SAT", 
	"AVG.SAT", "FIRST.QTL.MATH", "THIRD.QTL.MATH", "FIRST.QTL.VERBAL", 
	"THIRD.QTL.VERBAL", "FIRST.QTL.SAT", "THIRD.QTL.SAT", "APPLICANTS", 
	"ACCEPTED", "ENROLLED", "HS.TOP.10", "HS.TOP.25", "FT.UNDERGRAD", 
	"PT.UNDERGRAD", "IN.STATE.TUTION", "OUT.STATE.TUTION", "ROOM.BOARD.COST", 
	"ROOM.COST", "BOARD.COST", "ADD.FEES", "BOOKS.COST", 
	"PERS.SPENDING", "PHD.FACT", "TERM.DEG.FACT", "STUDENT.FACT", "ALUM.DONAT", 
	"EXPEND.PER.STUDENT", "GRAD.RATE")


#	1 "FICE", 2 "NAME", 3 "STATE", 4 "PUBLIC-PRIVATE", 
#	5 "AVG-MATH-SAT", 6 "AVG-VERBAL-SAT", 7 "AVG-COMBINED-SAT", 
#	8 "AVG-SAT", 9 "FIRST-QTL-MATH", 10 "THIRD-QTL-MATH", 11 "FIRST-QTL-VERBAL", 
#	12 "THIRD-QTL-VERBAL", 13 "FIRST-QTL-SAT", 14 "THIRD--QTL-SAT", 
#	15 "APPLICANTS", 
#	16 "ACCEPTED", 17 "ENROLLED", 18 "HS-TOP-10", 19 "HS-TOP-25", 
#	20 "FT-UNDERGRAD", 
#	21 "PT-UNDERGRAD", 22 "IN-STATE-TUTION", 23 "OUT-STATE-TUTION", 
#	24 "ROOM-BOARD-COST", 
#	25 "ROOM-COST", 26 "BOARD-COST", 27 "ADD-FEES", 28 "BOOKS-COST", 
#	29 "PERS-SPENDING", 30 "PHD-FACT", 31 "TERM-DEG-FACT", 32 "STUDENT-FACT", 
# 	33 "ALUM-DONAT", 
#	34 "EXPEND-PER-STUDENT", 35 "GRAD-RATE"

data = read.csv("C:/Users/dppodium/reg_project/usnews.csv", 
			stringsAsFactors=FALSE, col.names=names,
			header=FALSE, sep=",")
attach(data)
#data = data[complete.cases(data),]
grad_rate = data[,35]
#head(data)

#######find related predictor variable and store it in list###############

##find related variable and store its index in array.
discard = array(rep(0, 31), dim=31)
relation = list()
for (i in 1:31) { relation[[i]] = -1 }
for (i in 1:31) {
 if (discard[i] == 0) {
  for (j in 1:31) { 
	if (i != j) {
	  if (cor.matrix[i, j] >= 0.80) {
	    discard[j] = 1
	    relation[[i]]=c(relation[[i]], j)
	  }
	}
  }
 }
}

###############print interaction effects####################
k = 0
for (i in 4:34) {
  k = k + 1
  if (discard[k] == 0) {
    print (k)
    for (j in i:34) {
	print (j)
	if (i != j)	{ 
	  cor = round(cor(data[,i], data[,j], use = "pairwise.complete.obs", method = "spearman"), 3)
	  if (cor >= 0.85){
    	    print (c(names[i], names[j]))
	    print (cor)
    	    interaction.plot(data[,i], data[,j], GRAD.RATE, type="l", legend=FALSE)
    	    par(ask=TRUE)
	  }
  	}
    }
  }
}

######################################################
##center the input vars for later use
avgPublicPrivate = PUBLIC.PRIVATE - mean(PUBLIC.PRIVATE, na.rm=TRUE)
avgMath = AVG.MATH.SAT - mean(AVG.MATH.SAT, na.rm = TRUE)
avgSAT = AVG.COMBINED.SAT - mean(AVG.COMBINED.SAT, na.rm = TRUE)
boardCost = BOARD.COST - mean(BOARD.COST, na.rm = TRUE)
alumDonat = ALUM.DONAT - mean(ALUM.DONAT, na.rm = TRUE)
expenPerStud = EXPEND.PER.STUDENT - mean(EXPEND.PER.STUDENT, na.rm = TRUE)
thirdQtlSAT = THIRD.QTL.SAT - mean(THIRD.QTL.SAT, na.rm = TRUE)
hstop10 = HS.TOP.10 - mean(HS.TOP.10, na.rm = TRUE)
tution.total = I(IN.STATE.TUTION + OUT.STATE.TUTION)
tution = tution.total - mean(tution.total, na.rm = TRUE)
collType = PUBLIC.PRIVATE - mean(PUBLIC.PRIVATE, na.rm = TRUE)
avgOutTution = OUT.STATE.TUTION - mean(OUT.STATE.TUTION, na.rm=TRUE)
avgInState = IN.STATE.TUTION - mean(IN.STATE.TUTION, na.rm=TRUE)
avgFirstVerbal = FIRST.QTL.VERBAL - mean(FIRST.QTL.VERBAL, na.rm = TRUE)
avgThirdVerbal = THIRD.QTL.VERBAL - mean(THIRD.QTL.VERBAL, na.rm = TRUE)
avgFirstMath = FIRST.QTL.MATH - mean(FIRST.QTL.MATH, na.rm = TRUE)
avgThirdMath = THIRD.QTL.MATH - mean(THIRD.QTL.MATH, na.rm = TRUE)
avgApplicant = APPLICANTS - mean(APPLICANTS, na.rm = TRUE)
avgTop25 = HS.TOP.25 - mean(HS.TOP.25, na.rm = TRUE)
avgUndergrad = PT.UNDERGRAD - mean(PT.UNDERGRAD, na.rm = TRUE)
avgFTUndergrad = FT.UNDERGRAD - mean(FT.UNDERGRAD, na.rm = TRUE)
avgRoomBoardCost = ROOM.BOARD.COST - mean(ROOM.BOARD.COST, na.rm = TRUE)
avgRoomCost = ROOM.COST - mean(ROOM.COST, na.rm = TRUE)
avgBoardCost = BOARD.COST - mean(BOARD.COST, na.rm = TRUE)
avgAddFees = ADD.FEES - mean(ADD.FEES, na.rm = TRUE)
avgPersSpend = PERS.SPENDING - mean(PERS.SPENDING, na.rm = TRUE)
avgPHDFact = PHD.FACT - mean(PHD.FACT, na.rm = TRUE)
avgTermDegFact = TERM.DEG.FACT - mean(TERM.DEG.FACT, na.rm = TRUE)
avgGradRate = GRAD.RATE - mean(GRAD.RATE, na.rm=TRUE)

#############################################################################

############## T and F test on each predictor variable ######################

t.test(PUBLIC.PRIVATE, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
t.test(AVG.MATH.SAT, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
t.test(OUT.STATE.TUTION, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
t.test(ROOM.BOARD.COST, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
t.test(ALUM.DONAT, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
t.test(EXPEND.PER.STUDENT, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)

var.test(PUBLIC.PRIVATE, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
var.test(AVG.MATH.SAT, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
var.test(OUT.STATE.TUTION, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
var.test(ROOM.BOARD.COST, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
var.test(ALUM.DONAT, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)
var.test(EXPEND.PER.STUDENT, y=GRAD.RATE, data=data, na.action=na.exclude, conf.level=.95)

#############################################################################

####################First model using above variables#########################

formula.best = as.formula(GRAD.RATE ~ PUBLIC.PRIVATE + AVG.MATH.SAT 
	+ OUT.STATE.TUTION + ROOM.BOARD.COST + ALUM.DONAT + EXPEND.PER.STUDENT)
model.best = lm(formula.best, data=data, na.action=na.exclude)
summary(model.best)
ncvTest(model.best)
anova(model.best)
plot(model.best)
plot(model.best$residuals, model.best$fit)
qqPlot(model.best$residuals, dist= "norm", col=palette()[1], ylab="Residual Quantiles", main="Normal Probability Plot", pch=19)

vif(model.best)

boxcox.model = boxcox(model.best, plotit=TRUE, interp=TRUE)
boxcox.model$lambda 

#############again one of the best, transformed using bulge rule######################
### with interaction terms

formula.best = as.formula(I(GRAD.RATE^2) ~ I(PUBLIC.PRIVATE^2) + I(AVG.MATH.SAT^2)
	+ I(OUT.STATE.TUTION^2) + I(EXPEND.PER.STUDENT^2)
	+ PUBLIC.PRIVATE:ROOM.BOARD.COST
	+ PUBLIC.PRIVATE:I(OUT.STATE.TUTION^2)
	+ PUBLIC.PRIVATE:ALUM.DONAT
	+ EXPEND.PER.STUDENT:PUBLIC.PRIVATE)
model.best = lm(formula.best, data=data)
summary(model.best)
ncvTest(model.best)
anova(model.best)
plot(model.best)
hist(model.best$residuals)
plot(model.best$residuals, model.best$fit)
vif(model.best)
AIC(model.best)
qqPlot(model.best$residuals, dist= "norm", col=palette()[1], ylab="Residual Quantiles", main="Normal Probability Plot", pch=19)


###################################################################################################

####################Model without using outliers################################################

##
avg.math.out = rm.outlier(AVG.MATH.SAT, fill = FALSE, median = TRUE, opposite = FALSE)
gradrate.out = rm.outlier(GRAD.RATE, fill = FALSE, median = TRUE, opposite = FALSE)
public.private.out = rm.outlier(PUBLIC.PRIVATE, fill = TRUE, median = FALSE, opposite = FALSE)
outState.out = rm.outlier(OUT.STATE.TUTION, fill = FALSE, median = TRUE, opposite = FALSE)
roomBoard.out = rm.outlier(ROOM.BOARD.COST, fill = FALSE, median = TRUE, opposite = FALSE)
alumDonat.out = rm.outlier(ALUM.DONAT, fill = FALSE, median = TRUE, opposite = FALSE)
expenseStud.out = rm.outlier(EXPEND.PER.STUDENT, fill = TRUE, median = FALSE, opposite = FALSE)
gradrate.out = rm.outlier(GRAD.RATE, fill = FALSE, median = TRUE, opposite = FALSE)


formula.best = as.formula(gradrate.out ~ public.private.out^2 + avg.math.out^2 
	+ outState.out^2 + roomBoard.out^2 + alumDonat.out + expenseStud.out^2
	+ avg.math.out:public.private.out
	+ outState.out:public.private.out
	+ alumDonat.out:public.private.out
	+ expenseStud.out:public.private.out)
model.best = lm(formula.best, na.action=na.exclude)
summary(model.best)
ncvTest(model.best)
anova(model.best)
plot(model.best)
qqPlot(model.best$residuals, dist= "norm", col=palette()[1], ylab="Residual Quantiles", main="Normal Probability Plot", pch=19)

###########################################################################################################

######################Some other random (nonsense model)#################################################

formula.third_best = as.formula(avgGradRate ~ I(avgFirstMath^2) + I(avgMath^2) + I(avgThirdMath^2)  
	+ I(avgApplicant^2) + I(hstop10^2) + avgTop25 + tution + I(avgRoomBoardCost^2) 
	+ avgRoomCost + I(avgBoardCost^2) + avgAddFees + I(avgPersSpend^2) + I(avgPHDFact^2) + I(avgTermDegFact^2)
	+ alumDonat
	+ (I(avgMath^2):I(avgFirstMath^2)) 
	+ (I(avgMath^2):I(avgPersSpend^2)) 
	+ (avgThirdMath:avgMath) + 
	+ (hstop10:I(avgPersSpend^2)))
multi.model = lm(formula=formula.third_best, data=data, na.action=na.exclude)
summary(multi.model)
ncvTest(multi.model)
plot(multi.model)

#########################################################################################################
##done
###############
