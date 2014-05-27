install.packages("HH")
install.packages("rgl")
install.packages("lmtest")
install.packages("leaps")
install.packages("ggplot2")
install.packages("car")
install.packages("sos")
install.packages("outliers")
install.packages("DAAG",dependencies="Depends")

library(car)
library(DAAG)
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

####################first fit the data using all predictor variable##########################


formula = as.formula(GRAD.RATE ~ PUBLIC.PRIVATE + AVG.MATH.SAT + AVG.VERBAL.SAT + 
    AVG.COMBINED.SAT + AVG.SAT + FIRST.QTL.MATH + THIRD.QTL.MATH + 
    FIRST.QTL.VERBAL + THIRD.QTL.VERBAL + FIRST.QTL.SAT + THIRD.QTL.SAT + 
    APPLICANTS + ACCEPTED + ENROLLED + HS.TOP.10 + HS.TOP.25 + 
    FT.UNDERGRAD + PT.UNDERGRAD + IN.STATE.TUTION + OUT.STATE.TUTION + 
    ROOM.BOARD.COST + ROOM.COST + BOARD.COST + ADD.FEES + BOOKS.COST + 
    PERS.SPENDING + PHD.FACT + TERM.DEG.FACT + STUDENT.FACT + 
    ALUM.DONAT + EXPEND.PER.STUDENT)
model.full = lm(formula, data=na.omit(data), na.action=na.omit)
summary(model.full)
ncvTest(model.full)
plot(model.full)

######stepswise search for best model using AIC value#####################

model.step = step(model.full, data=na.omit(data))
summary(model.step)
ncvTest(model.step)
plot(model.step)
aic1 = AIC(model.step)
vif(model.step)

CVlm(df=na.omit(data),form = formula.step, aic1,m=10)

##improvement?
formula.step = as.formula(GRAD.RATE ~ PUBLIC.PRIVATE + AVG.COMBINED.SAT 
	+ THIRD.QTL.MATH + FIRST.QTL.VERBAL + ENROLLED + PT.UNDERGRAD 
	+ OUT.STATE.TUTION + ROOM.BOARD.COST + ADD.FEES 
	+ ALUM.DONAT + EXPEND.PER.STUDENT
	+ PUBLIC.PRIVATE:ENROLLED 
	+ PUBLIC.PRIVATE:AVG.COMBINED.SAT
	+ OUT.STATE.TUTION:PUBLIC.PRIVATE
	+ ALUM.DONAT:OUT.STATE.TUTION)
model.step.mod = lm(formula.step, data=na.omit(data), na.action=na.omit)
summary(model.step.mod)
ncvTest(model.step.mod)
AIC(model.step.mod)

########################################################################

#########find subset of predictor variable based on response variable##################

subset = regsubsets(x=formula, y=GRAD.RATE, data=data, method="seqrep", nvmax=20)
n = length(GRAD.RATE)
sout = summary(subset)
p = apply(sout$which, 1, sum)

aic = sout$bic - log(n) * p + 2 * p
plot(p, aic, ylab = "AIC")
ibest = seq(along = aic)[aic == min(aic)]
foo = sout$which[ibest, ]
form = names(foo)[foo][-1]
form = paste(form, collapse = " + ")
form = paste("GRAD.RATE ~", form)
out.best = lm(as.formula(form), data=data)
summary(out.best)
ncvTest(out.best)
anova(out.best)
plot(out.best)
AIC(out.best)
qq.plot(out.best$residuals, dist= "norm", col=palette()[1], ylab="Residual Quantiles", main="Normal Probability Plot", pch=19)

######################################################################################################



