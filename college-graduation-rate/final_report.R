install.packages("leaps")
install.packages("car")
install.packages("outliers")

library(outliers)
library(leaps)
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

###########################First best model#############################################

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
AIC(model.step)
vif(model.step)
sqrt(vif(model.step)) > 3 #(problem)
avPlots(model.step, ask=TRUE)

outlierTest(model.step)
##
leveragePlots(model.step) # leverage plots
qqPlot(model.step, main="QQ Plot")

##studentized residuals
n = length(model.step$fitted.values)
qt(p = 1-0.01/2, df = model.step$df.residual)
qt(p = 1-0.01/2, df = model.step$df.residual-1)
qt(p = 1-0.05/(2*n), df = model.step$df.residual)
qt(p = 1-0.05/(2*n), df = model.step$df.residual-1)

residuals.stud = rstudent(model = model.step)
plot(x = model.step$fitted.values, y = residuals.stud, xlab = 
         "Estimated mean response", ylab = "Studentized 
         deleted residuals", main = expression(paste(t[i], 
         " vs. estimated mean response")), panel.first = 
         grid(col = "gray", lty = "dotted"), ylim = 
         c(min(qt(p = 0.05/(2*n), df = model.step$df.residual-
         1), min(residuals.stud)), max(qt(p = 1-0.05/(2*n), df = 
         model.step$df.residual-1), max(residuals.stud))))

#########compute dffit
p = 32
n = length(GRAD.RATE)
thresh = 2*sqrt(p/n) 
dfft = abs(dffits(model.full)) < thresh
data_infl = data[dfft,]

model.infl = lm(formula, data=data_infl)
summary(model.infl)
ncvTest(model.infl)
plot(model.infl)

####dfbeta plot
dfbetaPlots(model.infl)
####added variable plots
av.plots(model.infl, ask=TRUE)
############

#########################################################################################

###########################Second best model#############################################
#########################################################################################

###########################third best model#############################################
#########################################################################################


