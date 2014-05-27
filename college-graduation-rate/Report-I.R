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


##################descriptive statistics##################
par(mfrow=c(3,6))
for (i in 4:35) {
  stats = c(fivenum(data[, i]), mean(na.omit(data[,i])))
  stats = c(stats, median(na.omit(data[,i])))
  stats = c(stats, sd(na.omit(data[,i])))
  stats = c(names(data[i]), stats)
  stats <- paste(stats, collapse = " , ")
  print (stats)
  #stats = fivenum(data[, i])
  #form <- paste(stats, collapse = " + ")
  #boxplot(data[,i], main=names(data[i]), col=i)
  #par(ask=TRUE)
}
############################################################

##################multivariate descriptive analysis#########################################

cor.matrix = round(cor(data[,4:34], use = "pairwise.complete.obs", method = "spearman"), 3)

##find related variable and store it in relation list.
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

#### print colinear data
for (i in 1:length(discard)) {
  if (discard[i] == 0) { 
	##print indexes and names
	print (grep(colnames(data)[i+3], colnames(data)))
	print (colnames(data)[i+3]) 
  }
}

####find correlated with response variable and fit regression line for each####

count = 0
#data_comp = data[complete.cases(data),]
#grad_rate = data_comp[, 35]
j = 1
for (i in 4:34) {
  cor = round(cor(data[,i], GRAD.RATE, use = "pairwise.complete.obs", method = "spearman"), 3)
  if (cor >= 0.4 && (discard[j] == 0)){
    count = count + 1
    correlation = c(names[i], cor)
    print(correlation)
    form = as.formula(GRAD.RATE ~ data[,i])
    pairs(form, data=data_comp, upper.panel=panel.smooth, col=i,
	labels=c("GRAD.RATE", names(data[i])))
    par(ask=TRUE)
    ## fit regression line with respect to GRAD.RATE
    model = lm(form, data=data)
    print (summary(model))
    print (ncvTest(model))
    plot(model$fit, model$residuals)
    par(mfrow=c(2,2))
    plot(model)
    par(ask=TRUE)
  }
  j = j + 1
}

##report count of number of correlated variable.
count

###done
########################################################################################################


