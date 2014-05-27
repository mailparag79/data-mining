install.packages("HH")
install.packages("rgl")
install.packages("lmtest")
install.packages("leaps")
install.packages("ggplot2")
install.packages("car")
install.packages("sos")
library(sos); 
findFn("outlier")
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
stats = c(names(data[4]), fivenum(data[, 4]))
stats <- paste(stats, collapse = " , ")

par(mfrow=c(3,6))
for (i in 4:35) {
  stats = c(names(data[i]), fivenum(data[, i]))
  stats <- paste(stats, collapse = " , ")
  print (stats)
  #stats = fivenum(data[, i])
  #form <- paste(stats, collapse = " + ")
  #boxplot(data[,i], main=names(data[i]), col=i)
  #par(ask=TRUE)
}
boxplot(grad_rate, main=names(data[35]), col=35)
var(grad_rate)
fivenum(grad_rate)

############################################################

##################multivariate descriptive analysis#########################################

formula = as.formula(GRAD.RATE ~ PUBLIC.PRIVATE + AVG.MATH.SAT + AVG.VERBAL.SAT + 
    AVG.COMBINED.SAT + AVG.SAT + FIRST.QTL.MATH + THIRD.QTL.MATH + 
    FIRST.QTL.VERBAL + THIRD.QTL.VERBAL + FIRST.QTL.SAT + THIRD.QTL.SAT + 
    APPLICANTS + ACCEPTED + ENROLLED + HS.TOP.10 + HS.TOP.25 + 
    FT.UNDERGRAD + PT.UNDERGRAD + I(IN.STATE.TUTION + OUT.STATE.TUTION) + 
    ROOM.BOARD.COST + ROOM.COST + BOARD.COST + ADD.FEES + BOOKS.COST + 
    PERS.SPENDING + PHD.FACT + TERM.DEG.FACT + STUDENT.FACT + 
    ALUM.DONAT + EXPEND.PER.STUDENT)
pairs(formula, data=data)
multi.model1 = lm(formula, data=data)
summary(multi.model1)
bptest(multi.model1)
##pairwise correlation matrix

vif <- function(object, ...)
UseMethod("vif")

vif.default <- function(object, ...)
stop("No default method for vif. Sorry.")

vif.lm <- function(object, ...) {
  V <- summary(object)$cov.unscaled
  Vi <- crossprod(model.matrix(object))
        nam <- names(coef(object))
  if(k <- match("(Intercept)", nam, nomatch = F)) {
                v1 <- diag(V)[-k]
                v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
                nam <- nam[-k]
        } else {
                v1 <- diag(V)
                v2 <- diag(Vi)
                warning("No intercept term detected. Results may surprise.")
        }
        structure(v1*v2, names = nam)
} 
cor.matrix = round(cor(data[,4:34], use = "pairwise.complete.obs", method = "spearman"), 3)
vif(multi.model1)
vif.len = length(data.vif)

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

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
}

cor.matrix_selected = round(cor(data[,c(4, 5, 15, 21, 23:25, 27:30, 32:34)], 
	use = "pairwise.complete.obs", method = "spearman"), 3)

pairs(data[,c(4, 5, 15, 21, 23:25, 27:30, 32:34)], bg="light blue")

####colinear data
for (i in 1:length(discard)) {
  if (discard[i] == 0) { 
	#print (grep(colnames(data)[i+3], colnames(data)))
	print (colnames(data)[i+3]) 
  }
}

####correlated with response variable

count = 0
data_comp = data[complete.cases(data),]
grad_rate = data_comp[, 35]
j = 1
for (i in 4:34) {
  cor = round(cor(data[,i], GRAD.RATE, use = "pairwise.complete.obs", method = "spearman"), 3)
  if (cor >= 0.4 && (discard[j] == 0)){
    count = count + 1
    correlation = c(names[i], cor)
    print(correlation)
    form = as.formula(grad_rate ~ data_comp[,i])
    #pairs(form, data=data_comp, upper.panel=panel.smooth, col=i,
	#labels=c("GRAD.RATE", names(data[i])))
    #par(ask=TRUE)
    ## fit regression line with respect to GRAD.RATE
    model = lm(form, data=data_comp)
    #print (summary(model))
    #print (ncvTest(model))
    #plot(model$fit, model$residuals)
    plot(model)
    par(ask=TRUE)
  }
  j = j + 1
}

count
###############################################################################################

###################Multi-variate regression####################################################

count = 0
j = 1
for (i in 4:34) {
  cor = round(cor(data[,i], GRAD.RATE, use = "pairwise.complete.obs", method = "spearman"), 3)
  if (cor >= 0.3 && discard[j] == 0){
    count = count + 1
    correlation = c(names[i], cor)
    print(correlation)
  }
  j = j + 1
}

###############interaction effects####################
k = 0
for (i in 4:34) {
  k = k + 1
  if (discard[k] == 0) {
    print (k)
    for (j in i:34) {
	print (j)
	if (i != j)	{ 
	  cor = data.cor[i]
	  if (cor >= 0.3){
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
data = data_unscaled
data_scaled = data[, 4:35]

#create function that looks for values > +/- 2 sd from mean
outdet <- function(x) abs(scale(x)) >= 2
#index with the function to remove those values
data_scaled[!apply(sapply(data_scaled, outdet), 1, any), ]
grad.rate = data_scaled[,32]
formula.best = as.formula(GRAD.RATE ~ PUBLIC.PRIVATE + AVG.MATH.SAT 
	+ OUT.STATE.TUTION + ROOM.BOARD.COST + ALUM.DONAT + EXPEND.PER.STUDENT)
model.best = lm(formula, data=data_scaled)
summary(model.best)

formula = as.formula(GRAD.RATE ~ PUBLIC.PRIVATE + AVG.MATH.SAT + THIRD.QTL.SAT 
	+ HS.TOP.10 + I(IN.STATE.TUTION + OUT.STATE.TUTION) + ROOM.BOARD.COST 
	+ BOARD.COST + ALUM.DONAT + EXPEND.PER.STUDENT 
	+ (HS.TOP.10 : I(IN.STATE.TUTION + OUT.STATE.TUTION) : AVG.MATH.SAT)
	+ (BOARD.COST : ALUM.DONAT : EXPEND.PER.STUDENT) )
pairs(formula, data=data)

formula1 = as.formula(GRAD.RATE ~ I(AVG.MATH.SAT^2) + sqrt(THIRD.QTL.SAT)
	+ I(HS.TOP.10^2) + I(I(IN.STATE.TUTION + OUT.STATE.TUTION)^2))

formula2 = as.formula(GRAD.RATE ~ (AVG.MATH.SAT) + THIRD.QTL.SAT
	+ (HS.TOP.10) + I(IN.STATE.TUTION + OUT.STATE.TUTION))


##center the input vars
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

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(0.25,0.75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

avgGradRate = GRAD.RATE - mean(GRAD.RATE, na.rm=TRUE)
avgGradRate  = remove_outliers(avgGradRate)

formula3 = as.formula(GRAD.RATE ~ avgSAT + thirdQtlSAT 
	+ I(hstop10^2)  + tution + boardCost + alumDonat + expenPerStud 
	+ (tution:alumDonat))
formula4 = as.formula(sqrt(GRAD.RATE) ~ I(avgSAT^2) + thirdQtlSAT 
	+ I(hstop10^2)  + tution + boardCost + alumDonat + I(expenPerStud^2) 
	+ (tution:alumDonat))
formula5 = as.formula(sqrt(GRAD.RATE) ~ I(avgSAT^2) + thirdQtlSAT 
	+ I(hstop10^2)  + tution + boardCost + alumDonat + I(expenPerStud^2) 
	+ (avgSAT:hstop10))
formula6 = as.formula(sqrt(GRAD.RATE) ~ I(avgSAT^2) + thirdQtlSAT 
	+ I(hstop10^2)  + tution + boardCost + alumDonat + I(expenPerStud^2) 
	+ (alumDonat:boardCost))
formula7 = as.formula(sqrt(GRAD.RATE) ~ I(avgMath^2) + thirdQtlSAT 
	+ I(hstop10^2)  + tution + boardCost + alumDonat + I(expenPerStud^2) 
	+ (avgMath:hstop10)+(tution:alumDonat))

avgSAT.hstop10 = avgSAT*hstop10
formula8 = as.formula(sqrt(GRAD.RATE) ~ I(avgSAT^2) + thirdQtlSAT 
	+ tution + alumDonat + expenPerStud 
	+ avgSAT.hstop10)


formula9 = as.formula(avgGradRate ~ I(avgFirstMath^2) + I(avgMath^2) + I(avgThirdMath^2)  
	+ I(avgApplicant^2) + I(hstop10^2) + avgTop25 + tution + I(avgRoomBoardCost^2) 
	+ avgRoomCost + I(avgBoardCost^2) + avgAddFees + I(avgPersSpend^2) + I(avgPHDFact^2) + I(avgTermDegFact^2)
	+ alumDonat
	+ (I(avgMath^2):I(avgFirstMath^2)) 
	+ (I(avgMath^2):I(avgPersSpend^2)) 
	+ (avgThirdMath:avgMath) + 
	+ (hstop10:I(avgPersSpend^2)))
multi.model = lm(formula=formula9, data=data, na.action=na.exclude)
summary(multi.model)

formula10 = as.formula(avgGradRate ~ I(avgFirstMath^2) + I(avgMath^2) + I(avgThirdMath^2)  
	+ I(avgApplicant^2) + I(hstop10^2) + avgTop25 + tution + I(avgRoomBoardCost^2) 
	+ avgRoomCost + I(avgBoardCost^2) + avgAddFees + I(avgPersSpend^2) + I(avgTermDegFact^2)
	+ alumDonat
	+ (I(avgMath^2):I(avgFirstMath^2):I(avgPersSpend^2)) 
	+ (hstop10:I(avgPersSpend^2)))
multi.model = lm(formula=formula10, data=data, na.action=na.exclude)
summary(multi.model)

formula = as.formula(GRAD.RATE ~ PUBLIC.PRIVATE + AVG.MATH.SAT + AVG.VERBAL.SAT + 
    AVG.COMBINED.SAT + AVG.SAT + FIRST.QTL.MATH + THIRD.QTL.MATH + 
    FIRST.QTL.VERBAL + THIRD.QTL.VERBAL + FIRST.QTL.SAT + THIRD.QTL.SAT + 
    APPLICANTS + ACCEPTED + ENROLLED + HS.TOP.10 + HS.TOP.25 + 
    FT.UNDERGRAD + PT.UNDERGRAD + IN.STATE.TUTION + OUT.STATE.TUTION + 
    ROOM.BOARD.COST + ROOM.COST + BOARD.COST + ADD.FEES + BOOKS.COST + 
    PERS.SPENDING + PHD.FACT + TERM.DEG.FACT + STUDENT.FACT + 
    ALUM.DONAT + EXPEND.PER.STUDENT)
model.full = lm(formula, data=data)
summary(model.full)
model.step = step(model.full, data=data)
summary(model.step)

subset = regsubsets(x=formula, y=GRAD.RATE, data=data, method="seqrep", nvmax=20)
n = length(GRAD.RATE)
sout = summary(subset)
p <- apply(sout$which, 1, sum)

aic <- sout$bic - log(n) * p + 2 * p
plot(p, aic, ylab = "AIC")
ibest <- seq(along = aic)[aic == min(aic)]
foo <- sout$which[ibest, ]
form <- names(foo)[foo][-1]
form <- paste(form, collapse = " + ")
form <- paste("GRAD.RATE ~", form)
out.best <- lm(as.formula(form), data=data)
summary(out.best)
bptest(out.best)
anova(out.best)

n <- nrow(data)
expected <- qnorm( ((1:n) -.375) / (n + .25))
cor(sort(residuals(out.best)),expected)

p <- apply(sout$which, 1, sum)
plot(p, sout$bic, ylab = "BIC")
ibest <- seq(along = sout$bic)[sout$bic == min(sout$bic)]
foo <- sout$which[ibest, ]
form <- names(foo)[foo][-1]
form <- paste(form, collapse = " + ")
form <- paste("GRAD.RATE ~", form)
out.best <- lm(as.formula(form), data=data)
summary(out.best)
bptest(out.best)
anova(out.best)


plot(out.best)
library(car)
qq.plot(out.best$residuals, dist= "norm", col=palette()[1], ylab="Residual Quantiles", main="Normal Probability Plot", pch=19)

formula11 = as.formula(GRAD.RATE ~ THIRD.QTL.MATH + FIRST.QTL.VERBAL + APPLICANTS + PT.UNDERGRAD 
	+ I(IN.STATE.TUTION + OUT.STATE.TUTION) + ADD.FEES + ALUM.DONAT + EXPEND.PER.STUDENT)
pairs(formula11, data=data)
multi.model = lm(formula=formula11, data=data, na.action=na.omit)
summary(multi.model)

bptest(multi.model)
anova(multi.model)

formula90 = as.formula(GRAD.RATE ~ avgFirstVerbal + avgMath + avgThirdVerbal  
	+ avgApplicant + hstop10 + avgTop25 + avgFTUndergrad + tution + avgRoomBoardCost 
	+ avgRoomCost + avgBoardCost + avgAddFees + avgPersSpend + avgPHDFact + avgTermDegFact
	+ alumDonat 
	+ (avgFirstVerbal:avgMath) + (avgFirstVerbal:avgApplicant)
	+ (avgFirstVerbal:avgTop25) + (avgFirstVerbal:avgFTUndergrad) 
	+ (avgThirdVerbal:avgMath) 
	+ (hstop10:avgMath) + (hstop10:avgFirstVerbal) + (hstop10:avgThirdVerbal)
	+ (hstop10:avgPersSpend) + (avgTop25:avgMath) + (avgPersSpend:tution))

#	+ (avgPHDFact:tution) + (avgApplicant:avgRoomBoardCost) + (avgApplicant:avgPersSpend)
#	+ (tution:avgThirdVerbal) + (alumDonat :tution) + (hstop10:avgFirstVerbal))
pairs(formula9, data=data)
multi.model = lm(formula=formula9, data=data)
summary(multi.model)
bptest(multi.model)
anova(multi.model)


avgGradRate  = remove_outliers(avgGradRate)
avgThirdVerbal = remove_outliers(avgThirdVerbal )
avgApplicant = remove_outliers(avgApplicant )
alumDonat = remove_outliers(alumDonat )
tution = remove_outliers(tution )
avgUndergrad= remove_outliers(avgUndergrad)
hstop10= remove_outliers(hstop10)

formula10 = as.formula(avgGradRate ~ I(avgThirdVerbal^2)
	+ avgApplicant + alumDonat + tution + I(avgUndergrad^2)
	+ (hstop10:I(avgFirstVerbal^2)) + (I(avgFirstVerbal^2):avgApplicant) + I(avgThirdVerbal^2):alumDonat 
	+ avgApplicant:alumDonat + alumDonat:tution)

multi.model = lm(formula=formula10, data=data)
summary(multi.model)
bptest(multi.model)
plot((multi.model))

pairs(formula11, data=data)

##multivariate model
multi.model = lm(formula=formula9, data=data)
summary(multi.model)
predict.plot(multi.model)
plot((multi.model))
pairs(formula1, data=data)

formula = as.formula(GRAD.RATE ~ THIRD.QTL.MATH + 
    FIRST.QTL.VERBAL + THIRD.QTL.VERBAL + FIRST.QTL.SAT + THIRD.QTL.SAT + 
    APPLICANTS + ACCEPTED + ENROLLED + HS.TOP.10 + HS.TOP.25 + 
    FT.UNDERGRAD + PT.UNDERGRAD + I(IN.STATE.TUTION + OUT.STATE.TUTION) + 
    ROOM.BOARD.COST + ROOM.COST + BOARD.COST + ADD.FEES + BOOKS.COST + 
    PERS.SPENDING + PHD.FACT + TERM.DEG.FACT + STUDENT.FACT + 
    ALUM.DONAT + EXPEND.PER.STUDENT)
multi.model = lm(formula=formula, data=data)
summary(multi.model)
plot((multi.model))

plot(GRAD.RATE*THIRD.QTL.MATH)
plot(,residuals(multi.model),ylab="Residuals",xlab="Dispoinc * Targtpop",main="Residual analysis")
abline(h=0)


###############################################################################################




plot(grad_rate, type="l")
formula = as.formula(GRAD.RATE ~ I(IN.STATE.TUTION + OUT.STATE.TUTION))
pairs(formula, data=data)
names(data)[4]
colnames(data)[1]
names = names(data)
names
data.cor = array()
for (i in 4:34) {
  data.cor[i] = cor(data[,i], GRAD.RATE, use = "na.or.complete",method = "spearman")
  cor = data.cor[i]
  if (cor > 0.5){
    correlation = c(names[i], ":", cor)
    print(correlation)
    form = as.formula(GRAD.RATE ~ data[,i])
    pairs(form, data=data)
  }
}

cor(data[, 4:35], use = "na.or.complete", method="spearman")
library(cars)
vif(data[, 4:35])

install.packages("cars")
library("psych")
x=c(4:34)
x
partial.r(m=data.frame(x=data[,4:34], y=data[,35]),x=x, y=c(35))
partial.cor(data.frame(x=data[,4:34], y=data[,35]), tol)
plot(data.cor)

data.x.cor = array()
source("/home/pshah/Documents/regression_proj/pcor.R")
print(pcor.test(data[,4],data[,7],data[,35], method="spearman", use="mat"))

for (i in 4:34) {
  for(j in 4:34) {
    pcor = pcor.test(data[,i],data[,j],data[,35], method="spearman")
    if (!(pcor$estimate == 1) && pcor$estimate > 0.82) {
        print(c(names[i], " : ", names[j]))
        print(pcor)
    }
  }
}


for (i in 4:34) {
  print(fivenum(data[, i]))
}

form  <- as.formula(paste("GRAD.RATE ~ ", paste(colnames(data)[4:34], 
		collapse='+')))
form

data.model = lm(form, data=data)
data.matrix.model = model.matrix(data.model)
svd(data.matrix.model)
summary(data.model)
install.packages("rgl",dependencies="Depends")
library(rgl)
plot(data)
DAAG::vif(data.model)

# Open new graphic window for PC
X11()
plot3d(data)

library(lattice)
cloud(form,pch=1,cex=.7,scales = list(arrows=FALSE,cex= .65, col = "black", font = 3),main="3d data plot",zoom=.85,col="blue",pretty=FALSE,screen=list(z=40,x=-60))
