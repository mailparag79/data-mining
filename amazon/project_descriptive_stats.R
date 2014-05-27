install.packages("DMwR")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("rattle")
library(DMwR)
library(ggplot2)
library(Hmisc)
library(rattle)

###rattle mostly used for getting descriptive statistics

train = read.csv("C:/Users/parag/Documents/data_mining/project/train.csv", header = TRUE)
test = read.csv("C:/Users/parag/Documents/data_mining/project/test.csv", header = TRUE)
attach(train)
columnNames = colnames(train)
Y = as.factor(ACTION)

count_unique = function( column ) { length( unique( column )) }
apply( train, 2, count_unique )
apply( train, 2, describe )

plot(ACTION, ROLE_TITLE, xlim = c(0, 1), 
	main = "ACTION by ROLE TITLE", 
	xlab = "ACTION", ylab = "ROLE TITLE", 
	col = c("dark green", "red"), 
	pch = c(21, 22), cex = 2, lwd = 2)
qplot(as.factor(ACTION), data = train, fill = I("blue"), xlab = "ACTION")
qplot(as.factor(ACTION), data = train, fill = as.factor(ROLE_TITLE), 
	scale_y_continuous(limits = c(0, 10000)), position = "dodge")

qplot(Y, data=train, geom="bar", fill=Y, xlab = "ACTION")
