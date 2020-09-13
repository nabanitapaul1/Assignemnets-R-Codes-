# Import Library

library(mice) # to identify missing value
library(VIM) # for aggregation of missing value
library(caret) # test train partitioning
library(ROCR) # ROC curve

# 1.) Whether the client has subscribed a term deposit or not

bank_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/LogisticsRegression/bank-full.csv", header=TRUE,sep = ";")
names(bank_data)
View(bank_data)
attach(bank_data)
levels(bank_data$job) # admin, blue-collar, entrepreneur, housemaid, management, retired, self-employed, services,student, technician, unemployed, unknown
levels(marital) # divorced, married, single
levels(education) # primary, secondary, tertiary, unknown
levels(default)   # no, yes

levels(housing) # no, yes
levels(loan)    # no, yes
levels(contact) # cellular, telephone, unknown
levels(month)    # jan to dec
levels(poutcome)  # failure, other success, unknown
levels(y)         # no, yes

 # EDA 
summary(bank_data)
str(bank_data)

# Analysis of missing Value

md.pattern(bank_data)
md.pairs(bank_data)
sum(is.na(bank_data))

aggr_plot <- aggr(bank_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bank_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# There is no missing data

#Graphical Representation

num_data <- data.frame(age,balance,day,duration,campaign,pdays,previous)
length(bank_data$age)
View(num_data)

#Co-relation Plots
pairs(num_data)
cor(num_data)      

str(bank_data$y)

# changing response variable in to binary format

bank_data$y <- ifelse(bank_data$y == "yes", 1, 0)
bank_data$y <- factor(bank_data$y, levels = c(0, 1))
str(bank_data)
View(bank_data)

# Convert the response variable in numeric format

#bank_data$y <- as.numeric(as.character(bank_data$y)) #This step is not required here 


# Divding into train and test data


set.seed(100)
trainDataIndex <- createDataPartition(bank_data$y, p=0.7, list = F)  # 70% training data
trainData <- bank_data[trainDataIndex, ] 

testData <- bank_data[-trainDataIndex, ]
View(trainData)
View(testData)

# Imbalanced  datasets

table(trainData$y) # 0 = 27946,1 = 3703
#The %ni% is the negation of the %in% function and 
#I have used it here to select all the columns except the y column.

'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

# Oversampling
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "y"],
                     y = trainData$y)
names(up_train)[17] <- "y"
names(up_train)
View(up_train)
table(up_train$y)  #  0 = 27946, 1 = 27946
attach(up_train)

# Logistics Regression Model

logitmod_bankdata <- glm(y ~ ., family = "binomial", data=up_train)
summary(logitmod_bankdata)
pred <- predict(logitmod_bankdata,  testData, type = "response")
table(pred)
exp(coef(logitmod_bankdata))
View(testData)

# Confusion matrix table
testData$y
table(testData$y)
confusion <-table(pred>0.5,testData$y)
confusion
probo <- pred>0.5
table(probo)    # False= 10340 True = 3222

# accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))  # 0.8399941
error = 1-Accuracy  # 0.16

# Accuracy Calculation

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$y
mean(y_pred == y_act)  # 0.8399

# AOC curve
length(pred)
length(testData$y)

rocrpred<-prediction(pred,testData$y)
rocrperf<-performance(rocrpred,'tpr','fpr')


plot(rocrperf,colorize=T,text.adj=c(-2,2.5), main="ROC Curve")
abline(a=0,b=1)


# 2 Whether the credit card accepted or not

credit_card <- read.csv("D:/EXCELR/ASSIGNMENTS/R/LogisticsRegression/creditcard.csv")
credit_card <- credit_card[,-1]
View(credit_card)
names(credit_card)
attach(credit_card)
str(credit_card)

#EDA
summary(credit_card)

# Analysis of missing Value

md.pattern(credit_card)
md.pairs(credit_card)

aggr_plot <- aggr(credit_card, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(credit_card), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# There is no missing values

# Graphical Representation

num_data <- data.frame(reports, age , income, share, expenditure, dependents, months, majorcards,active)
View(num_data)

#Co-relation Plots

pairs(num_data)
cor(num_data)      

#Histogram
par(mfrow=c(1,8))
for(i in 1:8) {
  hist(credit_card[,i], main=names(credit_card)[i])
}

#barplot
par(mfrow=c(1,8))
for(i in 1:8) {
  boxplot(credit_card[,i], main=names(credit_card)[i])
}

correlations <- cor(credit_card[,c(3:7,10:13)])
corrplot(correlations, method="circle")
pairs(credit_card[,c(3:7,10:13)])

x <- credit_card[,c(3:7,10:13)]
y <- credit_card[,2]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# changing response variable in to binary format
credit_card$card <- ifelse(credit_card$card == "yes", 1, 0)
credit_card$card <- factor(credit_card$card, levels = c(0, 1))
str(credit_card)
View(credit_card)
# Convert the response variable in numeric format

#credit_card$card <- as.numeric(as.character(credit_card$card)) #This step is not required here 

# Dividing into train and test data

set.seed(100)
trainDataIndex <- createDataPartition(credit_card$card, p=0.7, list = F)  # 70% training data
trainData <- credit_card[trainDataIndex, ]

testData <- credit_card[-trainDataIndex, ]
View(credit_card)


# Imbalanced  datasets

table(trainData$card) # 0 = 208,1 = 717
#The %ni% is the negation of the %in% function and 
#I have used it here to select all the columns except the y column.

'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

# Upsample
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "card"],
                     y = trainData$card)
names(up_train)[12] <- "card"
names(up_train)
View(up_train)
table(up_train$card)  #  0 = 717, 1 = 717
attach(up_train)

# Logistics Regression Model

logitmod_creditdata <- glm(card ~ ., family = "binomial", data=up_train)
summary(logitmod_creditdata)
pred <- predict(logitmod_creditdata,  testData, type = "response")
table(pred)
exp(coef(logitmod_creditdata))

# Confusion matrix table

confusion <-table(pred>0.5,testData$card)
confusion
probo <- pred>0.5
table(probo)    # False= 200 True = 194

# accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))  # 0.944
error = 1-Accuracy  # 0.0558

# Accuracy Calculation
card_pred_num <- ifelse(pred > 0.5, 1, 0)
card_pred <- factor(y_pred_num, levels=c(0, 1))
card_act <- testData$card
mean(card_pred == card_act)  # 0.71

# AOC curve
length(pred)
length(testData$card)

rocrpred<-prediction(pred,testData$card)
rocrperf<-performance(rocrpred,'tpr','fpr')

plot(rocrperf,colorize=T,text.adj=c(-2,2.5), main="ROC Curve")
abline(a=0,b=1)







