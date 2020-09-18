# Importing library
library(caret) # data partitioning
library(kernlab) # for ksvm
library(e1071) # for svm

# Import data
# 1.) classify the Size_Categories using SVM

forest <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SupportVectorMachines/forestfires.csv")
View(forest)
names(forest)
table(forest$size_category)
str(forest$size_category)
barplot(table(forest$size_category))
forest <- forest[,-c(1,2)] # removing month and day variable.
summary(forest)
attach(forest)

# EDA
#Data partitioning

set.seed(333)
forest_train_test <- createDataPartition(forest$size_category, times=1, p=0.7,list = FALSE)
forest_train <- forest[forest_train_test, ]
forest_test  <- forest[-forest_train_test, ]

##Training a model on the data ----
# begin by training a simple linear SVM

forest_linear <- ksvm(size_category ~ ., data = forest_train,kernel = "vanilladot") # vanilladot is like basic linear plot
#plot(forest_linear, forest_train)

# basic information about the model

#svmfit <- svm( size_category~ ., data = forest_train, kernel = "linear",scale = FALSE)
#plot(svmfit,forest_train)

## Evaluating model performance ----
# predictions on test dataset
forest_predict <- predict( forest_linear, forest_test)

head(forest_predict)

table( forest_predict)
agreement <- forest_predict == forest_test$size_category
table(agreement)
prop.table(table(agreement))

# accuracy
acc_vanilaplot <- mean(forest_predict==forest_test$size_category) #0.98

## Improving model performance ----
forest_rbf <- ksvm(size_category ~ ., data = forest_train, kernel = "rbfdot")
forest_predict_rbf <- predict(forest_rbf, forest_test)
agreement_rbf <- forest_predict_rbf == forest_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))

acc_rbfdot <- mean(forest_predict_rbf == forest_test$size_category) #0.82467

forest_polydot <- ksvm(size_category ~ ., data = forest_train, kernel = "polydot")
forest_predict_polydot <- predict(forest_polydot, forest_test)

agreement_polydot <- forest_predict_polydot == forest_test$size_category
table(agreement_polydot)
prop.table(table(agreement_polydot))
acc_polydot <-  mean(forest_predict_polydot == forest_test$size_category) # 0.9805

#2 Prepare a classification model using SVM for salary data 

# Import data

salary_train <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SupportVectorMachines/SalaryData_Train.csv")
salary_test <-  read.csv("D:/EXCELR/ASSIGNMENTS/R/SupportVectorMachines/SalaryData_Test.csv")
attach(salary_train)
View(salary_train)
str(salary_train)
names(salary_train)
barplot(table(salary_train$Salary), main="Barplot of Train datasets")
barplot(table(salary_test$Salary), main="Barplot of Test datasets")

##Training a model on the data ----
# begin by training a simple linear SVM

salary_linear <- ksvm(Salary ~ ., data = salary_train,kernel = "vanilladot") # vanilladot is like basic linear plot


# basic information about the model

svmfit <- svm( Salary~ ., data = salary_train, kernel = "linear",scale = FALSE)
plot(svmfit,salary_train)

## Evaluating model performance ----
# predictions on testing dataset
salary_predict <- predict( salary_linear, salary_test)

head(salary_predict)

table( salary_predict)


agreement <- salary_predict == salary_test$Salary
table(agreement)
prop.table(table(agreement))
# accuracy
acc_vanilaplot <- mean(salary_predict==salary_test$Salary) #0.84


## Improving model performance ----
salary_rbf <- ksvm(Salary ~ ., data = salary_train, kernel = "rbfdot")
salary_predict_rbf <- predict(salary_rbf, salary_test)

agreement_rbf <- salary_predict_rbf == salary_test$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))
?ksvm
acc_rbfdot <- mean(salary_predict_rbf == salary_test$Salary) # 0.854


salary_polydot <- ksvm(Salary ~ ., data = salary_train, kernel = "polydot")
salary_predict_polydot <- predict(salary_polydot, salary_test)

agreement_polydot <- salary_predict_polydot == salary_test$Salary
table(agreement_polydot)
prop.table(table(agreement_polydot))
acc_polydot <-  mean(salary_predict_polydot == slary_test$Salary) # 0.90
