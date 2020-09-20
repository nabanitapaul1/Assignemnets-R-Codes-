
#Importing Libraries

library(C50) # for decision tre
library(tree)  # for descision tree
library(caret) # for confusion matrix
library(party)
library(gmodels) # for crosstabulation
library(binst) # for creating bins


#1.)A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Approach - A decision tree can be built with target variable Sale (we will first convert it in categorical variable) & all other variable will be independent in the analysis.

# Importing dataset

company <- read.csv("D:/EXCELR/ASSIGNMENTS/R/RandomForests/Company_data.csv")
View(company)
summary(company)
str(company)
names(company)

#EDA

# Converting sales variable into categorical variable

#sales <- create_bins(company$Sales, breaks, method = "cuts")

#Creating Bins (to convert sales into categorical variable)

sales_bin <- cut(company$Sales,3, labels =c("Low","Average","High"))
company$salesStatus<- sales_bin
View(company)

# Creating training and test datasets

company_train_test <- createDataPartition(company$salesStatus, p=0.7, list=FALSE, times=1)
length(company_train_test)
company_train <- company[company_train_test,]
company_test <- company[-company_train_test,]
View(company_test)
min(company$Sales)
max(company$Sales)
str(company)
attach(company)
length(company_test)
# Decisison Tree
op_tree = tree(salesStatus ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = company_train)
summary(op_tree)
plot(op_tree)
text(op_tree,pretty = 0)

# Model Evaluation

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(op_tree,newdata=company_test))
# Model Performance
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=company_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

mean(pred_tree$final==company_test$salesStatus) # Accuracy = 68.067%
CrossTable(company_test$salesStatus,pred_tree$final)

#confusionMatrix(company_test$salesStatus,pred_tree$final)
class(company_test$Sales)
class(pred_tree$final)
pred<- predict(op_tree,newdata=company_test)
x<- cbind(company_test$High,pred)
View(x)

#2.) Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

# Import dataset

fraud <- read.csv("D:/EXCELR/ASSIGNMENTS/R/RandomForests/Fraud_check.csv")
View(fraud)
summary(fraud)
status = ifelse(fraud$Taxable.Income<=30000, "Risky", "Good")
fraud = data.frame(fraud, status)
attach(fraud)
View(fraud)
str(fraud)
names(fraud)

# EDA
fraud$Undergrad <- as.factor(fraud$Undergrad)
fraud$Marital.Status <- as.factor(fraud$Marital.Status)
fraud$Urban <- as.factor(fraud$Urban)
fraud$status <- as.factor(fraud$status)
# Creating training and test datasets

fraud_train_test <- createDataPartition(fraud$status, p=0.7, list=FALSE, times=1)
length(fraud_train_test)
fraud_train <- fraud[fraud_train_test,]
fraud_test <- fraud[-fraud_train_test,]
View(fraud_test)
names(fraud)

# Decisison Tree
op_tree = ctree(status ~Undergrad+Marital.Status+City.Population+Work.Experience+Urban, data = fraud_train)
summary(op_tree)
op_tree<- C5.0(fraud_train[,-c(3,7)],fraud_train$status)
windows()
names(fraud_train)
summary(op_tree)
plot(op_tree)
#text(op_tree,pretty = 0)
str(fraud)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(op_tree,newdata=fraud_test))
pred_tree["final"] <- NULL
pred <- predict(op_tree,newdata=fraud_test)
View(fraud_test)
length(pred)
View(pred)
# Accuracy
mean(pred==fraud_test$status) # Accuracy = 79.32%
CrossTable(pred,fraud_test$status)
confusionMatrix(pred,fraud_test$status)


