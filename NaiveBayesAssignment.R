
# importing library
library(e1071) # for Naive Bayes algorithm
library(gmodels) # for cross table
library(caret)  # for data partition and confusion matrix

#1.)1) Prepare a classification model using Naive Bayes for salary data 

# Training data

salary_train <- read.csv("D:/EXCELR/ASSIGNMENTS/R/NaiveBayes/SalaryData_Train.csv")
View(salary_train)
summary(salary_train)
str(salary_train)
names(salary_train)
levels(salary_train$Salary)

salary_train$workclass <-  as.factor(salary_train$workclass)
salary_train$education <-  as.factor(salary_train$education)
salary_train$maritalstatus <-  as.factor(salary_train$maritalstatus)
salary_train$occupation <-  as.factor(salary_train$occupation)
salary_train$relationship <-  as.factor(salary_train$relationship)
salary_train$race <-  as.factor(salary_train$race)
salary_train$sex <-  as.factor(salary_train$sex)
salary_train$native <-  as.factor(salary_train$native)
salary_train$Salary <-  as.factor(salary_train$Salary)


# Test data
salary_test <- read.csv("D:/EXCELR/ASSIGNMENTS/R/NaiveBayes/SalaryData_Test.csv")
View(salary_test)
str(salary_test)

salary_test$workclass <-  as.factor(salary_test$workclass)
salary_test$education <-  as.factor(salary_test$education)
salary_test$maritalstatus <-  as.factor(salary_test$maritalstatus)
salary_test$occupation <-  as.factor(salary_test$occupation)
salary_test$relationship <-  as.factor(salary_test$relationship)
salary_test$race <-  as.factor(salary_test$race)
salary_test$sex <-  as.factor(salary_test$sex)
salary_test$native <-  as.factor(salary_test$native)
salary_test$Salary <-  as.factor(salary_test$Salary)

#Graphical representation
barplot(table(as.factor(salary_train[,14]),as.factor(salary_train[,2])),legend=c("<=50K",">50K"))

#Naive Bayes

model<-naiveBayes(salary_train$Salary~.,data=salary_train)
summary(model)
model$levels
model$apriori    
model$isnumeric
model$tables
model$call
pred<-predict(model,newdata = salary_test[,-14])

#Accuracy
Accuracy <- mean(pred==salary_test$Salary) #0.819

#crosstable

ct <- CrossTable(x = salary_test$Salary, y = pred,prop.chisq=FALSE)

#Confusion Matrix

confusion <- confusionMatrix(salary_test$Salary,pred)
x <- as.data.frame(pred)
x<-  cbind(salary_test$Salary,x)
View(x)

#Accuracy

Accuracy <- sum(diag(ct$t))/sum(ct$t)

#2.) Build a naive Bayes model on the data set for classifying the ham and spam

sms_raw <- read.csv("D:/EXCELR/ASSIGNMENTS/R/NaiveBayes/sms_raw_NB.csv", stringsAsFactors = FALSE)
View(sms_raw)
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

# graphical represenation
barplot(table(sms_raw$type))

# build a corpus using the text mining (tm) package

library(tm)
# Converting to corpus

sms_corpus <- Corpus(VectorSource(sms_raw$text))
sms_corpus <- tm_map(sms_corpus, function(x) iconv(enc2utf8(x), sub='byte'))
class(sms_corpus)

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
class(sms_dtm)

# creating training and test datasets

sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


# indicator features for frequent words
# dictionary of words which are used more than 5 times
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
inspect(sms_corpus_train[1:100])

# convert counts to a factor
# custom function: if a word is used more than 0 times then mention 1 else mention 0
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
# Margin = 2 is for columns
# Margin = 1 is for rows
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
View(sms_train)

##  Training a model on the data ----

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
table(sms_test_pred)
prop.table(table(sms_test_pred))


CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
confusionMatrix(sms_test_pred, sms_raw_test$type)
#Accuracy
mean(sms_test_pred==sms_raw_test$type)  # 0.97
sms_raw_test$type
