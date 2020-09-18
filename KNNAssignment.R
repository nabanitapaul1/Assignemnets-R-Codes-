# import library
library(class)# for knn function
library(caret) # for data partition and confusion matrix
library(gmodels) # for crosstable function

#1.) Implement a KNN model to classify the animals in to categories

zoo_data = read.csv("D:/EXCELR/ASSIGNMENTS/R/KNN/Zoo.csv")
View(zoo_data)
colnames(zoo_data)
str(zoo_data)
dim(zoo_data)
summary(zoo_data)

# recode domestic as a factor
zoo_data$domestic <- factor(zoo_data$domestic, levels = c(0, 1),
                         labels = c("Wild Animal", "Domestic Animal"))

# table of diagnosis

table(zoo_data$domestic) #  Wildanimaland Domestic Animal
prop.table(table(zoo_data$domestic) )#proportion
str(zoo_data)
View(zoo_data)

# table or proportions with more informative labels
round(prop.table(table(zoo_data$domestic)) * 100, digits = 2)

zoo_data1<- zoo_data[-c(1,16)]
names(zoo_data1)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(0.01, 0.02, 0.03, 0.04, 0.05,0.06))
normalize(c(10, 20, 30, 40, 50))
# normalize the wbcd data
zoo_data_norm <- as.data.frame(lapply(zoo_data1, normalize))
View(zoo_data_norm)

# confirm that normalization worked
summary(zoo_data_norm)

# create training and test data
zoo_train_test <- createDataPartition(zoo_data$domestic, p=0.7,list=FALSE, times=1) 
length(zoo_train_test)
zoo_data_train<- zoo_data_norm[zoo_train_test,]
#zoo_data_train <- zoo_data_norm[1:81, ]
zoo_data_test <- zoo_data_norm[- zoo_train_test,]
#zoo_data_test <- zoo_data_norm[82:101, ]

# create labels for training and test data
zoo_data_train_labels <- zoo_data[zoo_train_test, 16]
#zoo_data_train_labels <- zoo_data[1:81, 16]
zoo_data_test_labels <- zoo_data[-zoo_train_test, 16]
#zoo_data_test_labels <- zoo_data[82:101, 16]

#---- Training a model on the data ----

# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the prediction model
# we have to test on test dataset

zoo_data_pred <- knn(train = zoo_data_train, test = zoo_data_test,
                      cl = zoo_data_train_labels, k=5)
length(zoo_data_pred)

##--------Evaluating model performance ----


# Create the cross tabulation of predicted vs. actual
CrossTable(x = zoo_data_test_labels, y = zoo_data_pred,
           prop.chisq=FALSE)

confusionMatrix(table(zoo_data_test_labels,zoo_data_pred))


x <- as.data.frame(zoo_data_pred)

x <- cbind(x,zoo_data_test_labels)
View(x)

# Accuracy

Accuracy <- mean(zoo_data_pred==zoo_data_test_labels)
error =1-Accuracy

#Other  for loop

zoo_pred <- NULL
zoo_accuracy <- NULL

for (i in 1:20) {

    zoo_pred <- knn(train = zoo_data_train, test = zoo_data_test,
                    cl = zoo_data_train_labels,k=i)
  
  zoo_accuracy[i] <- mean(zoo_pred==zoo_data_test_labels)
}

zoo_accuracy_knn <- as.data.frame(cbind(k=1:20,Accuracy =zoo_accuracy))
library(ggplot2)
#Visualization
ggplot(zoo_accuracy_knn,aes(k,Accuracy))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Accuracy')

View(zoo_accuracy_knn)
# Below is the final Model

zoo_data_pred <- knn(train = zoo_data_train, test = zoo_data_test,
                       cl = zoo_data_train_labels, k=3)

# Create the cross tabulation of predicted vs. actual
CrossTable(  x = zoo_data_pred,y= zoo_data_test_labels,
           prop.chisq=FALSE)

#Confusion Matrix
confusionMatrix(zoo_data_test_labels,zoo_data_pred)

# Accuracy

Accuracy <- mean(zoo_data_pred==zoo_data_test_labels)
error =1-Accuracy

x <- as.data.frame(zoo_data_pred)

x <- cbind(x,zoo_data_test_labels)
View(x)

# 2.) Prepare a model for glass classification using KNN

glass_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/KNN/glass.csv")
View(glass_data)
colnames(glass_data)
str(glass_data)
dim(glass_data)
summary(glass_data) 

factor(glass_data$Type)
# recode domestic as a factor
glass_data$Type <- factor(glass_data$Type, levels = c(1, 2,3,5,6,7),
                            labels = c("building_windows_float_processed","building_windows_non_float_processed","vehicle_windows_float_processed", "containers","tableware","headlamps"))
levels(glass_data$Type)                                      

# table of diagnosis

table(glass_data$Type)
prop.table(table(glass_data$Type))#proportion
str(glass_data$Type)
View(glass_data$Type)

# table or proportions with more informative labels
round(prop.table(table(glass_data$Type)) * 100, digits = 2)
glass_data1<- glass_data[-10]
names(glass_data1)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the glass data
glass_data_norm <- as.data.frame(lapply(glass_data1, normalize))
View(glass_data_norm)

# confirm that normalization worked
summary(glass_data_norm)

# Train and Test data
glass_train_test <- createDataPartition(glass_data$Type, p=0.7,list=FALSE, times=1) 

length(glass_train_test)
glass_train <- glass_data_norm[glass_train_test,]
glass_test <- glass_data_norm[- glass_train_test,]

# create labels for traing  and test  datasets
glass_train_labels <- glass_data[glass_train_test, 10]

glass_test_labels <- glass_data[-glass_train_test, 10]

# Applying KNN
#---- Training a model on the data ----

# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the prediction model
# we have to test on test dataset

glass_data_pred <- knn(train = glass_train, test = glass_test,
                     cl = glass_train_labels, k=1)


##--------Evaluating model performance ----

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_data_pred,
           prop.chisq=FALSE)

confusionMatrix(table(glass_test_labels,glass_data_pred))

# Accuracy

Accuracy <- mean(glass_data_pred==glass_test_labels)
error =1-Accuracy

#Other  for loop

glass_pred <- NULL
glass_accuracy <- NULL

for (i in 1:10) {
  glass_pred <- knn(train = glass_train, test = glass_test,
                    cl = glass_train_labels,k=i)
 
  glass_accuracy[i] <- mean(glass_pred==glass_test_labels)
}

glass_accuracy_knn <- as.data.frame(cbind(k=1:10,Accuracy =glass_accuracy))

#Visualization
ggplot(glass_accuracy_knn,aes(k,Accuracy))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Accuracy')

# Below is the final Model

glass_data_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=3)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_data_pred,
           prop.chisq=FALSE)

#Confusion Matrix
confusionMatrix(glass_test_labels,glass_data_pred)

# Accuracy

Accuracy <- mean(glass_data_pred==glass_test_labels)
error =1-Accuracy
