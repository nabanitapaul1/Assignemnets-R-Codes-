
# Import library
library(neuralnet)
library(nnet)
library(caret)
library(NeuralNetTools)
library(moments)
library(plyr)

#1.) Build a Neural Network model for 50_startups data to predict profit 

# Import dataset

startup <- read.csv("D:/EXCELR/ASSIGNMENTS/R/NeuralNetworks/50_Startups.csv")
View(startup)
names(startup)

#EDA

summary(startup)

# Graphical Representation

par(mfrow=c(1,2))

# for Administartion
hist(startup$Administration)
qqnorm(startup$Administration)
qqline(startup$Administration)
boxplot(startup$Administration)
skewness(startup$Administration)

# for R.D Spend
hist(startup$R.D.Spend)
qqnorm(startup$R.D.Spend)
qqline(startup$R.D.Spend)
boxplot(startup$R.D.Spend)
skewness(startup$R.D.Spend)

#for Marketing.Spend
hist(startup$Marketing.Spend)
qqnorm(startup$Marketing.Spend)
qqline(startup$Marketing.Spend)
boxplot(startup$Marketing.Spend)
skewness(startup$Marketing.Spend)

#for Profit
hist(startup$Profit)
qqnorm(startup$Profit)
qqline(startup$Profit)
boxplot(startup$Profit)
skewness(startup$Profit)

#for State
barplot(table(startup$State))

# Conversion  of State   variable into numerical

startup$State <- as.numeric(revalue(startup$State, c("New York" = "0", "California"= "1" ,"Florida"="2")))
str(startup)
View(startup)

# corelation between profits and other variables(Adminstartion, R.D Spend,Marketing Spend, States)
windows()
pairs(startup)

#Correlation coefficients -- Strength and direction correlation
cor(startup)

# Normalization
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

startup_norm<-as.data.frame(lapply(startup,FUN=normalize))
class(startup_norm)
summary(startup_norm)

# Data Partitioning

startup_train_test <- createDataPartition(startup_norm$Profit, times=1, p=0.7, list=FALSE)
startup_train <- startup_norm[startup_train_test,]
startup_test <- startup_norm[-startup_train_test,]

# Model Building

# Creating a neural network

attach(startup)

#USing one single hidden layer
?neuralnet
startup_model <- neuralnet(Profit~R.D.Spend+Administration
                           +Marketing.Spend+State,data = startup_train)
#startup_model <- neuralnet(Profit~R.D.Spend+Administration
#                           +Marketing.Spend+State,data = startup_train, stepmax=1e7)
#startup_model2 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = startup_train, 
 #                           linear.output=FALSE, likelihood=TRUE,rep = 3)

str(startup_model)
#str(startup_model2)

# Visualize the network topology
plot(startup_model, rep="best")
#plot(startup_model2, rep="best")

par(mar = numeric(4), family = 'serif')
plotnet(startup_model, alpha = 0.2)

# Evaluating model performance
names(startup)
results_model <- c()
results_model <- compute(startup_model, startup_test[1:4]) # compute functions to generate output for the prepared models
# obtain predicted profit values
str(results_model)
results_model$neurons
predicted_profit <- results_model$net.result


# examine the correlation between predicted and actual values
cor(predicted_profit, startup_test$Profit)#0.9748265
plot(predicted_profit,startup_test$Profit)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(startup$Profit)
str_min <- min(startup$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)


## Improving model performance ----
# a more complex neural network topology with 2 hidden neurons
startup_model2 <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = startup_train, hidden = 2)



# plot the network
plot(startup_model2)

# evaluate the results as we did before
model_results2 <- compute(startup_model2, startup_test[1:4])
predicted_profit <- model_results2$net.result
cor(predicted_profit, startup_test$Profit)#0.9654926
plot(predicted_profit,startup_test$Profit)

model_results2$net.result

# A complex model with 5,5 hidden layers
set.seed(12345)
startup_model3 <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = startup_train, hidden =c(5,5))



# plot the network
plot(startup_model3)

# evaluate the results as we did before
model_results3 <- compute(startup_model3, startup_test[1:4])
predicted_profit <- model_results3$net.result
cor(predicted_profit, startup_test$Profit)#0.9665219
plot(predicted_profit,startup_test$Profit)

model_results3$net.result

#SSE (error) is reducing, as no. of hidden layers are increased.

#2.) PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS

# import data

forest_fire <- read.csv("D:/EXCELR/ASSIGNMENTS/R/NeuralNetworks/forestfires.csv")
summary(forest_fire)
View(forest_fire)
str(forest_fire)
forest_fire$size_category<-as.factor(forest_fire$size_category)
levels(forest_fire$size_category)


#Changing the values of size_category
forest_fire$size_category <- as.numeric(ifelse(forest_fire$size_category == "large", "1", "0")) 

colnames(forest_fire)
forest_fire <- forest_fire[,-c(1:2)]
names(forest_fire)
attach(forest_fire)
str(forest_fire)
summary(forest_fire)


# EDA

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
forest_norm <- as.data.frame(lapply(forest_fire, normalize))
View(forest_norm)
summary(forest_norm)

#Data Partitioning

forestfire_train_test <- createDataPartition(forest_norm$size_category, times=1, p=0.7, list=FALSE)
forestfire_train <- forest_norm[forestfire_train_test,]
forestfire_test <- forest_norm[-forestfire_train_test,]
names(forestfire_train)

# Model Building
# Creating a neural network

#forestfire_model <- neuralnet(size_category~.,data = forestfire_train[,-c(29)])
set.seed(333)
forestfire_model <- neuralnet(size_category~FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+      
                                daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+ monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep,
    
                                 data = forestfire_train,hidden=1,err.fct = "ce",linear.output = FALSE)	

str(forestfire_model)
summary(forestfire_model)

# Visualize the network topology
plot(forestfire_model, rep="best")

# Evaluating model performance
names(forestfire_test)
results_model <- c()
results_model <- compute(forestfire_model, forestfire_test[1:28]) # compute functions to generate output for the prepared models

# obtain predicted size_category values
str(results_model)
results_model$neurons
predicted_size_category <- results_model$net.result
head(results_model$net.result)
head(forestfire_test[1,])

# examine the correlation between predicted and actual values
#cor(predicted_size_category, forestfire_test$size_category)#0.867
#plot(predicted_size_category, forestfire_test$size_category)

#Confusion matrix and misclassification error
pred <- ifelse(predicted_size_category>0.5,1,0)
tab1 <- table(pred,forestfire_test$size_category) 
accuracy = sum(diag(tab1))/sum(tab1) # 0.9677
error = 1-accuracy #0.03225806

## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
set.seed(333)
forestfire_model2 <- neuralnet(size_category~FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+      
                                daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+ monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep,
                              
                              data = forestfire_train,hidden=5,err.fct = "ce",linear.output = FALSE)	

str(forestfire_model2)
summary(forestfire_model2)

# Visualize the network topology
plot(forestfire_model2, rep="best")


# Evaluating model performance
names(forestfire_test)
results_model <- c()
results_model <- compute(forestfire_model2, forestfire_test[1:28]) # compute functions to generate output for the prepared models

# obtain predicted size_category values
str(results_model)
results_model$neurons
predicted_size_category <- results_model$net.result
head(results_model$net.result)
head(forestfire_test[1,])

# examine the correlation between predicted and actual values
#cor(predicted_size_category, forestfire_test$size_category)#0.867
#plot(predicted_size_category, forestfire_test$size_category)

#Confusion matrix and misclassification error
pred <- ifelse(predicted_size_category>0.5,1,0)
tab1 <- table(pred,forestfire_test$size_category) 
accuracy = sum(diag(tab1))/sum(tab1) # 0.9741
error = 1-accuracy #0.02580

# A complex model with 5,5 hidden layers
set.seed(12345)
startup_model3 <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = startup_train, hidden =c(5,5))


# plot the network
plot(startup_model3)

# evaluate the results as we did before
model_results3 <- compute(startup_model3, startup_test[1:4])
predicted_profit <- model_results3$net.result
cor(predicted_profit, startup_test$Profit)#0.9665219
plot(predicted_profit,startup_test$Profit)
model_results3$net.result

#SSE (error) is reducing, as no. of hidden layers are increased.

#3.) Prepare a model for strength of concrete data using Neural Networks

# Import dataset

concrete = read.csv("D:/EXCELR/ASSIGNMENTS/R/NeuralNetworks/concrete.csv")
View(concrete)
summary(concrete)
str(concrete)

# EDA
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Training a model on the data ----
# train the neuralnet model

# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)
str(concrete_model)
summary(concrete_model)
# visualize the network topology
plot(concrete_model)


### SSE sum of squared errors. least SSE best models
## Evaluating model performance 

----
  # obtain model results
results_model <- c()
results_model <- compute(concrete_model, concrete_test[1:8]) # compute functions to generate output for the prepared models
# obtain predicted strength values
str(results_model)
results_model$neurons
predicted_strength <- results_model$net.result

# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)#0.8056
plot(predicted_strength,concrete_test$strength)
## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)


summary(concrete_model2)

# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)#0.92
plot(predicted_strength2,concrete_test$strength)
model_results2$net.result

#concrete_model2 <- neuralnet(strength ~ cement + slag +
#                              ash + water + superplastic +  
#                              coarseagg + fineagg + age,
#                            data = concrete_train, hidden = c(5,3))
