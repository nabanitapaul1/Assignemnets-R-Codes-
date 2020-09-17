#Forecast the CocaCola prices and Airlines Passengers data set. Prepare a document for each model explaining 
#how many dummy variables you have created and RMSE value for each model. Finally which model you will use for 
#Forecasting.

# Import data
library(readr)
library(readxl)

# Cococola

#Importing data set
cocacola <- read_excel("D:/EXCELR/ASSIGNMENTS/R/Forecasting/CocaCola_Sales_Rawdata.xlsx")
View(cocacola)
names(cocacola)
plot(cocacola$Sales,type = 'o')
length(cocacola$Sales)

# Data Pre processing 
# So creating 4 dummy variables 
# grepl = Pattern matching and replacement

Q1 <-  ifelse(grepl("Q1",cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cocacola$Quarter),'1','0')

cocacola<-cbind(cocacola,Q1,Q2,Q3,Q4)
View(cocacola)

colnames(cocacola)

# input t
cocacola["t"] <- c(1:42)
View(cocacola)

cocacola["log_sales"] <- log(cocacola["Sales"])
cocacola["t_square"] <- cocacola["t"]*cocacola["t"]
View(cocacola)

## Pre processing completed

attach(cocacola)
summary(cocacola)
cocacola$Sales

# partitioning
train <- cocacola[1:38,]
test <- cocacola[39:42,]
View(train)

#Model Building
########################### LINEAR MODEL #############################

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)

linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
rmse_linear <- sqrt(mean((test$Sales-linear_pred$fit)^2, na.rm = T))
rmse_linear # 591.553

######################### Exponential #################################

expo_model <- lm(log_sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo  # 466.248

######################### Quadratic ####################################

Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm=T))
rmse_Quad # 475.5618

######################### Additive Seasonality #########################

sea_add_model <- lm(Sales ~ Q1+Q2+Q3, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add # 1860.024

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Sales ~ t+t_square+Q1+Q2+Q3, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad # 301.738

######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_sales ~ Q1+Q2+Q3, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea # 1963.39

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic trend has least RMSE value


############### Combining Training & test data to build Additive seasonality with Quadratic Trend ############

# New Model

Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Q1+Q2+Q3, data = cocacola)
summary(Add_sea_Quad_model_final)

# Fitted values

new_model_fin <- Add_sea_Quad_model_final$fitted.values

View(new_model_fin)

Quarter <- as.data.frame(cocacola$Quarter)

Final <- as.data.frame(cbind(Quarter,cocacola$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
View(Final)

# Actual Model
plot(Final$Sales,main = "ActualGraph",col.axis="blue",type="o") 

#Predicted graph

plot(Final$New_Pred_Value, main = "PredictedGraph",col.axis="Green",type="s")

#airlines

# Importing data sets

airlines <- read_excel("D:/EXCELR/ASSIGNMENTS/R/Forecasting/Airlines+Data.xlsx")
View(airlines)
names(airlines)
plot(airlines$Passengers,type = 'o')
length(airlines$Month)
# Pre Processing

# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names 
View(X)
airlines_passengers <- cbind(airlines,X)
View(airlines_passengers)
colnames(airlines_passengers)

# input t
airlines_passengers["t"] <- c(1:96)
View(airlines_passengers)

airlines_passengers["log_passengers"] <- log(airlines_passengers["Passengers"])
airlines_passengers["t_square"] <- airlines_passengers["t"]*airlines_passengers["t"]
View(airlines_passengers)

summary(airlines_passengers)

## Pre processing completed

attach(airlines_passengers)

# partitioning
train <- airlines_passengers[1:84,]
test <- airlines_passengers[85:96,]
View(train)

# Model Building
########################### LINEAR MODEL #############################

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2, na.rm = T))
rmse_linear # 53.19924

######################### Exponential #################################

expo_model <- lm(log_passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo  # 46.05736

######################### Quadratic ####################################

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm=T))
rmse_Quad # 48.05189

######################### Additive Seasonality #########################

sea_add_model <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add # 132.898

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad # 26.36082

######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea # 140.0632

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic trend has least RMSE value


############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = airlines_passengers)
summary(Add_sea_Quad_model_final)

# Fitted values

new_model_fin <- Add_sea_Quad_model_final$fitted.values

View(new_model_fin)
names(airlines_passengers)
Month <- as.data.frame(airlines_passengers$Month)

Final <- as.data.frame(cbind(Month,Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
View(Final)
# Actual Model
plot(Final$Passengers,main = "ActualGraph",
     col.axis="blue",type="o") 

#Predicted graph

plot(Final$New_Pred_Value, main = "PredictedGraph",col.axis="Green",type="s")

