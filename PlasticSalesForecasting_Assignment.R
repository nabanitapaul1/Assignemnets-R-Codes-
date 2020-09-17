#Importing library

library(forecast)
library(fpp)
library(smooth) #
library(tseries)
library(MLmetrics) # For MAPE

#Import dataset

plastic <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Forecasting/PlasticSales.csv")
View(plastic)
summary(plastic)
names(plastic)
plot(plastic$Sales,type = 'o')
plastic_sales <- plastic$Sales
sum(is.na(plastic_sales))

#Converting to time series data

plastic_ts<-ts(plastic$Sales,frequency = 12,start=c(60))
View(plastic_ts)
class(plastic_ts)

# Plotting time series data
plot(plastic_ts)

plastic_train<-plastic_ts[1:48]
plastic_test<-plastic_ts[49:60] # Considering only 12 months of data for testing

# seasonal data
# converting time series object
plastic_train<-ts(plastic_train,frequency = 12)
plastic_test<-ts(plastic_test,frequency = 12)
View(plastic_train)

#Building Model

#1.  Moving Average
?ma
plastic_sales_ma <- ma(plastic_ts,order= 12, centre = TRUE)
plot(plastic_ts)
lines(plastic_sales_ma, col= "Red")

#2. Exponential Smoothing 
#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
# Simple Exponential 

hw_a<-HoltWinters(plastic_train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=12)) # Predicting for 12 months

# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=12))
hwa_mape<-MAPE(hwa_pred$fit,plastic_test)*100

# Double Exponential
# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(plastic_train,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 12))

# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=12))
hwab_mape<-MAPE(hwab_pred$fit,plastic_test)*100

# with alpha = 0.2, beta = 0.15, gamma = 0.05 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(plastic_train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 12))

# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=12))
hwabg_mape<-MAPE(hwabg_pred$fit,plastic_test)*100

# With out optimum values 
hw_na<-HoltWinters(plastic_train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 12))
hwna_pred
plot(forecast(hw_na,h=12))
hwna_mape<-MAPE(hwna_pred$fit,plastic_test)*100

hw_nab<-HoltWinters(plastic_train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=12))
hwnab_pred
plot(forecast(hw_nab,h=12))
hwnab_mape<-MAPE(hwnab_pred$fit,plastic_test)*100

hw_nabg<-HoltWinters(plastic_train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,plastic_test)*100

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

# Based on the MAPE value who choose Holts winter exponential technique  which assumes the time series
# Data level, trend, seasonality characters without default values of alpha, beta and gamma

new_model <- HoltWinters(plastic_ts)
new_model

plot(forecast(new_model,n.ahead=12))

# Forecasted values for the next 12 Months
forecast_new <- data.frame(predict(new_model,n.ahead=12))


