
# importing library

library(lattice)
library(ggplot2)
library(moments)

#1.) Calories_consumed-> predict weight gained using calories consumed

# Importing datasets

cal.wt <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/calories_consumed.csv")
#View(cal.wt)
summary(cal.wt)
class(cal.wt)
colnames(cal.wt)<-c("weight_gained","calories_consumed")
attach(cal.wt)
colnames(cal.wt)
min(cal.wt$calories_consumed)
max(cal.wt$calories_consumed)

#EDA
# Graphical Representation

# plots for calories consumed 
dotplot(calories_consumed, main="Dot Plot of Calories consumed")
boxplot(calories_consumed,col="dodgerblue4")


hist(calories_consumed)
qqnorm(calories_consumed)# quantiles quantiles plot, plot all the points
qqline(calories_consumed) 

# pots for weight gained
dotplot(weight_gained, main="Dot Plot of Weight Gained")
boxplot(weight_gained,col="red", horizontal = FALSE)

hist(weight_gained)
qqnorm(weight_gained)
qqline(weight_gained)

# Scatter plot (between calories consumed and weight gained)
plot(calories_consumed,weight_gained,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Calories consumed", 
     ylab="Weight gained", pch=20)

# correlation (between calories consumed and weight gained)
cor(calories_consumed,weight_gained) # 0.946991

# Model Building 
#Regression

# Model1 
reg_cal.wt <-lm(weight_gained~calories_consumed, data=cal.wt) # Y ~ X   #  linear model
summary(reg_cal.wt) # R-square = 0.8968
class(reg_cal.wt)
str(reg_cal.wt)
R_squared_lm <- summary(reg_cal.wt)$r.squared 
reg_cal.wt$coefficients
reg_cal.wt$residuals

# Regression Line
calories_consumed.pred  =  -625.7523557 +0.4201566* weight_gained
-625.7523557 +0.4201566* 3400
900- 802.7801 #(error=actual-predicted)

#Root Mean Square Error
RMSE_lm<- sqrt(sum(reg_cal.wt$residuals^2)/nrow(cal.wt))  #RMSE = 103.3025


pred <- predict(reg_cal.wt)
pred
cor(pred, weight_gained) # 0.94corelation between input (predicted value and output variable
plot(pred,weight_gained)

# visualization
ggplot(data = cal.wt, aes(x = calories_consumed, y = weight_gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal.wt, aes(x=calories_consumed, y=pred))


# Model Evaluation

# doing transformation of input variable

# Model 2

# Square Root Transfromation
cor(weight_gained,sqrt(calories_consumed))  # 0.92

# transform the variables to check whether the predicted values are better
cal.wt_pred_sqrt <- lm(weight_gained~sqrt(calories_consumed), data=cal.wt)
summary(cal.wt_pred_sqrt) #0.85
R_squared_sqrt  <- summary(cal.wt_pred_sqrt)$r.squared
plot(sqrt(calories_consumed),weight_gained)
pred_sqrt <- predict(cal.wt_pred_sqrt)

# Root mean Square value

RMSE_sqrt <-  sqrt(sum(cal.wt_pred_sqrt$residuals^2)/nrow(cal.wt)) ## RMSE = 121.7122

ggplot(data = cal.wt, aes(x = sqrt(calories_consumed), y = weight_gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal.wt, aes(x=sqrt(calories_consumed), y=pred_sqrt))

cal.wt_pred_sqrt$residuals
cal.wt_pred_sqrt$coefficients

# Model 3
# Logarithmic transformation

hist(log(calories_consumed))
skewness(log(calories_consumed))
cor(weight_gained,log(calories_consumed)) # 0.89
plot(log(calories_consumed),weight_gained ) 

reg_log <-lm(weight_gained~log(calories_consumed), data=cal.wt)
summary(reg_log)  #R-square = 0.8077
R_squared_log <- summary(reg_log)$r.squared
reg_log$coefficients
reg_log$residuals
pred_log <-  predict(reg_log)
RMSE_log <- sqrt(sum(reg_log$residuals^2)/nrow(cal.wt)) ## RMSE = 141.005

pred_log <- predict(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

ggplot(data = cal.wt, aes(x = log(calories_consumed) , y = weight_gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal.wt, aes(x=log(calories_consumed), y=pred_log))


# Model 4

# Polynomial model with 2 degree (quadratic model)

 
plot(calories_consumed*calories_consumed, weight_gained)

cor(calories_consumed*calories_consumed, weight_gained) # 0.9710
hist(log(weight_gained))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(weight_gained) ~ calories_consumed + I(calories_consumed*calories_consumed))

summary(reg2degree) #0.98
R_squared_quadratic <- summary(reg2degree)$r.squared

logpol <- predict(reg2degree)
expy <- exp(logpol)
reg2degree$coefficients
err = weight_gained - expy

RMSE_quadratic <- sqrt(sum(err^2)/nrow(cal.wt))  #SE # 117.4145

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = cal.wt, aes(x = calories_consumed + I(calories_consumed^2), y = weight_gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal.wt, aes(x=calories_consumed+I(calories_consumed^2), y=expy))

cal_wt <-  data.frame(c("Linear","Square_root","Logarithmic","Quadratic"), c(R_squared_lm, R_squared_sqrt,R_squared_log,R_squared_quadratic),c(RMSE_lm,RMSE_sqrt,RMSE_log,RMSE_quadratic))
colnames(cal_wt)<-c("Model","R-Squared","RMSE")
View(cal_wt)


#2.) Delivery_time -> Predict delivery time using sorting time 

# Importing Data

dvl.st <-  read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/delivery_time.csv")
#View(dvl.st)
colnames(dvl.st) <- c("delivery_time","sorting_time")
attach(dvl.st)
min(dvl.st$sorting_time)
max(dvl.st$sorting_time)

#EDA 
#Graphical Representation

# for delivery time

hist(delivery_time)
boxplot(delivery_time)
qqnorm(delivery_time)
qqline(delivery_time)
dotplot(delivery_time, main="Dot Plot of delivery time")
skewness(delivery_time)
kurtosis(delivery_time)

# for sorting time

hist(sorting_time)
boxplot(sorting_time)
qqnorm(sorting_time)
qqline(sorting_time)
dotplot(sorting_time, main="Dot Plot of sorting time")
skewness(sorting_time)
kurtosis(sorting_time)

# Scatter - Plot

plot(sorting_time,delivery_time, main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="sorting time", 
     ylab="delivery time", pch=20) 

# Co-relation Analysis
cor(sorting_time,delivery_time) # 0.82
#ggplot(dvl.st,aes(x=sorting_time,y= delivery_time ), color="steelblue")


# Model Building

#Regression

# Model1
reg_dvl.st <- lm(delivery_time~sorting_time, data = dvl.st) 
summary(reg_dvl.st) # R2 = 0.68
R_squared_lm <- summary(reg_dvl.st)$r.squared
pred_dvl.st <- predict(reg_dvl.st) 

reg_dvl.st$residuals

sum(reg_dvl.st$residuals)

mean(reg_dvl.st$residuals)


# Root Mean Square Error
RMSE_lm <- sqrt(sum(reg_dvl.st$residuals^2)/nrow(dvl.st))  #RMSE = 2.79

sqrt(mean(reg_dvl.st$residuals^2))

confint(reg_dvl.st,level=0.95)
predict(reg_dvl.st,interval="predict")

# ggplot for adding regresion line for data


ggplot(data = dvl.st, aes(x = sorting_time, y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time, y=pred_dvl.st))

# Model Evaluation

#Transformation
# Square root transformation

# Model2 

cor(sqrt(sorting_time), delivery_time) #0.83 

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(delivery_time~sqrt(sorting_time), data=dvl.st)
summary(reg_sqrt) # R-square = 0.6958
R_squared_sqrt <- summary(reg_sqrt)$r.squared
plot(sqrt(sorting_time),delivery_time) 

# Root mean Square value

RMSE_sqrt<- sqrt(sum(reg_sqrt$residuals^2)/nrow(dvl.st)) # 2.73 
pred_sqrt <- predict(reg_sqrt)

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
reg_sqrt$ coefficients
ggplot(data = dvl.st, aes(x = sqrt(sorting_time), y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sqrt(sorting_time), y=pred_sqrt))

# Logarithmic transformation
cor(delivery_time,log(sorting_time)) # 0.83
plot(delivery_time,log(sorting_time))

reg_log <-lm(delivery_time~log(sorting_time), data=dvl.st)
summary(reg_log)  #R-square = 0.69
R_squared_log <- summary(reg_log)$r.squared
reg_log$coefficients

RMSE_log <- sqrt(sum(reg_log$residuals^2)/nrow(dvl.st)) ## RMSE = 2.7331

pred_log <- predict(reg_log)
confint(reg_log,level=0.95)


predict(reg_log,interval="predict")

ggplot(data = dvl.st, aes(x = log(sorting_time), y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=log(sorting_time), y=pred_log))

# Exponential Model

# Model 3

hist(log(delivery_time))
boxplot(log(delivery_time))

# x = sorting_time and y = log(delivery_time)

plot(sorting_time, log(delivery_time))

cor(sorting_time, log(delivery_time)) # 0.84

reg_exp <- lm(log(delivery_time) ~ sorting_time)  #lm(log(Y) ~ X)

summary(reg_exp) # R-squared = 0.71
R_squared_expo <- summary(reg_exp)$r.squared


reg_exp$residuals
reg_exp$coefficients
pred_ex <- predict(reg_exp)
at <- exp(pred_ex)
error = delivery_time -at

RMSE_expo <- sqrt(sum(error^2)/nrow(dvl.st))  #RMSE = 2.94


confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

ggplot(data = dvl.st, aes(x = sorting_time, y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time, y=at))

#  Model 4
# Polynomial model with 2 degree (quadratic model)


plot(sorting_time*sorting_time, delivery_time)

cor(sorting_time*sorting_time, delivery_time) # 0.79

plot(sorting_time*sorting_time, log(delivery_time))

cor(sorting_time, log(delivery_time)) #0.84
cor(sorting_time*sorting_time, log(delivery_time))0.78

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(delivery_time) ~ sorting_time + I(sorting_time*sorting_time))

summary(reg2degree) #0.76

R_squared_quadratic <- summary(reg2degree)$r.squared
logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time - expy

RMSE_quadratic <- sqrt(sum(err^2)/nrow(dvl.st))  #RMSE # 2.79


confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = dvl.st, aes(x = sorting_time + I(sorting_time^2), y = log(delivery_time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time+I(sorting_time^2), y=logpol))

#  Polynomial model with 3 degree

reg3degree<-lm(log(delivery_time)~sorting_time + I(delivery_time*sorting_time) + I(sorting_time*sorting_time*sorting_time))

summary(reg3degree) #R- Square =  0.9276
R_squared_3degree <- summary(reg3degree)$r.squared

logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = dvl.st, aes(x = sorting_time + I(sorting_time^2) + I(sorting_time^3), y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dvl.st, aes(x=sorting_time+I(sorting_time^2)+I(sorting_time^3), y=expy3))


dvl_st <- data.frame(c("Linear","Square_root","Exponential","Logarithmic"),c(R_squared_lm,R_squared_sqrt,R_squared_expo,R_squared_log),c(RMSE_lm,RMSE_sqrt, RMSE_expo, RMSE_log))
colnames(dvl_st)<- c("Model","R-squared","RMSE")

View(dvl_st)


# 3.) Emp_data -> Build a prediction model for Churn_out_rate

# Import Data

emp_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/emp_data.csv")
#View(emp_data)
attach(emp_data)
colnames(emp_data)
min(emp_data$Churn_out_rate)
max(emp_data$Churn_out_rate)

#EDA
# Graphical Representation

# For Salary Hike
hist(Salary_hike)
boxplot(Salary_hike)
qqnorm(Salary_hike)
qqline(Salary_hike)
skewness(Salary_hike)

kurtosis(Salary_hike)

# For Churn out rate
hist(Churn_out_rate)
boxplot(Churn_out_rate)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)
skewness(Churn_out_rate)
kurtosis(Churn_out_rate)

#  Scatter Plot

plot(Salary_hike,Churn_out_rate, xlab="salary_hike",ylab="churn_out_rate")

# Co-relation
cor(Salary_hike,Churn_out_rate) # - 0.911

# Model Building
# Regression 

# Model1 
reg_sal_churn <- lm(Churn_out_rate~ Salary_hike,data =emp_data )
summary(reg_sal_churn) # R square = 0.83
R_squared_lm <- summary(reg_sal_churn)$r.squared

#Root mean Square error
RMSE_lm <- sqrt(sum(reg_sal_churn$residuals^2)/nrow(emp_data)) # = 3.99## RMSE 
pred_sal_churn <- predict(reg_sal_churn) 

reg_sal_churn$residuals
reg_sal_churn$coefficients
sum(reg_sal_churn$residuals) 

mean(reg_sal_churn$residuals)

confint(reg_sal_churn,level=0.95)
predict(reg_sal_churn,interval="predict")

# ggplot for adding regresion line for data

ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred_sal_churn))


# Model Evaluation

# Model2  
#Transformation
# Square root transformation

cor(sqrt(Salary_hike), Churn_out_rate) #-0.91 

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike), data=emp_data)
summary(reg_sqrt) # R-square = 0.84
R_squared_sqrt <-summary(reg_sqrt)$r.squared 
plot(sqrt(Salary_hike),Churn_out_rate) 

# Root mean Square value

RMSE_sqrt<-  sqrt(sum(reg_sqrt$residuals^2)/nrow(emp_data)) # = 3.89## RMSE 
pred_sqrt <- predict(reg_sqrt)

pred_sqrt2 <- pred_sqrt * pred_sqrt
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

ggplot(data = emp_data, aes(x = sqrt(Salary_hike), y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=sqrt(Salary_hike), y=pred_sqrt))
reg_sqrt$coefficients
reg_sqrt$residuals

# Model 3

# Logarithmic transformation
hist(log(Salary_hike))
skewness(log(Salary_hike))
cor(Churn_out_rate,log(Salary_hike)) # -0.92
plot(log(Salary_hike),Churn_out_rate )

reg_log <-lm(Churn_out_rate~log(Salary_hike), data=emp_data)
summary(reg_log)  #R-square = 0.84
R_squared_log <- summary(reg_log)$r.squared
reg_log$coefficients
RMSE_log<- sqrt(sum(reg_log$residuals^2)/nrow(emp_data)) ## RMSE = 3.7
pred_log <- predict(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

ggplot(data = emp_data, aes(x = log(Salary_hike) , y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=log(Salary_hike), y=pred_log))

# Model 4

# Exponential Model

hist(log(Churn_out_rate))
boxplot(log(Churn_out_rate))
skewness(Churn_out_rate)
# x = sorting_time and y = log(delivery_time)

plot(Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate)) # -0.93

reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp) # R-squared = 0.87
R_squared_expo <- summary(reg_exp)$r.squared
reg_exp$residuals

sqrt(mean(reg_exp$residuals^2)) # RMSE
RMSE_expo <- sqrt(sum((exp(reg_exp$residuals))^2)/nrow(emp_data))   #RMSE = 1.002


logat <- predict(reg_exp) # predicted values
at <- exp(logat)

error = delivery_time - at

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=at))


# Model 5
# Polynomial model with 2 degree (quadratic model)


plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate) # -0.901


# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree) #0.98
R_squared_2degree <- summary(reg2degree)$r.squared

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time - expy

RMSE_2degree <-  sqrt(sum(reg2degree$residuals^2)/nrow(emp_data))  #RMSE # 0.0167

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x = Salary_hike + I(Churn_out_rate^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=expy))



emp.data<- data.frame(c("Linear","Square_Root", "Logarithmic"),c(R_squared_lm, R_squared_sqrt,R_squared_log),c(RMSE_lm,RMSE_sqrt,RMSE_log))
colnames(emp.data)<- c("Model","R-squared","RMSE")
View(emp.data)



#4) Salary_data -> Build a prediction model for Salary

# Importing data
salary_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/Salary_Data.csv")
#View(salary_data)
attach(salary_data)
min(salary_data$YearsExperience)
max(salary_data$YearsExperience)

#EDA
#Graphical Representation

# For YearsExperience
hist(YearsExperience)
boxplot(YearsExperience)
qqnorm(YearsExperience)
qqline(YearsExperience)
skewness(YearsExperience)
kurtosis(YearsExperience)

# For Salary
hist(Salary)
boxplot(Salary)
qqnorm(Salary)
qqline(Salary)
skewness(Salary)
kurtosis(Salary)

#  Scatter Plot

plot(YearsExperience,Salary, xlab="YearsExperience",ylab="Salary")

# Co-relation
cor(YearsExperience,Salary) # 0.978

# Regression 

# Model1 
reg_Ye_sal <- lm(Salary~ YearsExperience,data =salary_data )
summary(reg_Ye_sal) # R square = 0.957
R_squared_lm <- summary(reg_Ye_sal)$r.squared

#Root mean Square error
RMSE_lm <- sqrt(sum(reg_Ye_sal$residuals^2)/nrow(salary_data)) # = 5592.044## RMSE 
pred_Ye_Sal <- predict(reg_Ye_sal) 

reg_Ye_sal$residuals
reg_Ye_sal$coefficients
sum(reg_Ye_sal$residuals) 

mean(reg_Ye_sal$residuals)

confint(reg_Ye_sal,level=0.95)
predict(reg_Ye_sal,interval="predict")

# ggplot for adding regresion line for data
 
ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=pred_Ye_Sal))

# Model Evaluation
#Transformation

# Model 2
# Square root transformation

cor(sqrt(YearsExperience), Salary) #-0.964

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Salary~sqrt(YearsExperience), data=salary_data)
summary(reg_sqrt) # R-square = 0.93
R_squared_sqrt <- summary(reg_sqrt)$r.squared 
plot(sqrt(YearsExperience),Salary) 

# Root mean Square value

sqrt(sum(reg_sqrt$residuals^2)/nrow(salary_data)) # = 7080.096## RMSE 
RMSE_sqrt <- sqrt(sum(reg_sqrt$residuals^2)/nrow(salary_data))

pred_sqrt <- predict(reg_sqrt)
pred_sqrt2 <- pred_sqrt * pred_sqrt
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

ggplot(data = salary_data, aes(x = sqrt(YearsExperience), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=sqrt(YearsExperience), y=pred_sqrt))
reg_sqrt$coefficients
reg_sqrt$residuals

# Model 3
# Logarithmic transformation

hist(log(YearsExperience))
skewness(log(YearsExperience))
cor(Salary,log(YearsExperience)) # -0.92
plot(log(YearsExperience),Salary )

reg_log <-lm(Salary~log(YearsExperience), data=salary_data)
summary(reg_log)  #R-square = 0.85
R_squared_log <- summary(reg_log)$r.squared

reg_log$coefficients

RMSE_log<- sqrt(sum(reg_log$residuals^2)/nrow(salary_data)) ## RMSE = 10302.89
pred_log <- predict(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

ggplot(data = salary_data, aes(x = log(YearsExperience) , y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=log(YearsExperience), y=pred_log))


# Model 4
# Exponential Model

hist(log(Salary))
boxplot(log(Salary))
skewness(Salary)
# x = sorting_time and y = log(delivery_time)

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary)) # -0.93

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp) # R-squared = 0.93
R_squared_expo <- summary(reg_exp)$r.squared
exp(reg_exp$residuals)
exp(reg_exp$coefficients)
sqrt(mean(reg_exp$residuals^2)) # RMSE
sqrt(sum(reg_exp$residuals^2)/nrow(salary_data))   #RMSE = 0.094
logat <- predict(reg_exp) # predicted values
at <- exp(logat)
error = Salary - at

RMSE_expo <- sqrt(sum(error^2)/nrow(salary_data))  #  RMSE =  7213.235

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

ggplot(data = salary_data, aes(x = YearsExperience, y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=logat))


salary.data <-  data.frame(c("Linear","Square_root", "Exponential","Logarithmic"),c(R_squared_lm,R_squared_sqrt,R_squared_expo,R_squared_log),c(RMSE_lm,RMSE_sqrt,RMSE_expo,RMSE_log))
colnames(salary.data) <- c("Model","R-Squared","RMSE")
View(salary.data)


# Deployment

library(shiny)

ui= fluidPage(style = "border: 2px solid black",
  titlePanel(title= h3(tags$b("Multi Purpose  Dashboard"), align="center", style="color:Blue")),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      # Weight gain VS calories Consumed
      fluidRow("Weight Gain Calculator",br(),
               column(12, numericInput(inputId ="calorie",label="Please give your daily calorie intake", 1500 )),style = "border: 1px solid blue"),
      br(),
      fluidRow(" Food Delivery Time Calculator",br(),
               column(12, numericInput(inputId ="sort_time",label="Please give sorting time", 2 )),style = "border: 1px solid blue"),
      br(),
      fluidRow(" Churn Out Rate Calculator",br(),
               column(12, numericInput(inputId ="salary_hike",label="Please give salary hike", 1900 )),style = "border: 1px solid blue"),
      br(),
      fluidRow("Salary Hike Calculator",br(),
               column(12, numericInput(inputId ="work_experince",label="Please give your years of experience", 1 )),style = "border: 1px solid blue"),
       
      style = "border: 2px solid orange"),
    mainPanel(
      br(),
      # Weight gain
      fluidRow(column(12,  verbatimTextOutput("predict_weight"))),
      tags$style((HTML("#predict_weight{font-size:20px;color:coral;border-radius:4px 4px 4px 4px;border:1px solid blue;}"))),
      
      br(),
      br(),
      br(),
      br(),
      
      # Delivery time
      fluidRow(column(12, verbatimTextOutput("predict_delivery_time"))),
      tags$style((HTML("#predict_delivery_time{font-size:20px;color:coral;border-radius:4px 4px 4px 4px;border:1px solid blue;}"))),
      
      br(),
      br(),
      br(),
      br(),
      
      # Churn out rate
      fluidRow(column(12, verbatimTextOutput("predict_churnout_rate"))),
      tags$style((HTML("#predict_churnout_rate{font-size:20px;color:coral;border-radius:4px 4px 4px 4px;border:1px solid blue;}"))),
      
     
      br(),
      br(),
      br(),
      br(),
      
      # Salary hike
      
      fluidRow(column(12, verbatimTextOutput("predict_salary_hike"))),
      tags$style((HTML("#predict_salary_hike{font-size:20px;color:coral;border-radius:4px 4px 4px 4px;border:1px solid blue;}"))),
      
      style = "border: 2px solid orange"
      
    )
    )
)
server= function(input,output){

  # weight gain and Calorie consumed
  calories_consumed <- reactive({
    nw=data.frame(calories_consumed= input$calorie)  
    nw
    
    
  })
  reg_cal.wt <- reactive({
    cal.wt <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/calories_consumed.csv")
    colnames(cal.wt) <-  c("weight_gained","calories_consumed")
    reg_cal.wt <-lm(weight_gained~calories_consumed, data=cal.wt)
    reg_cal.wt
  })
 
  
  output$predict_weight <- renderText({
    
    
  w=predict(reg_cal.wt(),calories_consumed())
  w=round(w,2)
  w = paste("The weight gained is : ", w ," gm")
  }
  )
  
  # delivery time and sorting time
  sorting_time<- reactive({
  nw=data.frame(sorting_time= input$sort_time)  
  nw

  })
  reg_dvl.st <- reactive({
    dvl.st <-  read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/delivery_time.csv")
    colnames(dvl.st) <- c("delivery_time","sorting_time")
    reg_dvl.st <-lm(delivery_time~sqrt(sorting_time), data=dvl.st)
    reg_dvl.st
  })
  
  
  output$predict_delivery_time <- renderText({
    
    w=predict(reg_dvl.st(),sorting_time())
    w=round(w,2)
    w = paste("The delivery time is : ", w ," min")
  } )
  
  # churn out rate and salary hike
  
  salary_hike<- reactive({
    nw=data.frame(Salary_hike= input$salary_hike)  
    nw
    
  })
  reg_churn_salary <- reactive({
    emp_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/emp_data.csv")
    reg_churn_salary <-lm(Churn_out_rate~log(Salary_hike), data=emp_data)
    reg_churn_salary
  })
  
  
  output$predict_churnout_rate <- renderText({
    
    w=predict(reg_churn_salary(),salary_hike())
    w= round(w,2)
    w = paste("The churn out rate is : ", w ," %")
  } )
  
  # Salary Hike and years of experience
  
  YearsExperience<- reactive({
    nw=data.frame(YearsExperience= input$work_experince)  
    nw
    
  })
  reg_salary_experience <- reactive({
    salary_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/SimpleLinearRegression/Salary_Data.csv")
    reg_salary_experience <-lm(Salary~log(YearsExperience), data=salary_data)
    reg_salary_experience
  })
  
  
  output$predict_salary_hike<- renderText({
    
    w=predict(reg_salary_experience(),YearsExperience())
    w= round(w,2)
    w = paste("The salary hike is : Rs.", w )
  } )
  
  
}
shinyApp(ui= ui,server=server)

