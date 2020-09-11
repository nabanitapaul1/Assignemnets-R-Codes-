
# Importing library
library(dummies)  # This package is for creating dummy variable
library(moments) # This is for statistical analysis like skewness, kurtosis
library(mice) # This is for estimating missing values
library(VIM) # For plot of Missing data
library(corpcor) # This is for co-relation
library(car) # For AV plot & Influence plot
library(heplots) # for eta
library(MASS) # stepAIC

# 1.) Prepare a prediction model for profit of 50_startups data.

startup <- read.csv("D:/EXCELR/ASSIGNMENTS/R/MultiLiearRegression/50_Startups.csv")
colnames(startup) <- c("R&DSpent","Administration","Marketing_Spend","State","Profit")
colnames(startup)
attach(startup)
View(startup)
table(State)
levels(State)
str(startup)

#EDA

startup <- cbind(startup, dummy(startup$State, sep = "_"))
attach(startup)
class(startup)
View(startup)
summary(startup)
str(startup)

#Graphical Representation
par(mfrow=c(1,3)) # dividing the graph window in three parts

#R&DSpent
hist(`R&DSpent`)
boxplot(`R&DSpent`)
qqnorm(`R&DSpent`)
qqline(`R&DSpent`)
skewness(`R&DSpent`)
kurtosis(`R&DSpent`)

#Administartion
hist(Administration)
boxplot(Administration)
qqnorm(Administration)
qqline(Administration)
skewness(Administration)
kurtosis(Administration)

#Marketing_Spend
hist(Marketing_Spend)
boxplot(Marketing_Spend)
qqnorm(Marketing_Spend)
qqline(Marketing_Spend)
skewness(Marketing_Spend)
kurtosis(Marketing_Spend)

#State
barplot(table(startup$State), ylim=c(0, 30), xlab="States", ylab="N", col=c("red","green","yellow"),
        main="Bar diagram of States")

#Profit
hist(Profit)
boxplot(Profit)
qqnorm(Profit)
qqline(Profit)
skewness(Profit)
kurtosis(Profit)

a_num <- data.frame(Administration, Marketing_Spend, Profit, `R&DSpent`)
names(a_num)

par(mfrow=c(1,4)) # divide graph area in 5 columns
for(i in a_num) {
  print(i)
}

# Analysis of missing Value
par(mfrow=c(1,2))
md.pattern(startup)
md.pairs(startup)
aggr_plot <- aggr(startup, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(startup), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Co-relations
cor(startup$`R&DSpent`,startup$Profit)#0.97
plot(startup$`R&DSpent`,startup$Profit) # Linear +ve  Relation
cor(startup$Administration,startup$Profit) #0.2007
plot(startup$Administration,startup$Profit) # No Co-realtion 
cor(startup$Marketing_Spend,startup$Profit) #0.747
plot(startup$Marketing_Spend,startup$Profit) # Linear +ve Relation
#cor(startup$Profit,startup$State)
pairs(startup[,-4])
cor(startup$Marketing_Spend,startup$`R&DSpent`) #0.72
plot(startup$Marketing_Spend,startup$`R&DSpent`)

#partial corelation
cor2pcor(cor(startup[,-4]))
plot(startup)

####### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}

pairs(startup[,-4], upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

cor(startup$`R&DSpent`,startup$Marketing_Spend) #0.72
cor(startup[,-4])

# anova test between profit and States

model.aov <- aov(startup$Profit ~ startup$State, data = startup)
summary(model.aov)# not significant

#In R we dont need to build dummy variable for categorical variable
#Categorical variable should be factor otherewise need to be  converted to 
#factor using"as.factor(variable_name)"
plot(Profit, Administration)

#Model Building

# Model 1
startup_reg <- lm(Profit~.,data=startup)
startup_reg
startup_reg$residuals
startup_reg$coefficients
str(startup_reg)
summary(startup_reg) #R2-value = 0.9508, administration,Marketing_spend,stateflorida, stateNewYork not significant
R2 <- summary(startup_reg)$r.squared 

#Root Mean Square Error
RMSE1 <- sqrt(sum(startup_reg$residuals^2)/nrow(startup))  #RMSE = 8854.761

#vif(startup_reg)

#  AV plot
avPlots(startup_reg, id.n=2,id.cex=0.7)

#identifying influential variable
influence.measures(startup_reg)
influenceIndexPlot(startup_reg, id.n=3) # Index Plots of the influence measures
influencePlot(startup_reg, id.n=3) # A user friendly representation of the above, bubble plot
startup_reg1<-lm(Profit~., data=startup[-c(50)])

summary(startup_reg1)

profit_R <- lm(startup$Profit~startup$`R&DSpent`,data=startup)
summary(profit_R) # R-squared= 0.9465 # significant
profit_ad <- lm(startup$Profit~startup$Administration,data=startup)
summary(profit_ad) # R-squared=0.04029, not significant

profit_mark <-lm(startup$Profit~startup$Marketing_Spend, data=startup)
summary(profit_mark)#R-squared =0.5592 , significant
profit_states <- lm(startup$Profit~startup$State,data = startup)
summary(profit_states) #R-squared = 0.5592, not significant


pred <- predict(startup_reg1)
pred
cor(pred, startup$Profit) # 0.97 correlation between input (predicted value and output variable
plot(pred,startup$Profit)
hist(startup_reg$residuals)
#vif(startup_reg)

#Model 2

startup_reg2 <- lm(startup$Profit~startup$`R&DSpent`+startup$Marketing_Spend+startup$State,data=startup)
summary(startup_reg2) # R-squared = 0.9505
R2_remov_administration <- summary(startup_reg2)$r.squared
#vif((startup_reg2))

#Root Mean Square Error
RMSE2 <- sqrt(sum(startup_reg2$residuals^2)/nrow(startup))  #RMSE = 8881.617

avPlots(startup_reg2, id.n=2,id.cex=0.7)
influence.measures(startup_reg2)
influenceIndexPlot(startup_reg2, id.n=3) # Index Plots of the influence measures
influencePlot(startup_reg2, id.n=3) 
startup_reg22<-lm(startup_reg1<-lm(startup$Profit~startup$`R&DSpent`+startup$Marketing_Spend+startup$State, data=startup[,-c(50)]))
summary(startup_reg22)

# Model between profit and two variables

#Model3
startup_reg3 <- lm(startup$Profit~startup$`R&DSpent`+startup$Marketing_Spend)
summary(startup_reg3) #R-squared = 0.9505
R2_remov_administration_States <- summary(startup_reg3)$r.squared

#Root Mean Square Error
RMSE3 <- sqrt(sum(startup_reg3$residuals^2)/nrow(startup))  #RMSE = 8881.886

avPlots(startup_reg3, id.n=2,id.cex=0.7)

influence.measures(startup_reg3)
influenceIndexPlot(startup_reg3, id.n=3) # Index Plots of the influence measures
influencePlot(startup_reg, id.n=3) # A user friendly representation of the above, bubble plot

startup_reg33<-lm(Profit~ `R&DSpent`+ Marketing_Spend, data=startup[-c(27,48,49,50)], na.action=na.exclude )

summary(startup_reg33)
qqplot(log(startup$Marketing_Spend))
hist(log(startup$`R&DSpent`))
log(startup$Marketing_Spend)
(startup_reg2)
attach(startup)

#Model 4
startup_re_rem_marketing <- lm(Profit~`R&DSpent`+ Administration + State,data=startup)
startup_re_rem_marketing
startup_re_rem_marketing$residuals
startup_re_rem_marketing$coefficients
str(startup_re_rem_marketing)
summary(startup_reg) #R2-value = 0.9508, administration,Marketing_spend,stateflorida, stateNewYork not significant

R4 <- summary(startup_re_rem_marketing)$r.squared 

#Root Mean Square Error
RMSE1 <- sqrt(sum(startup_re_rem_marketing$residuals^2)/nrow(startup))  #RMSE = 8854.761

# Preparing table on model and it's RMSE values 
class(R2)
class(startup_reg2)

model_performance <- data.frame(c("Model1","Model2","Model3"),c(R2,R2_remov_administration,R2_remov_administration_States),c(RMSE1,RMSE2,RMSE3))

colnames(model_performance) <- c("model","R-Squared","RMSE")

View(model_performance)



#2.) Predict Price of a computer

computer_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/MultiLiearRegression/Computer_Data.csv")

computer_data <- computer_data[,-1]
View(computer_data)
summary(computer_data)
colnames(computer_data)
str(computer_data)

# Analysis of missing Value

md.pattern(computer_data)
md.pairs(computer_data)

aggr_plot <- aggr(computer_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(computer_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# No missing data 

# Normalization function to make data unit and scale free
normalize <- function(x) { 
    return((x - min(x)) / (max(x) - min(x)))
  }
 
 

# EDA

computer_data<- as.data.frame(cbind(lapply(computer_data[c(1:5,9,10)], normalize),computer_data[c(6,7,8)]))
View(computer_data)
summary(computer_data)
str(computer_data)
attach(computer_data)
names(computer_data)
levels(cd)
levels(multi)
levels(premium)
par(mfrow=c(1,3))

# Graphical Representation

#price
hist(price)
boxplot(price) # outliers are there
qqnorm(price)
qqline(price)
skewness(price)
kurtosis(price)

#speed
hist(speed)
boxplot(speed)
qqnorm(speed)
qqline(speed)
skewness(speed)
kurtosis(speed)

#hd
hist(hd)
boxplot(hd) #outliers are there
qqnorm(hd)
qqline(hd)
skewness(hd)
kurtosis(hd)
outliers(hd)

#ram
hist(ram)
boxplot(ram) # outliers are there

qqnorm(ram) 
qqline(ram)
skewness(ram)
kurtosis(ram)

#screen
hist(screen)
boxplot(screen)
qqnorm(screen)
qqline(screen)
skewness(screen)
kurtosis(screen)

#ads
hist(ads)
boxplot(ads)
qqnorm(ads)
qqline(ads)
skewness(ads)
kurtosis(ads)

#trend
hist(trend)
boxplot(trend)
qqnorm(trend)
qqline(trend)
skewness(trend)
kurtosis(trend)
#cd
barplot(table(cd), ylim=c(0, 6500), xlab="cd", ylab="N", col=c("blue","green"),
        main="Bar diagram of cd")
#multi
barplot(table(multi), ylim=c(0, 6500), xlab="multi", ylab="N", col=c("blue","green"),
        main="Bar diagram of multi")
#premium
barplot(table(premium), ylim=c(0, 6500), xlab="premium", ylab="N", col=c("blue","green"),
        main="Bar diagram of premium")

# Co-relation
pairs(computer_data)
plots (computer_data)
cor(computer_data[-c(8,9,10),])
computer_data[-c(8,9,10)]

####### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}

pairs(computer_data[-c(8,9,10)], upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

cor(price,speed)#0.300
plot(price,speed)
cor(price,hd) #0.43
plot(price,hd)
cor(price,ram)#0.622
plot(price,ram)
cor(price,screen)#0.296
plot(price,screen)
cor(price,ads) #0.054
plot(price,ads) 
cor(price,trend)#-0.199
plot(price,trend)

# Model Building

# Model 1
computer_data_reg <- lm(price~.,data=computer_data)
computer_data_reg$residuals
computer_data_reg$coefficients
str(computer_data_reg)
summary(computer_data_reg) #R2-value = 0.7756, administration,Marketing_spend,stateflorida, stateNewYork not significant

R2 <- summary(computer_data_reg)$r.squared 

#Root Mean Square Error
RMSE1 <- sqrt(sum(computer_data_reg$residuals^2)/nrow(computer_data))  #RMSE = 0.0618

#vif(computer_data_reg)

#  AV plot
avPlots(computer_data_reg, id.n=2,id.cex=0.7)
#identifying influential variable
influence.measures(computer_data_reg)
influenceIndexPlot(computer_data_reg, id.n=3) # Index Plots of the influence measures
influencePlot(computer_data_reg, id.n=3) # A user friendly representation of the above, bubble plot

computer_data_reg1<-lm(price~., data=computer_data[-c(1701,1401),])
summary(computer_data_reg1)
RMSE2 <- sqrt(sum(computer_data_reg1$residuals^2)/nrow(computer_data))
pred <- predict(computer_data_reg1)

#Squareroot Model
computer_data_reg2<-lm(sqrt(price)~., data=computer_data[-c(1701,1401),])
summary(computer_data_reg2)
RMSE2 <- sqrt(sum(computer_data_reg2$residuals^2)/nrow(computer_data))

computer_data_reg2$coefficients


# 3.) Consider only the below columns and prepare a prediction model for predicting Price.
price_car <- read.csv("D:/EXCELR/ASSIGNMENTS/R/MultiLiearRegression/ToyotaCorolla.csv")


#a <- read.csv("D:/EXCELR/ASSIGNMENTS/R/MultiLiearRegression/ToyotaCorolla.csv")
#View(a)

price_car <- price_car[,c(3,4,7,9,13,14,16,17,18)]
View(price_car)
summary(price_car)
colnames(price_car)
str(price_car)


# Analysis of missing Value
library(mice)
md.pattern(price_car)
md.pairs(price_car)
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(price_car, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(price_car), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# No missing data 

# Normalization function to make data unit and scale free
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

price_car[c(1:9)]


# EDA

price_car<- as.data.frame(lapply(price_car, normalize))

View(price_car)
summary(price_car)
str(price_car)
attach(price_car)
names(price_car)

par(mfrow=c(1,3))
library(moments)
# Graphical Representation
#Price
hist(price_car$Price)
boxplot(price_car$Price) 
qqnorm(price_car$Price)
qqline(price_car$Price)
skewness(price_car$Price)
kurtosis(price_car$Price)

#Age_08_04
hist(price_car$Age_08_04)
boxplot(price_car$Age_08_04)
qqnorm(price_car$Age_08_04)
qqline(price_car$Age_08_04)
skewness(price_car$Age_08_04)
kurtosis(price_car$Age_08_04)
install.packages("outliers")
library(outliers)

#KM
hist(price_car$KM)
boxplot(price_car$KM) #outliers are there
qqnorm(price_car$KM)
qqline(price_car$KM)
skewness(price_car$KM)
kurtosis(price_car$KM)
outliers(price_car$KM)

#HP
hist(price_car$HP)
boxplot(price_car$HP) 
qqnorm(price_car$HP) 
qqline(price_car$HP)
skewness(price_car$HP)
kurtosis(price_car$HP)

#cc
hist(price_car$cc)
boxplot(price_car$cc)
qqnorm(price_car$cc)
qqline(price_car$cc)
skewness(price_car$cc)
kurtosis(price_car$cc)

#Doors
hist(price_car$Doors)
boxplot(price_car$Doors)
qqnorm(price_car$Doors)
qqline(price_car$Doors)
skewness(price_car$Doors)
kurtosis(price_car$Doors)

#Gears
hist(price_car$Gears)
boxplot(price_car$Gears)
qqnorm(price_car$Gears)
qqline(price_car$Gears)
skewness(price_car$Gears)
kurtosis(price_car$Gears)

#Quartely_Tax
hist(price_car$Quarterly_Tax)
boxplot(price_car$Quarterly_Tax)
qqnorm(price_car$Quarterly_Tax)
qqline(price_car$Quarterly_Tax)
skewness(price_car$Quarterly_Tax)
kurtosis(price_car$Quarterly_Tax)

#Weight
hist(price_car$Weight)
boxplot(price_car$Weight)
qqnorm(price_car$Weight)
qqline(price_car$Weight)
skewness(price_car$Weight)
kurtosis(price_car$Weight)


# Co-relation
pairs(price_car)
plot (price_car)

cor(price_car)
####### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}

pairs(price_car, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
colnames(price_car)
cor(Price,Age_08_04)#-0.8765905
plot(Price,Age_08_04)
cor(Price,KM) #-0.5699
plot(Price,KM)
cor(Price,HP)#0.31
plot(Price,HP)
cor(Price,cc)#0.126
plot(Price,cc)
cor(Price,Doors) #0.185
plot(Price,Doors)  
cor(Price,Gears)#-0.0.0631
plot(Price,Gears)
cor(Price,Quarterly_Tax)#-0.2191
plot(Price,Quarterly_Tax)
cor(Price,Weight)#-0.5811
plot(Price,Weight)

# Model Building

# Model 1
price_car_reg <- lm(Price~.,data=price_car)
price_car_reg$residuals
price_car_reg$coefficients
str(price_car_reg)
summary(price_car_reg) #R2-value = 0.8638, doors and cc not significant
R2 <- summary(price_car_reg)$r.squared 

#Root Mean Square Error
RMSE1 <- sqrt(sum(price_car_reg$residuals^2)/nrow(price_car))  #RMSE = 0.04754

names(price_car)
#vif(price_car_reg)

# model 2 (After removing doors variable )


price_data_reg1<-lm(Price~., data=price_car[,-6])
summary(price_data_reg1)
RMSE2 <- sqrt(sum(price_data_reg1$residuals^2)/nrow(price_car))
pred <- predict(price_data_reg1)



#identifying influential variable
influence.measures(price_data_reg1)
influenceIndexPlot(price_data_reg1, id.n=3) # Index Plots of the influence measures
influencePlot(price_data_reg1, id.n=3) # A user friendly representation of the above, bubble plot


# Model3
price_data_reg2<-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, data=price_car[-81,])
               
summary(price_data_reg2)
RMSE2 <- sqrt(sum(price_data_reg2$residuals^2)/nrow(price_car))
price_data_reg2$coefficient                    
R2 <- summary(price_data_reg2)$r.squared 
# AIC
stepAIC(price_data_reg2) # backward


#  AV plot
avPlots(price_data_reg2, id.n=2,id.cex=0.7)



