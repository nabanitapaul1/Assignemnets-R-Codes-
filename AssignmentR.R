# Hypothesis Assignment

# 1.)
cutlet <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Hypothesis Testing/Cutlets.csv")
cutlet
View(cutlet)
attach(cutlet)

# normality Test
shapiro.test(Unit.A)
hist(Unit.A)
shapiro.test(Unit.B)
hist(Unit.B)

# Paired T-test

t.test(Unit.A, Unit.B, alternative = "two.sided", conf.level =0.95, paired = TRUE)

# 2.)
TAT  <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Hypothesis Testing/LabTAT.csv")
TAT
View(TAT)
attach(TAT)
#normality Test
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)

stacked_TAT<- stack(TAT)
View(stacked_TAT)
# varience Test
library(car)
?leveneTest
leveneTest(values~ind, data=stacked_TAT)

# One way Anova Test
?aov
Anova_TAT  <- aov(values~ind,data=stacked_TAT)
summary(Anova_TAT)

# 3.)

buyer_ratio <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Hypothesis Testing/BuyerRatio.csv")
View(buyer_ratio)
attach(buyer_ratio)
# Chi-Square Test

table(Observed.Values) #  Count will be shown for all countries.  
# Comparison between two values amoung countries


#create a data frame
dat <- data.frame(male=c(50,142,131,70),
                  female=c(435,1523,1356,750),row.names =c("East","West","North","South"))



a_data <- data.frame(males=c(50,142,131,70),
                     females=c(435,1523,1356,750),row.names=names(buyer_ratio)[-1])

str(dat)

values(buyer_ratio)
buyer_ratio[1,-1]
buyer_ratio[2,-1]

chisq.test(a_data)

pchisq(1.5959,3)
1-pchisq(1.5959,3)

# 4.)

library("plyr")

customer_order_form <-read.csv("D:/EXCELR/ASSIGNMENTS/R/Hypothesis Testing/Costomer+OrderForm.csv") 
View(customer_order_form)
attach(customer_order_form)
prop.table(customer_order_form)

class(customer_order_form)
table(customer_order_form)
#table(customer_order_form$Phillippines)
colnames(customer_order_form)

# Main Code

library(MASS)
cus_ord <- apply(customer_order_form ,2 ,table)

class(cus_ord)
chisq.test(cus_ord)



# 5.) 

fantaloons <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Hypothesis Testing/Faltoons.csv")
View(fantaloons)
attach(fantaloons)
table_wkdays <- table(Weekdays) # take unique value and returns count for the unique values
table_wkend <- table(Weekend)
fant <- apply(fantaloons , 2,table)

table_wkdays
table_wkend
fant

#case 1
prop.test(x=c(167,233),n=c(280,520), conf.level = 0.95,correct = TRUE, alternative = "two.sided")
prop.test(x=c(113,287),n=c(280,520), conf.level = 0.95,correct = TRUE, alternative = "two.sided")

#Case 2
prop.test(x=c(113,287),n=c(280,520), conf.level = 0.95,correct = TRUE, alternative = "less")

