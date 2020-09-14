# Importing library

library(animation)
library(xlsx)
library(data.table)

# 1.) Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

# Import Data
crime_data <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Clustering/crime_data.csv")
View(crime_data)

# EDA
summary(crime_data)

# Normalization
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

normalized_crime_data <- as.data.frame(lapply(crime_data[,2:5], normalize))
summary(normalized_crime_data)

# applying k-means alogorithm
fit_crime_data <- kmeans.ani(normalized_crime_data,5)
summary(fit_crime_data)
str(fit_crime_data)
fit_crime_data$cluster
fit_crime_data$centers
membership_crime_data<- data.frame(crime_data, fit_crime_data$cluster) # append cluster membership
View(membership_crime_data_f)
membership_crime_data_f <- membership_crime_data[,c(ncol(membership_crime_data),1:(ncol(membership_crime_data)-1))]
aggregate(crime_data[,2:5], by=list(fit_crime_data$cluster), FUN=mean)

#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 2:15) twss[i] = sum(kmeans(normalized_crime_data, centers=i)$withinss)
plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km$withinss

fit_crime_data <- kmeans.ani(normalized_crime_data,6)
summary(fit_crime_data)
str(fit_crime_data)
fit_crime_data$cluster
fit_crime_data$centers
membership_crime_data<- data.frame(crime_data, fit_crime_data$cluster) # append cluster membership
View(membership_crime_data_f)
membership_crime_data_f <- membership_crime_data[,c(ncol(membership_crime_data),1:(ncol(membership_crime_data)-1))]
aggregate(crime_data[,2:5], by=list(fit_crime_data$cluster), FUN=mean)




#2.) Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 

# Importing data
airlines_data <- read.xlsx("D:/EXCELR/ASSIGNMENTS/R/Clustering/EastWestAirlines.xlsx",2)
View(airlines_data)

# EDA
colnames(airlines_data)
summary(airlines_data)

# Normalization
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

normalized_airlines_data <- as.data.frame(lapply(airlines_data[,2:11], normalize))
summary(normalized_airlines_data)

# hierarchical Clustering

d <- dist(normalized_airlines_data, method = "euclidean")# distance matrix

fit <- hclust(d, method="complete")
summary(fit)


plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=30) # cut tree into 30 clusters


rect.hclust(fit, k=30, border="red")


membership<-as.matrix(groups)

final <- data.frame(airlines_data, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
aggregate(airlines_data[,2:11], by=list(final$membership), FUN=mean)

attach(final)

#K-means

#Elbow chart

twss = c()
for (i in 2:30) twss[i] = sum(kmeans(normalized_airlines_data, centers=i)$withinss)
plot(1:30, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km$withinss

fit <- kmeans.ani(normalized_airlines_data, 7) # 7 cluster solution
str(fit)
final2<- data.frame(normalized_airlines_data, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(airlines_data[,2:11], by=list(fit$cluster), FUN=mean)





