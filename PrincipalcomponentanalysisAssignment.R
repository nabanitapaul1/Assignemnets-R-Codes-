#1.) Perform Principal component analysis and perform clustering using first 
# principal component scores (both hierarchical and k mean clustering(scree plot or elbow curve) and obtain 
#                              optimum number of clusters and check whether we have obtained same number of clusters with the original data 
#                             (class column we have ignored at the beginning who shows it has 3 clusters)df

# Import libraray
library(cluster)
library(animation)


# Importing Dataset

wine_data_main = read.csv("D:/EXCELR/ASSIGNMENTS/R/PCA/wine.csv")

View(wine_data_main)
colnames(wine_data_main)

## the first column in my data has university names

# mydata[-1] -> Considering only numerical values for applying PCA
wine_data <- wine_data_main[-1]
View(wine_data)
attach(wine_data)
summary(wine_data)
str(wine_data)

cor(wine_data)

# Applying principalComponenet Analysis

pcaObj_wine<-princomp(wine_data,cor = TRUE,scores = TRUE, covmat=NULL)

## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different

summary(pcaObj_wine)
str(pcaObj_wine)

plot(pcaObj_wine) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj_wine)

View(pcaObj_wine)
View(pcaObj_wine$scores)
pcaObj_wine$scores[,1:3] # Top 3 PCA Scores which represents the whole data
View(pcaObj_wine$scores[,1:3])

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata

wine_data<-cbind(wine_data,pcaObj_wine$scores[,1:3])
View(wine_data)
colnames(wine_data)

# Clustering

# preparing data for clustering (considering only pca scores as they represent the entire data)


wine_clus_data<-wine_data[,14:16]
colnames(wine_clus_data)

# Normalizing the data 
wine_norm_clus<-scale(wine_clus_data) # Scale function is used to normalize data
summary(wine_norm_clus)

dist1<-dist(wine_norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

#Hierarchical
# Clustering the data using hclust function --> Hierarchical

fit1<-hclust(dist1,method="complete") # method here is complete linkage


plot(fit1) # Displaying Dendrogram
plot(fit1, hang=-1)

groups <- cutree(fit1, k=4) # cut tree into 5 clusters

rect.hclust(fit1, k=4, border="red")
membership_1<-as.matrix(groups) # cluster numbering 
View(membership_1)

final1<-cbind(membership_1,wine_data) # binding column wise with orginal data
View(final1)
names(final1)
aggregate <- aggregate(final1,by=list(membership_1),FUN=mean) # Inferences can be
# drawn from the aggregate of the wine data
aggregate
write.csv(final1,file="Wine_clus_pcs.csv",row.names = F,col.names = F)
getwd()

# K-means

# Normalizing the data 


wine_norm<-scale(wine_data[,14:16]) # Scale function is used to normalize data
View(wine_norm)

#elbow curve & k ~ sqrt(n/2) to decide the k value
#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 2:15) twss[i] = sum(kmeans(wine_norm, centers=i)$withinss)
plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km$withinss
View(wine_norm_clus)

fit <- kmeans.ani(wine_norm, 4) # 4 cluster solution
str(fit)
final2<- data.frame(wine_data, fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
aggregate_wine_data <- aggregate(wine_data, by=list(fit$cluster), FUN=mean)
aggregate_wine_data
View(aggregate_wine_data)
names(aggregate_wine_data)
names(wine_data)

# Clustering  of original data

View(wine_data_main) 

# Normalization

#elbow curve & k ~ sqrt(n/2) to decide the k value
wine_norm<-scale(wine_data_main[-1]) # Scale function is used to normalize data
View(wine_norm)


#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 2:15) twss[i] = sum(kmeans(wine_norm, centers=i)$withinss)
plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km$withinss

fit <- kmeans.ani(wine_norm, 4) # 4 cluster solution
str(fit)
final2<- data.frame(wine_data_main[-1], fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
aggregate_wine_data <- aggregate(wine_data_main[-1], by=list(fit$cluster), FUN=mean)
aggregate_wine_data
View(aggregate_wine_data)
names(aggregate_wine_data)

