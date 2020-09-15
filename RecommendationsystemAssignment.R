# Import library

install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
install.packages("caTools")

library("recommenderlab")
library(caTools)


# 1.) Problem statement.
#     Recommend a best book based on the author, publisher and ratings.


# books rating data
book_rate_data <-  read.csv("D:/EXCELR/ASSIGNMENTS/R/RecommendationSystem/Books.csv")
View(book_rate_data)
book_rate_data <- book_rate_data[c(2,3,4)]

sum(is.na(book_rate_data$Book.Rating))

#meta data about the variables
str(book_rate_data)
summary(book_rate_data)
View(book_rate_data)
colnames(book_rate_data)
levels(book_rate_data$Book.Title)
names(book_rate_data)

# rating distribution
hist(book_rate_data$Book.Rating)

# the data type should be real rating matrix inorder to build recomendation engine
book_rate_data_matrix <- as(book_rate_data,'realRatingMatrix')
summary(book_rate_data_matrix)
length(book_rate_data_matrix)
View(book_rate_data_matrix)

# Recommendation

#Popularity Based
book_recomend_model_popular <- Recommender(book_rate_data_matrix, method='POPULAR')

# Predictions
recommended_items1 <- predict(book_recomend_model_popular, book_rate_data_matrix[1166], n=5)
recommended_items1 <- predict(book_recomend_model_popular, book_rate_data_matrix[1100], n=5)
recommended_items1 <- predict(book_recomend_model_popular, book_rate_data_matrix[1326], n=5)
recommended_items1 <- predict(book_recomend_model_popular, book_rate_data_matrix[1467], n=5)
as(recommended_items1, 'list')

#  Popularity model recommends same movies to all users # we need to improve our model using collaborative Filtering

# User Based Collaborative Filtering

 book_recomend_model_UBCF <- Recommender(book_rate_data_matrix, method = 'UBCF')
 
# Predictions
recommended_items2 <- predict(book_recomend_model_UBCF, book_rate_data_matrix[50:100], n=5)
a_list <-as(recommended_items2,'list') 

 
 