# 1.) 1) Extract tweets for any user (try choosing a user who has more tweets)
#     2) Perform sentimental analysis on the tweets extracted from the above

# Extracting data from tweeter

install.packages("twitteR") 
library("twitteR")

#autenthetic R and twitter both
install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#Extract the data
install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

# Authenticated 
setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)
?userTimeline
Tweets_AkshayKumar <- userTimeline('akshaykumar', n = 1000,includeRts = T)
class(Tweets_AkshayKumar)


Tweets_Akshay_KumarDF <- twListToDF(Tweets_AkshayKumar)
dim(Tweets_Akshay_KumarDF)
View(Tweets_Akshay_KumarDF)
str(Tweets_Akshay_KumarDF)
summary(Tweets_Akshay_KumarDF)
write.csv(Tweets_Akshay_KumarDF, "Tweets_AkshayKumar.csv",row.names = F)

getwd()

install.packages("rvest")
install.packages("XML")
install.packages("magrittr")
library(rvest)
library(XML)
library(magrittr)

ak_tweet = read.csv("C:/EXCELR/ASSIGNMENTS/TextMining/Tweets_AkshayKumar.csv",header = TRUE)
View(ak_tweet)
str(ak_tweet)
names(ak_tweet)
summary(ak_tweet$text)
length(ak_tweet)
class(ak_tweet$text)


# Corpus
# this package is for text mining
install.packages("tm")
library(tm)

#Converting to corpus
ak_tweet_text <- Corpus(VectorSource(ak_tweet$text))

?tm_map
ak_tweet_text[1]
inspect(ak_tweet_text[1:30])
class(ak_tweet_text)

#Data Cleansing
ak_tweet_text<- tm_map(ak_tweet_text, tolower)
inspect(ak_tweet_text[50:30])

ak_tweet_text <- tm_map(ak_tweet_text, removePunctuation)
inspect(ak_tweet_text[50:30])

ak_tweet_text <- tm_map(ak_tweet_text, removeNumbers)
inspect(ak_tweet_text[50:30])

ak_tweet_text <- tm_map(ak_tweet_text, removeWords, stopwords('english'))
inspect(ak_tweet_text[50:30])
# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
ak_tweet_text <- tm_map(ak_tweet_text, content_transformer(removeURL))
inspect(ak_tweet_text[50:30])

#striping white spaces 
ak_tweet_text <- tm_map(ak_tweet_text, stripWhitespace)
inspect(ak_tweet_text[50:30])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(ak_tweet_text)
tdm
View(tdm)

# document Term matrix
dtm <- t(tdm)
View(tdm)
tdm <- as.matrix(tdm)

tdm[100:109, 1:10]

tdm[90:100, 1:20]

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 10)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Removing menaing less word
list_word <-  c("uffuffbuffuffb","diljitdosanjh\u0085", '\u0085',"ufuffuffb", "\u0092re","uuu", "riteishd")
ak_tweet_text <- tm_map(ak_tweet_text, removeWords, list_word)
ak_tweet_text <- tm_map(ak_tweet_text, stripWhitespace)

tdm2 <- TermDocumentMatrix(ak_tweet_text)
tdm2

tdm2 <- as.matrix(tdm2)
tdm2[90:100, 1:20]

w <- rowSums(tdm2)

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

# wordcloud - shapes for word cloud
library(wordcloud)

y <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(y),freq = y)

wordcloud(words = names(y),freq = y, max.words = 150, random.order = F, min.freq = 5, colors=brewer.pal(8,"Dark2"), scale=c(5,0.3),rot.per = 0.7)

#Sentiment analyis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

ak_tweet = read.csv("C:/EXCELR/ASSIGNMENTS/TextMining/Tweets_AkshayKumar.csv",header = TRUE)
#ak_tweet_text<- iconv(ak_tweet$text,"UTF-8")
ak_tweet_text  <- ak_tweet$text
class(ak_tweet_text)
ak_tweet_text <-  as.character(ak_tweet_text)
# obtain Sentiment Scores
?get_nrc_sentiment
s <-  get_nrc_sentiment(ak_tweet_text)
class(s)
s[16:14,]

ak_tweet_text[16:14]
get_nrc_sentiment("heplpine")


#  bar plot
barplot(colSums(s),las=2, col=rainbow(10), ylab = 'Count',main="Sentiment Score for Akshay Kumar Tweets")

# positive Tweets
sent.value <- get_sentiment(ak_tweet_text)

most.positive <- ak_tweet_text[sent.value == max(sent.value)]

most.positive

# negative tweets
most.negative <- ak_tweet_text[sent.value <= min(sent.value)] 
most.negative 

# segregetting positive and negative tweets and  neutral

 positive.tweets <- ak_tweet_text[sent.value > 0]
 head(positive.tweets)

negative.tweets <- ak_tweet_text[sent.value < 0] 
head(negative.tweets)

neutral.tweets <- ak_tweet_text[sent.value == 0] 
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)

table(category_senti)

barplot(table(category_senti))

#2.) 1) Extract reviews of any product from ecommerce website like snapdeal and amazon
#    2) Perform sentimental analysis


# Snapdeal

# Product Name --- Karbonn Aura
aurl1 <- "https://www.snapdeal.com/product/karbonn-black-aura-1-worlds/663390882770/reviews?page"
aurl2 <- "&sortBy=HELPFUL#defRevPDP"
snap_deal_reviews <- NULL
for (i in 1:20){
  print(i)
  a_link <- as.character(paste(aurl1,i,sep="="))
  a_link <- paste(a_link,aurl2,sep="")
  print(a_link)
  murl <- read_html(a_link)
  rev <- murl %>%
    html_nodes(".head") %>%
    html_text(".head")
  snap_deal_reviews <- c(snap_deal_reviews,rev)
}
length(snap_deal_reviews)

write_csv <- write.csv(snap_deal_reviews, "karbonn_aura.csv" )
getwd()

krabonn_aura = read.csv("C:/EXCELR/ASSIGNMENTS/TextMining/karbonn_aura.csv",header = TRUE)
View(krabonn_aura)
str(krabonn_aura)
names(krabonn_aura)
summary(krabonn_aura$x)
length(krabonn_aura$x)
class(krabonn_aura$x)

# Corpus
# this package is for text mining
install.packages("tm")
library(tm)

#Converting to corpus
karbonn_aura_text <- Corpus(VectorSource(krabonn_aura$x))

?tm_map
inspect(karbonn_aura_text[1:30])
class(karbonn_aura_text)

#Data Cleansing
karbonn_aura_text<- tm_map(karbonn_aura_text, tolower)
inspect(karbonn_aura_text[50:30])

karbonn_aura_text <- tm_map(karbonn_aura_text, removePunctuation)
inspect(karbonn_aura_text[50:30])

karbonn_aura_text <- tm_map(karbonn_aura_text, removeNumbers)
inspect(karbonn_aura_text[50:30])

karbonn_aura_text <- tm_map(karbonn_aura_text, removeWords, stopwords('english'))
inspect(karbonn_aura_text[50:30])

# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
karbonn_aura_text <- tm_map(karbonn_aura_text, content_transformer(removeURL))
inspect(karbonn_aura_text[50:30])

#striping white spaces 
karbonn_aura_text <- tm_map(karbonn_aura_text, stripWhitespace)
inspect(karbonn_aura_text[50:30])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(karbonn_aura_text)
tdm
View(tdm)

# Document term matrix
dtm <- t(tdm)
View(dtm)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]
tdm[90:100, 1:20]

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 5)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Removing meanigful less word
list_word <-c(stopwords('english'), "review","positive","karbonn","aura")

karbonn_aura_text <- tm_map(karbonn_aura_text,removeWords, list_word)
karbonn_aura_text <- tm_map(karbonn_aura_text, stripWhitespace)
inspect(karbonn_aura_text)

tdm2 <- TermDocumentMatrix(karbonn_aura_text)
tdm2

dtm2 <- t(tdm2)
dm2 <- as.matrix(tdm2)
tdm2[100:109, 1:10]

tdm2 <- as.matrix(tdm2)
tdm2[90:100, 1:20]
w <- rowSums(tdm2)
w

w_sub <- subset(w, w >= 5)
w_sub

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

barplot(w_sub, las=2, col = rainbow(30))

# wordcloud - shapes for word cloud
library(wordcloud)
y <- sort(rowSums(tdm2),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(y),freq = y)

wordcloud(words = names(y),freq = y, max.words = 150, random.order = F, min.freq = 3, colors=brewer.pal(8,"Dark2"), scale=c(5,0.3),rot.per = 0.7)

#Sentiment analyis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

karbonn_aura = read.csv("D:/EXCELR/ASSIGNMENTS/R/TextMining/karbonn_aura.csv",header = TRUE)
karbonn_aura_review  <- karbonn_aura$x
class(karbonn_aura_review)
karbonn_aura_review <-  as.character(karbonn_aura_review)

# obtain Sentiment Scores
s <-  get_nrc_sentiment(karbonn_aura_review)
class(s)
s[6:14,]

#  bar plot
barplot(colSums(s),las=2, col=rainbow(10), ylab = 'Count',main="Sentiment Score for Reviews of Karbonn Aura")

# positive reviews
sent.value <- get_sentiment(karbonn_aura_review)

most.positive <- karbonn_aura_review[sent.value == max(sent.value)]
most.positive

# negative reviews
most.negative <- karbonn_aura_review[sent.value <= min(sent.value)] 
most.negative 

# segregetting positive and negative reviews and  neutral

positive.reviews <- karbonn_aura_review[sent.value > 0]
head(positive.reviews)

negative.reviews <- karbonn_aura_review[sent.value < 0] 
head(negative.reviews)

neutral.reviews <- karbonn_aura_review[sent.value == 0] 
head(neutral.reviews)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)

table(category_senti)

barplot(table(category_senti))

#### Amazon Reviews####################

# Product Name : - Canon EOS 1500D
aurl <- "https://www.amazon.in/Canon-1500D-Digital-Camera-S18-55/product-reviews/B07BS4TJ43/ref=cm_cr_getr_d_paging_btm_prev_1?pageNumber"
amazon_reviews <- NULL
for (i in 1:20){
  
  a_link <- as.character(paste(aurl,i,sep="="))
  murl <- read_html(a_link)
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
  
}
length(amazon_reviews)
write_csv <- write.csv(amazon_reviews,"canon_camera.csv")
getwd()

# Sentimental Analysis

cannon_camera = read.csv("D:/EXCELR/ASSIGNMENTS/R/TextMining/canon_camera.csv",header = TRUE)
View(cannon_camera)
str(cannon_camera)
names(cannon_camera)
summary(cannon_camera$x)
length(cannon_camera$x)
class(cannon_camera$x)

# Corpus
# this package is for text mining

library(tm)

#Converting to corpus
cannon_camera_review <- Corpus(VectorSource(cannon_camera$x))
cannon_camera_review[1]
inspect(cannon_camera_review[1:30])
class(cannon_camera_review)

#Data Cleansing
cannon_camera_review<- tm_map(cannon_camera_review, tolower)
inspect(cannon_camera_review[50:30])

cannon_camera_review <- tm_map(cannon_camera_review, removePunctuation)
inspect(cannon_camera_review[50:30])

cannon_camera_review <- tm_map(cannon_camera_review, removeNumbers)
inspect(cannon_camera_review[50:30])

cannon_camera_review <- tm_map(cannon_camera_review, removeWords, stopwords('english'))
inspect(cannon_camera_review[50:30])

# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
cannon_camera_review <- tm_map(cannon_camera_review, content_transformer(removeURL))
inspect(cannon_camera_review[50:30])

#striping white spaces 
cannon_camera_review <- tm_map(cannon_camera_review, stripWhitespace)
inspect(cannon_camera_review[50:30])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(cannon_camera_review)
tdm
View(tdm)

# Document Term matrix
dtm <- t(tdm)
View(tdm)
tdm <- as.matrix(tdm)

tdm[100:109, 1:10]

tdm[90:100, 1:20]

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 10)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Removing meanigful less word
list_word <-c(stopwords('english'), "canon","amazon")

cannon_camera_review <- tm_map(cannon_camera_review,removeWords, list_word)
cannon_camera_review <- tm_map(cannon_camera_review, stripWhitespace)
inspect(cannon_camera_review)
tdm2 <- TermDocumentMatrix(cannon_camera_review)
tdm2

dtm2 <- t(tdm2)
dm2 <- as.matrix(tdm2)
tdm2[100:109, 1:10]

tdm2 <- as.matrix(tdm2)
tdm2[90:100, 1:20]
w <- rowSums(tdm2)
w

w_sub <- subset(w, w >= 10)
w_sub
barplot(w_sub, las=2, col = rainbow(30))

w <- rowSums(tdm2)
w

w_small <- subset(w, w >= 25)
w_small

barplot(w_small, las=2, col = rainbow(30))

barplot(w_sub, las=2, col = rainbow(30))

y <- sort(rowSums(tdm2),decreasing = TRUE)

# wordcloud - shapes for word cloud
library(wordcloud)

set.seed(222)
wordcloud(words = names(y),freq = y)

wordcloud(words = names(y),freq = y,  random.order = F, min.freq = 15, colors=brewer.pal(8,"Dark2"), scale=c(5,0.3),rot.per = 0.7)

#Sentiment analyis

library(syuzhet)
library(lubridate)
library(ggplot2)

library(scales)
library(reshape2)
library(dplyr)

canon_camera = read.csv("D:/EXCELR/ASSIGNMENTS/R/TextMining/canon_camera.csv",header = TRUE)
canon_camera_review  <- canon_camera$x
class(canon_camera)
canon_camera_review <-  as.character(canon_camera_review)

# obtain Sentiment Scores
s <-  get_nrc_sentiment(canon_camera_review)
class(s)

s[6:14,]

#  bar plot
barplot(colSums(s),las=2, col=rainbow(10), ylab = 'Count',main="Sentiment Score for Reviews of Canon Camera")

# positive reviews
sent.value <- get_sentiment(canon_camera_review)
most.positive <- canon_camera_review[sent.value == max(sent.value)]
most.positive

# negative reviews
most.negative <- canon_camera_review[sent.value <= min(sent.value)] 
most.negative 

# segregetting positive and negative reviews and  neutral

positive.reviews <- canon_camera_review[sent.value > 0]
head(positive.reviews)

negative.reviews <- canon_camera_review[sent.value < 0] 
head(negative.reviews)

neutral.reviews <- canon_camera_review[sent.value == 0] 
head(neutral.reviews)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)

table(category_senti)

barplot(table(category_senti))


#3.)  Extract movie reviews for any movie from IMDB and perform sentimental analysis

#  Extracting reviews from IMDB
#Movie   Name =  "Frozen"

library(rvest)
library(magrittr)
library(XML)

imdb_reviews<-NULL
aurl<-"https://www.imdb.com/title/tt1323045/reviews?ref_=tt_ql_3"

url<-read_html(aurl)
ping<-url %>%
    html_nodes(".show-more__control") %>%
    html_text() 
imdb_reviews<-c(imdb_reviews,ping)
length(imdb_reviews)

ping1<-url %>%
  html_nodes(".title") %>%
  html_text() 
imdb_reviews<-c(imdb_reviews,ping1)

length(imdb_reviews)
imdb_reviews[76]
write.csv(imdb_reviews,file="Frozen.csv")
getwd()

#Importing dataset

frozen <- read.csv("D:/EXCELR/ASSIGNMENTS/R/TextMining/Frozen.csv")
View(frozen)
str(frozen)
summary(frozen)

# Data preprocessing
# this package is for text mining

install.packages("tm")
library(tm)

#Converting to corpus
frozen_review <- Corpus(VectorSource(frozen$x))

frozen_review[1]
inspect(frozen_review[1:30])
class(frozen_review)

#Data Cleansing
frozen_review<- tm_map(frozen_review, tolower)
inspect(frozen_review[50:30])

frozen_review <- tm_map(frozen_review, removePunctuation)
inspect(frozen_review[50:30])

frozen_review <- tm_map(frozen_review, removeNumbers)
inspect(frozen_review[50:30])

frozen_review <- tm_map(frozen_review, removeWords, stopwords('english'))
inspect(frozen_review[50:30])

# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
frozen_review <- tm_map(frozen_review, content_transformer(removeURL))
inspect(frozen_review[50:30])

#striping white spaces 
frozen_review <- tm_map(frozen_review, stripWhitespace)
inspect(karbonn_aura_text[50:30])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(frozen_review)
tdm
View(tdm)

# Document term matrix
dtm <- t(tdm)
View(dtm)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 10)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

library(wordcloud)

y <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(y),freq = y, colors=brewer.pal(8,"Dark2"), scale=c(5,0.3),rot.per = 0.7)

wordcloud(words = names(y),freq = y, max.words = 200, random.order = F, min.freq = 3, colors=brewer.pal(8,"Dark2"), scale=c(5,0.3),rot.per = 0.7)

#Sentiment analyis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

forzen = read.csv("D:/EXCELR/ASSIGNMENTS/R/TextMining/Frozen.csv",header = TRUE)
frozen_review  <- frozen$x
class(frozen_review)
frozen_review <-  as.character(frozen_review)

# obtain Sentiment Scores
s <-  get_nrc_sentiment(frozen_review)
s[14:16,]

#  bar plot
barplot(colSums(s),las=2, col=rainbow(10), ylab = 'Count',main="Sentiment Score for Reviews of Frozen Movie")

# positive reviews
sent.value <- get_sentiment(frozen_review)
most.positive <- frozen_review[sent.value == max(sent.value)]
most.positive

# negative reviews
most.negative <- frozen_review[sent.value <= min(sent.value)] 
most.negative 

# segregeting positive and negative reviews and  neutral

positive.reviews <- frozen_review[sent.value > 0]
head(positive.reviews)

negative.reviews <- frozen_review[sent.value < 0] 
head(negative.reviews)

neutral.reviews <- frozen_review[sent.value == 0] 
head(neutral.reviews)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)

table(category_senti)

barplot(table(category_senti))

#2.)  Extract anything you choose from the internet and do some research on how we extract using R
#     Programming and perform sentimental analysis.

# Getting Youtube data


library(vosonSML)

# youtube authentication sets the api key

youtubeAuth <- Authenticate("youtube", apiKey = "xxxxxxxxxxxxxxxx") # put your own google developer key

#  to create a list of youtube video ids from urls
youtubeVideoIds <- GetYoutubeVideoIDs("https://www.youtube.com/watch?v=ii7obeNa20g")  

#Video Name= Chicago Speech of Swami Vivekananda at the World Parliament of Religions for ¦ RSM ¦ SRM ¦

youtubeData <- youtubeAuth %>%
  Collect(videoIDs = youtubeVideoIds,
          maxComments = 100,
          verbose = FALSE)

write.csv(youtubeData,"Vivekananda chicago speech.csv")
activityNetwork <- youtubeData %>% Create("activity") %>% Graph() %>% summary()
View(youtubeData)
class(youtubeData$Comment)
str(youtubeData$Comment)
length(youtubeData$Comment)

#Data Preprocessing

swami_vivekananda_comment <- youtubeData$Comment
length(swami_vivekananda_comment)

# this package is for text mining
install.packages("tm")
library(tm)

#Converting to corpus
swami_vivekananda_comment <- Corpus(VectorSource(swami_vivekananda_comment))
swami_vivekananda_comment[1]
inspect(swami_vivekananda_comment[1:30])
class(swami_vivekananda_comment)

#Data Cleansing
swami_vivekananda_comment<- tm_map(swami_vivekananda_comment, tolower)
inspect(swami_vivekananda_comment[50:30])

swami_vivekananda_comment <- tm_map(swami_vivekananda_comment, removePunctuation)
inspect(swami_vivekananda_comment[50:30])

swami_vivekananda_comment <- tm_map(swami_vivekananda_comment, removeNumbers)
inspect(swami_vivekananda_comment[50:30])

swami_vivekananda_comment <- tm_map(swami_vivekananda_comment, removeWords, stopwords('english'))
inspect(swami_vivekananda_comment[50:30])

# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
swami_vivekananda_comment <- tm_map(swami_vivekananda_comment, content_transformer(removeURL))
inspect(swami_vivekananda_comment[50:30])

#striping white spaces 
swami_vivekananda_comment <- tm_map(swami_vivekananda_comment, stripWhitespace)
inspect(swami_vivekananda_comment[50:30])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(swami_vivekananda_comment)
tdm
View(tdm)

# Document term matrix
dtm <- t(tdm)
View(dtm)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 10)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

library(wordcloud)
y <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(y),freq = y)

wordcloud(words = names(y),freq = y, max.words = 250, random.order = F, min.freq = 2, colors=brewer.pal(8,"Dark2"), scale=c(5,0.3),rot.per = 0.7)

#Sentiment analyis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# obtain Sentiment Scores
?get_nrc_sentiment
class(swami_vivekananda_comment)

swami_vivekananda_comment <- youtubeData$Comment
s <-  get_nrc_sentiment(swami_vivekananda_comment)
class(swami_vivekananda_comment)
s[6:8,]

#  bar plot
barplot(colSums(s),las=2, col=rainbow(10), ylab = 'Count',main="Sentiment Score for Swvievekananda Video Comments")

# positive comments
sent.value <- get_sentiment(swami_vivekananda_comment)

most.positive <- swami_vivekananda_comment[sent.value == max(sent.value)]

most.positive

# negative comments
most.negative <- swami_vivekananda_comment[sent.value <= min(sent.value)] 
most.negative 


# segregetting positive and negative tweets and  neutral

positive.comments <- swami_vivekananda_comment[sent.value > 0]
head(positive.comments)

negative.comments<- swami_vivekananda_comment[sent.value < 0] 
head(negative.comments)

neutral.comments <- swami_vivekananda_comment[sent.value == 0] 
head(neutral.comments)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)


table(category_senti)


barplot(table(category_senti))


