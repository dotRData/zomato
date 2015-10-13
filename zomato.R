setwd('/Users/ranand/Desktop/zomato')
library("gridExtra")
source('API.R')

clean_corpus <- function(x){
  corp <- Corpus(VectorSource(x))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, removeWords, c(stopwords("en"),'nbsp'))
  corp <- tm_map(corp, content_transformer(removeNumbers))
  corp <- tm_map(corp, content_transformer(removePunctuation))
  corp <- tm_map(corp, stripWhitespace)
  corp
}

clean_data <- function(x){
  x <- tolower(x)
  x <- str_replace_all(x,"\n", ' ')
  x <- str_replace_all(x,"\r", ' ')
  x
}

word_cloud <- function(data, rating = 0, freq = 2, words = 30){
  rev_data  <- c()
  rate_data <- c()
  
  for(i in 1:length(data$user_reviews)){
    rate_data <- append(rate_data, data$user_reviews[[i]]$review$rating)
    if(data$user_reviews[[i]]$review$rating >= rating){
      rev_data <- append(rev_data,clean_data(data$user_reviews[[i]]$review$review_text))
    }
  }

  print(rev_data)
  corpus <- clean_corpus(rev_data)

  par(mfrow=c(2,1))  
  wordcloud(corpus, min.freq = freq, max.words = words, colors=brewer.pal(7, "Dark2"))
  hist(rate_data,breaks=6, main= "Rating Data")
}

resturant_id = 16774318
reviews_data <- reviews(resturant_id, count = 100)
word_cloud(reviews_data, rating=0, freq = 1000, words=10)




