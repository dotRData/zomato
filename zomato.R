setwd('/Users/ranand/Desktop/zomato')

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

word_cloud <- function(data, rating, min.freq = 2, max.words=30){
  rev_data <- c()
  
  for(i in 1:length(data$user_reviews)){
    if(data$user_reviews[[i]]$review$rating >= rating){
      rev_data <- append(rev_data,str_replace_all(tolower(data$user_reviews[[i]]$review$review_text),"\n", ' '))
    }
  }
  print(rev_data)
  corp <- clean_corpus(rev_data)
  wordcloud(corp, scale=c(4,0.5), min.freq = 2, max.words=30,colors=brewer.pal(8, "Dark2"))
}

data <- reviews(resturant_id = 16774318)
word_cloud(data, 1)

