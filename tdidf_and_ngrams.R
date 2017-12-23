setwd("~/GitHub/ADR")
all = read.csv("cleared_all_reviews.csv", stringsAsFactors = FALSE)
all$X = NULL

#replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
#docs <- tm_map(docs, replacePunctuation)

library(tm)
docs <- Corpus(VectorSource((all$review)))
replacePunctuation <- content_transformer(function(x) {return (gsub("[^a-zA-Zà-ÿÀ-ß]+", " ", x))})
docs <- tm_map(docs, replacePunctuation)
docs <- tm_map(docs, removeWords, stopwords("russian"))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
library(SnowballC)
docs <- tm_map(docs, stemDocument, language = "russian")
docs <- tm_map(docs, stemDocument, language = "english")

# tf-idf
review_dtm_tfidf <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))
#review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf
inspect(review_dtm_tfidf[1,1:100])

# tokenize into 2,3,4-grams
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer, weighting = weightTfIdf))

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.trigram <- TermDocumentMatrix(docs, control = list(tokenize = TrigramTokenizer, weighting = weightTfIdf))

FourTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm.fourgram <- TermDocumentMatrix(docs, control = list(tokenize = FourTokenizer, weighting = weightTfIdf))

# most popular 
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

inspect(tdm.fourgram)
