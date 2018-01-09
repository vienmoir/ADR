setwd("~/GitHub/ADR")
all = read.csv("all_reviews_cleared.csv", stringsAsFactors = FALSE)
all$adr = as.factor(all$adr)

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

### classifier adr dictionary
setwd("~/")
adrs = read.csv("adr_matrix.csv", stringsAsFactors = FALSE)
library(RTextTools)
container <- create_container(adrs, all$adr, trainSize=1:900,testSize=901:1208, virgin=FALSE)
model <- train_model(container, 'TREE',kernel='linear')
results <- classify_model(container, model)
table(as.character(all$adr[901:1208]), as.character(results[,"TREE_LABEL"]))
all_results = data.frame(results$TREE_LABEL)
colnames(all_results) = "adr"

### classifier td idf
mat <- as.matrix(review_dtm_tfidf)
container <- create_container(mat, all$adr, trainSize=1:900,testSize=901:1208, virgin=FALSE)
model <- train_model(container, 'TREE',kernel='linear')
results <- classify_model(container, model)
table(as.character(all$adr[901:1208]), as.character(results[,"TREE_LABEL"]))
all_results$tdidf = results$TREE_LABEL

### classifier sentiment
m = read.csv("sentiment_scores.csv", stringsAsFactors = FALSE)
m$ys = all$adr
library(nnet)
model = nnet(ys ~ sum, data = m, size=10)
p = predict(model, m[901:1208,], type="class")
table(all$adr[901:1208], p)
all_results$sentiment = p

colnames(all_results) = c("adr","tdidf","sentiment")

all_results$adr = ifelse(all_results$adr == 1, 0.8, 0.2)
all_results$tdidf = ifelse(all_results$tdidf == 1, 0.7, 0.3)
all_results$sentiment = ifelse(all_results$sentiment == 1, 0.7, 0.3)

all_results$final = (all_results$adr + all_results$tdidf + all_results$sentiment) / 3
all_results$final = ifelse(all_results$final > 0.5, 1, 0)

table(all$adr[901:1208], all_results$final)
