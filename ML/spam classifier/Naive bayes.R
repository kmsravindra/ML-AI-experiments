library(tm)
library(magrittr)

setwd("C:/Users/kmsra_000/Documents/Machine_Learning/spam_classification/")
df = read.csv("Youtube01-Psy.csv", , stringsAsFactors=F)
str(df)

head(df)
summary(df)
unique(df$CLASS)
nrow(df)

df$COMMENT_ID <- NULL
df$AUTHOR <- NULL
df$DATE <- NULL

dim(df)

library(data.table)
df <- as.data.table(df)
df[,.N/nrow(df),by=CLASS]
# 50% is spam and 50% is not spam

df$CLASS <- as.factor(df$CLASS)

content_corpus = Corpus(VectorSource(df$CONTENT))
inspect(content_corpus[1:3])

getTransformations()

corpus_clean = tm_map(content_corpus, tolower)                # convert to lower case
corpus_clean = tm_map(corpus_clean, removeNumbers)            # remove digits
corpus_clean = tm_map(corpus_clean, removeWords, stopwords()) # and but or you etc
corpus_clean = tm_map(corpus_clean, removePunctuation)        # you'll never guess!
corpus_clean = tm_map(corpus_clean, stripWhitespace)          # reduces w/s to 1
corpus_clean = tm_map(corpus_clean, stemDocument)          # reduces w/s to 1

inspect(corpus_clean[1:3])

#corpus_clean = tm_map(corpus_clean, PlainTextDocument) # this is a tm API necessity
dtm = DocumentTermMatrix(corpus_clean)

str(dtm)

inspect(dtm)

library(caret)
folds <- createDataPartition(df$CLASS, p = 0.7, list=F)
train <- df[folds,]
test <- df[-folds,]

dtm.train <- dtm[folds,]
dtm.test <- dtm[-folds,]

corpus.train <- corpus_clean[folds]
corpus.test <- corpus_clean[-folds]

str(train)

library(wordcloud)
wordcloud(corpus.train, min.freq=10, # 10% of num docs in corpus is rough standard
          random.order = FALSE) # biggest words are nearer the centre


spam = train[train$CLASS==1,]
ham = train[train$CLASS==0,]

wordcloud(ham$CONTENT, min.freq=10, # 10% of num docs in corpus is rough standard
          random.order = FALSE) # biggest words are nearer the centre

wordcloud(spam$CONTENT,
          max.words = 10, # look at the 40 most common words
          scale=c(3, 0, 5)) # adjust max and min font sizes for words shown

wordcloud(ham$CONTENT,
          max.words = 10, # look at the 40 most common words
          scale=c(3, 0, 5)) # adjust max and min font sizes for words shown

str(dtm)

#freq_terms = findFreqTerms(dtm.train, 5)
#dtm.train = DocumentTermMatrix(corpus.train, list(dictionary=freq_terms))
#dtm.test =  DocumentTermMatrix(corpus.test, list(dictionary=freq_terms))

inspect(dtm.train)


svd = svd(as.matrix(dtm))
matrix = svd$u

data = as.data.frame(matrix)
data <- data[apply(data, 1, function(x) !all(x==0)),]

df$CLASS <- as.integer(as.character(df$CLASS))
data <- cbind(data,df$CLASS)

set.seed(123)
train_rows <- sample(x = nrow(data),size = 0.7*nrow(data))
train <- data[train_rows,]
test <- data[-train_rows,]

colnames(train)[ncol(train)] <- 'y'
colnames(test)[ncol(test)] <- 'y'

library(caret)
set.seed(123)
ctrl <- trainControl(method="repeatedcv",repeats = 3)

knnFit <- train(as.factor(y) ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
knnFit
plot(knnFit)
## K = 5 has the best Accuracy metric
pred <- predict(knnFit,test)
a <- table(pred,test$y)
a
accuracy <- sum(diag(a))/sum(a)
accuracy


#install.packages("e1071")
library(e1071)
# store our model in sms_classifier

convert_counts = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels=c("No", "Yes"))
  return (x)
}

# apply() allows us to work either with rows or columns of a matrix.
# MARGIN = 1 is for rows, and 2 for columns
reduced_dtm.train = apply(reduced_dtm.train, MARGIN=2, convert_counts)
reduced_dtm.test  = apply(reduced_dtm.test, MARGIN=2, convert_counts)

content_classifier = naiveBayes(reduced_dtm.train, train$CLASS, laplace = 1)

content_test.predicted = predict(content_classifier, reduced_dtm.test)

train$CLASS <- as.integer(as.character(train$CLASS))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train1 <- as.matrix(reduced_dtm.train)
train1 <- cbind(train1, train$CLASS)
colnames(train1)[ncol(train1)] <- 'y'
train1 <- as.data.frame(train1)
train1$y <- as.factor(train1$y)

test1 <- as.matrix(reduced_dtm.test)
#test1 <- cbind(test1, test$CLASS)
#colnames(test1)[ncol(test1)] <- 'y'
#test1 <- as.data.frame(test1)
#test1$y <- as.factor(test1$y)


# Train.
library(caret)
fit <- train(y ~ ., data = train1, method = 'bayesglm')

predicts = predict(fit, newdata = test1)


# once again we'll use CrossTable() from gmodels
#install.packages("gmodels")
library(gmodels)

CrossTable(predicts,
           test$CLASS,
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols

head(reduced_dtm.train)
