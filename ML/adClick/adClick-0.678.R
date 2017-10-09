gc()
setwd("C:/Users/kmsra_000/Documents/Machine_Learning/adClick")
# load libraries

library(data.table)
library(lubridate)

# load data 

train <- fread("train.csv", na.strings = c(" ","",NA))
test <- fread("test.csv",na.strings = c(" ","",NA))
#ss <- fread("sample_submission.csv")
testID <- test$ID

train[browserid == "Google Chrome", browserid := "Chrome"]
train[browserid == "Internet Explorer", browserid := "IE"]
train[browserid == "InternetExplorer", browserid := "IE"]
train[browserid == "Mozilla Firefox", browserid := "Firefox"]
train[browserid == "Mozilla", browserid := "Firefox"]

test[browserid == "Google Chrome", browserid := "Chrome"]
test[browserid == "Internet Explorer", browserid := "IE"]
test[browserid == "InternetExplorer", browserid := "IE"]
test[browserid == "Mozilla Firefox", browserid := "Firefox"]
test[browserid == "Mozilla", browserid := "Firefox"]

str(train)
head(train)

# check target distribution
# In train data, only 3.6% of the ads got clicked
train[,.N/nrow(train),click]

# check time range
# The train data set contains information for 10 days from 10th Jan 2017 to 20th Jan  2017
train[,range(datetime)]

# check missing values
# siteid has around 9% missing values, browserid has around 5%, dev id has around ~14% missing values
sapply(train, function(x)(sum(is.na(x))/length(x)))

# check ID variables
# This shows train data has more number of sites, offers, categories and merchants.
for(x in c('siteid','offerid','category','merchant'))
{
  print(sprintf("There are %d unique %s in the train set and %d unique %s in the test set",train[,uniqueN(get(x))], x,test[,uniqueN(get(x))], x))
}

# Since ID variable have no ordered in them, we can't use them as is. 
# One possible option is replace them with their counts
# let's check their class first

sapply(train, class)
sapply(test, class)

# convert ID variables to their respective count
# using for-set for faster speed
cols <- c('siteid','offerid','category','merchant')
for(x in seq(cols))
{  
  train[,eval(cols[x]) := .N, by = eval(cols[x])]
  test[,eval(cols[x]) := .N, by=eval(cols[x])]
}
#testID <- test$ID

#################

# converting other variables to count
# here we need to convert character class values to integer to show count value  under the same column
# otherwise we'll have to create new columns for these respective features and remove the old ones.
#cols <- c('countrycode','browserid','devid')
#for(x in seq(cols))
#{
#  train[, eval(cols[x]) := as.integer(as.factor(get(cols[x])))][,eval(cols[x]) := .N, by = eval(cols[x])]
#  test[, eval(cols[x]) := as.integer(as.factor(get(cols[x])))][,eval(cols[x]) := .N, by = eval(cols[x])]
#}


# Note: You might see different values such as Mozilla Firefox, Firefox, Mozilla etc. 
# You might like to club them into one value ( but look for their respective device)


# Extract features from datetime variable
# datetime is the timestamp at which ad got live on web
train[,datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")]
test[,datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")]

train[,tweekday := as.integer(as.factor(weekdays(datetime)))-1]
train[,thour := hour(datetime)]
train[,tminute := minute(datetime)]

test[,tweekday := as.integer(as.factor(weekdays(datetime)))]
test[,thour := hour(datetime)]
test[,tminute := minute(datetime)]

################
library(dummies)

country = data.frame(dummy(train$countrycode))
train = cbind(train,country)
train[,"countrycode"] = NULL

country = data.frame(dummy(test$countrycode))
test = cbind(test,country)
test[,"countrycode"] = NULL
rm(country)

dev = data.frame(dummy(train$dev))
train = cbind(train,dev)
train[,"devid"] = NULL

dev = data.frame(dummy(test$dev))
test = cbind(test,dev)
test[,"devid"] = NULL
rm(dev)

browser = data.frame(dummy(train$browserid))
train = cbind(train,browser)
train[,"browserid"] = NULL

browser = data.frame(dummy(test$browserid))
test = cbind(test,browser)
test[,"browserid"] = NULL
rm(browser)

train$weekend = rep(0,nrow(train))

train<-train[tweekday==1|tweekday==2,weekend:=1,]
rm(train1)

# Model Training
# using lightgbm for faster training than xgb
# since data is large, instead of cross validation, we'll do hold out validation
library(lightgbm)
library(caret)
str(train)
str(test)


train <- train[,.(siteid, offerid, category, merchant, countrycode.a,countrycode.b,countrycode.c,countrycode.d,countrycode.e,countrycode.f,dev.Desktop,dev.Mobile,dev.Tablet,dev.NA,browserid.Chrome,browserid.Edge,browserid.Firefox,browserid.IE,browserid.Opera,browserid.Safari,browserid.NA,thour,tweekday, tminute, click)]
test <- test[,.(siteid, offerid, category, merchant, countrycode.a,countrycode.b,countrycode.c,countrycode.d,countrycode.e,countrycode.f,dev.Desktop,dev.Mobile,dev.Tablet,dev.NA,browserid.Chrome,browserid.Edge,browserid.Firefox,browserid.IE,browserid.Opera,browserid.Safari,browserid.NA,thour,tweekday,tminute)]

folds <- createDataPartition(train$click, p = 0.7, list=F)

trainX <- train[folds,]
valX <- train[!folds,]

lgb.trainX <- lgb.Dataset(as.matrix(trainX[,-c('click'),with=F]), label = trainX$click)
lgb.valX <- lgb.Dataset(as.matrix(valX[,-c('click'),with=F]), label = valX$click)

params <- list(
  objective = 'binary',
  metric = 'auc',
  feature_fraction = 0.8,
  bagging_fraction = 0.5,
  #bagging_freq = 5,
  is_unbalance = TRUE,
  max_depth = 15
)

model <- lgb.train(params = params
                   ,data = lgb.trainX
                   ,valids = list(valid = lgb.valX)
                   ,learning_rate = 0.05
                   ,early_stopping_rounds = 40
                   ,eval_freq = 20
                   ,nrounds = 500
)

# get feature importance
lgb.plot.importance(tree_imp = lgb.importance(model,percentage = TRUE))

# make predictions
preds <- predict(model, data = as.matrix(test), n = model$best_iter)
rm(folds)
rm(train)
rm(trainX)
head(preds)


# make submission
sub <- data.table(ID = testID, click = preds)
fwrite(sub, "fin9.csv") #~ 0.67822
head(sub)

# What to do next ? 
# 1. Tune the parameters
# 2. Instead of count encoding, try doing label encoding (as.integer(as.factor(x))-1)
# 3. Create more features
# 3. Try different models and ensemble