rm(list=ls(all=TRUE))

library(data.table)
train <- fread("train.csv")
test <- fread("test.csv")
sample_sub <- fread("sample_submission.csv")

train <- train[order(PID)]
test <- test[order(PID)]

weight = rep(0,nrow(train))

train <- cbind(train, weight)

count = rep(1,nrow(train))
train <- cbind(train, count)


#Adding weights to the most recent eventids
train[train$Date=="201101",]$weight = 1/6*(0/3)
train[train$Date=="201102",]$weight = 1/6*(0/3)
train[train$Date=="201103",]$weight = 1/6*(0/3)
train[train$Date=="201104",]$weight = 1/6*(0/3)
train[train$Date=="201105",]$weight = 1/6*(0/3) 
train[train$Date=="201106",]$weight = 1/6*(0/3) 
train[train$Date=="201107",]$weight = 1/6*(0/3)  
train[train$Date=="201108",]$weight = 1/6*(0/3)  
train[train$Date=="201109",]$weight = 1/6*(0/3)  
train[train$Date=="201110",]$weight = 1/6*(0/3)
train[train$Date=="201111",]$weight = 1/6*(0/3) 
train[train$Date=="201112",]$weight = 1/6*(0/3) 

train[train$Date=="201201",]$weight = 2/6*(0/3)  
train[train$Date=="201202",]$weight = 2/6 *(0/3) 
train[train$Date=="201203",]$weight = 2/6 *(0/3) 
train[train$Date=="201204",]$weight = 2/6 *(0/3) 
train[train$Date=="201205",]$weight = 2/6 *(0/3) 
train[train$Date=="201206",]$weight = 2/6 *(0/3) 
train[train$Date=="201207",]$weight = 2/6 *(0/3) 
train[train$Date=="201208",]$weight = 2/6 *(0/3) 
train[train$Date=="201209",]$weight = 2/6 *(0/3) 
train[train$Date=="201210",]$weight = 2/6 *(0/3)
train[train$Date=="201211",]$weight = 2/6 *(0/3) 
train[train$Date=="201212",]$weight = 2/6 *(0/3) 

train[train$Date=="201301",]$weight = 0
train[train$Date=="201302",]$weight = 0
train[train$Date=="201303",]$weight = 0
train[train$Date=="201304",]$weight = 0
train[train$Date=="201305",]$weight = 0
train[train$Date=="201306",]$weight = 0
train[train$Date=="201307",]$weight = 1/21  
train[train$Date=="201308",]$weight = 2/21  
train[train$Date=="201309",]$weight = 3/21  
train[train$Date=="201310",]$weight = 4/21
train[train$Date=="201311",]$weight = 5/21  
train[train$Date=="201312",]$weight = 6/21  

summary(train)
str(train)
head(train)
head(test)


library(plyr)

groupColumns = c("PID","Event","Date")
dataColumns = c("count","weight")
res = ddply(train, groupColumns, function(x) x[dataColumns[1]]*x[dataColumns[2]])
head(res)


groupColumns = c("PID","Event","Date")
dataColumns = c("count")
res = ddply(train, groupColumns, function(x) colSums(x[dataColumns]))
head(res)

total = rep(1,nrow(res))
res = cbind(res,total)
groupColumns = c("PID")
dataColumns = c("count")
rest1 = ddply(train, groupColumns, function(x) colSums(x[dataColumns]))
head(res)



groupColumns = c("PID","Event")
dataColumns = c("weight")
res1 = ddply(train, groupColumns, function(x) colSums(x[dataColumns]))
head(res)

res1 <- res1[order(res1$PID,-res1$weight),]
library(dplyr)
res2 <- res1 %>%
  group_by(PID) %>%
  top_n(n = 10, wt = weight)


# getting the length of unique values of Event codes
length(unique(train$Event))
res3 = subset(res2, select=-c(weight))


#rm(train_dcast)

#train_dcast <- reshape(res3, idvar="ID", timevar="Event", direction = "wide")

res3 <- within(res3, {
  PID <- as.character(PID)
  ID <- ave(PID, PID, FUN=seq_along)
})

train_dcast <- dcast(res3, PID ~ ID, value.var="Event")
write.csv(train_dcast,"samplesub.csv")


#train_dcast <- dcast(data = train, PID ~ Event, length, value.var = "Event")
#random_submit <- colnames(train_dcast)[-1][apply(train_dcast[,-c('PID'),with=F],1, function(x)order(-x)[1:10])]

#random_mat <- as.data.table((matrix(random_submit,ncol = 10, byrow = T)))
#colnames(random_mat) <- colnames(sample_sub)[-1]

#random_mat <- cbind(PID = test$PID, random_mat)

#fwrite(random_mat,"random_sub.csv")


