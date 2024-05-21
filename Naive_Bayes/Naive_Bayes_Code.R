library(e1071)
library(gmodels)

source("/Users/nathankarst/Dropbox/Teaching/Data Analytics in R/BabsonAnalytics.R")

df = read.csv("/Users/nathankarst/Dropbox/Teaching/Data Analytics in R/data/MovieReviews.csv")
everyColumn = colnames(df)
df[everyColumn] = lapply(df[everyColumn], factor)

N = nrow(df)
trainingSize  = round(N*0.6)
trainingCases = sample(N, trainingSize)
training  = df[trainingCases,]
test      = df[-trainingCases,]

model = naiveBayes(PositiveTweet ~ ., data=training)
pred = predict(model, test)
obs = test$PositiveTweet

table(pred,obs)

error_rate = sum(pred != obs)/nrow(test)
error_bench = benchmarkErrorRate(training$PositiveTweet, test$PositiveTweet)

model$tables$awesome

#   awesome
#Y             0           1
#0      0.999452954 0.000547046
#1      0.714933993 0.285066007*
#* Given the tweet is positive, the prob. it contains "awesome" is 28.5%

limited = df[df$awesome == "1", ] # all the tweets that contain "awesome"
odds = sum(limited$PositiveTweet == "1") / sum(limited$PositiveTweet == "0")
sum(df$awesome == "1" & df$PositiveTweet == "1")/
  sum(df$awesome == "1" & df$PositiveTweet == "0")

limited = df[df$the == "1", ] # all the tweets that contain "the"
odds = sum(limited$PositiveTweet == "1") / sum(limited$PositiveTweet == "0")

limited = df[df$terrible == "1", ] # all the tweets that contain "terrible"
odds = sum(limited$PositiveTweet == "1") / sum(limited$PositiveTweet == "0")