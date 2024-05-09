

#LOAD

library(caret)
source("data/BabsonAnalytics.R")

UniversalBank <- read_csv("data/UniversalBank.csv")

View(UniversalBank)

#MANAGE


UniversalBank$Personal.Loan=as.factor(UniversalBank$Personal.Loan)  #K nearest neighbours always predict factors

# Knn must have numerical inputs - delete non numerical inputs
UniversalBank$Education = NULL
UniversalBank$Securities.Account= NULL
UniversalBank$CD.Account = NULL
UniversalBank$Online = NULL
UniversalBank$CreditCard = NULL


#normalize
standardizer = preProcess(UniversalBank, c("center","scale"))
UniversalBank = predict(standardizer, UniversalBank)


#PARTITION
set.seed(1234)
N = nrow(UniversalBank)
trainingSize = round(N*0.6)
trainingCases = sample(N, trainingSize)
training = UniversalBank[trainingCases,]
test = UniversalBank[-trainingCases,]

#BUILD

model = knn3(Personal.Loan ~ . , data = training, k = 3)

#PREDICT

predictions = predict(model, test, type="class")
observations = test$Personal.Loan

#EVALUATE

error_rate = sum(predictions != observations)/nrow(test)
error_rate = 1 - sum(predictions == observations)/nrow(test)


table(predictions, observations)

error_bench = benchmarkErrorRate(training$Personal.Loan, test$Personal.Loan)

# if the k is large then the model is underfit and is same as the benchmark
