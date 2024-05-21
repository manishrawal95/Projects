source('~/Downloads/data/BabsonAnalytics.R')

df = read.csv("~/Downloads/data/BostonHousing.csv")

df$ISHIGHVAL = as.logical(df$ISHIGHVAL)
df$CHAS = as.factor(df$CHAS)
df$MEDV = NULL

N = nrow(df)
trainingSize  = round(N*0.6)
trainingCases = sample(N, trainingSize)
training  = df[trainingCases,]
test      = df[-trainingCases,]

model = glm(ISHIGHVAL ~ ., data=training, family=binomial)
summary(model)

model = step(model)

pred = predict(model, test, type="response")

predTF = (pred > 0.75)
table(predTF, test$ISHIGHVAL)

errorRate = sum(predTF != test$ISHIGHVAL)/nrow(test)
errorBench = benchmarkErrorRate(training$ISHIGHVAL, test$ISHIGHVAL)

ROCChart(test$ISHIGHVAL, pred)
liftChart(test$ISHIGHVAL, pred)
