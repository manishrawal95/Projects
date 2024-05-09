# Phase 1
library(rpart)
library(rpart.plot)
library(gmodels)
source("BabsonAnalytics.R")

df <- read_csv("~/Downloads/Github/Classification and Regression Trees/eBayAuctions.csv")

df$Competitive <- as.factor(df$Competitive)
df$ClosePrice <- NULL

N <- nrow(df)
trainingSize <- round(N * 0.6)
trainingCases <- sample(N, trainingSize)
training <- df[trainingCases,]
test <- df[-trainingCases,]

model <- rpart(Competitive ~ ., data = training)

# Set the directory path
output_dir <- "~/Downloads/Github/Classification and Regression Trees/"

# Set up a larger plotting device and save the plot
png(paste0(output_dir, "phase1_plot.png"), width = 800, height = 600)
rpart.plot(model, extra = 102, under = TRUE, type = 0, tweak = 1.2)
dev.off()

pred <- predict(model, test, type = "class")

errorRate <- sum(pred != test$Competitive) / nrow(test)
errorBench <- benchmarkErrorRate(training$Competitive, test$Competitive)

