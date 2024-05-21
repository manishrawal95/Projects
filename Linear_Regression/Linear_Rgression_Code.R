############
### LOAD ###
############
df = read.csv('data/ToyotaCorolla.csv')

##############
### MANAGE ###
##############
df$Fuel_Type = as.factor(df$Fuel_Type)
df$Met_Color = as.logical(df$Met_Color)
df$Automatic = as.logical(df$Automatic)
df$Model = NULL


#################
### PARTITION ###
#################
set.seed(1234)
N = nrow(df)
trainingSize = round(N*0.8)
trainingCases = sample(N, trainingSize)
training = df[trainingCases, ]
test = df[-trainingCases, ]

#############
### BUILD ###
#############
model = lm(Price ~ ., data = training)
summary(model)

# improve the model
model = step(model)
summary(model)

###############
### PREDICT ###
###############
predictions = predict(model, test)

################
### EVALUATE ###
################
observations = test$Price
errors = observations - predictions
mape = mean(abs(errors/observations))
rmse = sqrt(mean(errors^2))

# benchmarking
errors_bench = observations - mean(training$Price)
mape_bench = mean(abs(errors_bench/observations))
rmse_bench = sqrt(mean(errors_bench^2))
