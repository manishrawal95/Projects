

benchmarkErrorRate <- function(training, test){
  prop_train = as.data.frame(prop.table(table(training)))
  prop_train=prop_train[order(-prop_train$Freq),]
  dominant_class=prop_train[1,1]
  guess = as.character(dominant_class)
  percent_correct_simple=sum(guess == as.character(test))/length(test)
  return(1 - percent_correct_simple)
}


# ?2014-2015 by Nathan Karst
# Creates a lift chart from  
# 
# INPUTS
#   obs         Binary (0/1 or TRUE/FALSE) observations
#   pred        Numerical predictions in [0,1]
#               
# OUTPUT
#   (none)
liftChart <- function(obs, pred){ 
  out <- tryCatch(
    {
      library(ggplot2)
    },
    error=function(cond) {
      install.packages("ggplot2")
      library(ggplot2)
    })  
  
  df = data.frame(obs,pred)  
  df = df[order(-pred),]
  
  y = c(0, cumsum(df[,1]))
  df1 = data.frame("Index"=0:length(pred),"y"=y,"type"="Observed")
  df2 = data.frame("Index"=0:length(pred),"y"=0:length(pred)*sum(df[,1])/length(pred),"type"="Benchmark")
  df3 = data.frame("Index"=c(0:sum(df[,1]),length(obs)),"y"=c(0:sum(df[,1]),sum(df[,1])),"type"="Ideal")
  
  df = rbind(df2,df3,df1)
  
  #plot(cumsum(df[,1]),ylab='Cumulative observations of TRUE')
  #lines(c(0,length(pred)), c(0,sum(df[,1])))
  qplot(Index,y,data=df,color=type,geom=c("line","point"),ylab="Cumulative Observations of TRUE",main="Lift Chart")
  
}


# ?2014-2015 by Nathan Karst
# Creates a ROC chart from  
# 
# INPUTS
#   obs         Binary (0/1 or TRUE/FALSE) observations
#   pred        Numerical predictions in [0,1]
#               
# OUTPUT
#   A lift chart with both 
ROCChart <- function(obs, pred){ 
  out <- tryCatch(
    {
      library(ggplot2)
    },
    error=function(cond) {
      install.packages("ggplot2")
      library(ggplot2)
    })  
  
  se = c()
  sp = c()
  P = c()
  for(i in 1:1001){
    p = (i-1)/1000
    
    predTF = (pred >= p)
    
    sp[i] = sum(predTF == FALSE & obs == FALSE)/sum(obs == FALSE)
    se[i] = sum(predTF == TRUE & obs == TRUE)/sum(obs == TRUE)
    P[i] = p
  } 
  
  AUC = c()
  for(i in 1:length(sp)-1){
    width = sp[i+1] - sp[i]
    height_left = se[i+1]
    height_right = se[i]
    height_delta = height_right - height_left
    
    AUC[i] = width*height_left + 0.5*width*height_delta
  } 
  
  AUC = sum(AUC)
  
  df1 = data.frame(x=1-sp,y=se,"type"="Observed")
  df1 = df1[order(df1$x,df1$y),]
  
  df2 = data.frame(x=1-sp,y=1-sp,"type"="Benchmark")
  df3 = data.frame(x=c(0,0,1),y=c(0,1,1),"type"="Ideal")
  
  
  df = rbind(df2,df3,df1)
  
  qplot(x,y,data=df,color=type,geom=c("point","line"),ylab="Sensitivity = True Positive Rate",xlab="1 - Specificity = False Positive Rate",main=paste("AUC =",round(AUC,2)))
  
}

easyPrune <- function(model){ 
  return(prune(model, cp = model$cptable[which.min(model$cptable[ , "xerror"]), "CP"]))
}


elbowChart <- function(df){
  x = c()
  for(i in 2:10){
    model = kmeans(df, i)
    x[i] = mean(model$withinss)
  }
  
  plot(1:10,x,type='b',ylab='Average Within Cluster Sum of Squares',xlab='Cluster Size')
}

removeOutliers <- function(df){
  for(i in 1:ncol(df)){
    if(is.integer(df[,i])){
      df[,i] = as.numeric(df[,i])
    }
    
    
    if(is.numeric(df[,i])){
      x = scale(df[,i])
      df = df[abs(x) < 3,]
    }
    
  }
  return(df)
}

knnCrossVal = function(form,data){
  model = train(
    form = form,
    data = data,
    method = "knn",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = expand.grid(k = seq(1, 25, by = 1))
  ) 
  plot(model)
  
  #return(which.max(model$results$Accuracy))
}
