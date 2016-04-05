library(ggplot2)

setwd("~/Spring Quater/Advanced mkt analytics/hw1")
data = read.csv("Homework 1 Data.csv")

data$pricePerUnit = data$dollars/data$units
data$LogUnits = log(data$units)
data$LogPrice = log(data$pricePerUnit)

# ggplot(aes(LogUnits,LogPrice),data = subset(data, weekOfYear == 1))+
#   geom_point()



#-----Q1-----
dumWeek = model.matrix(~factor(data$weekOfYear))[,-1]
models = list()
#baseline model: price, week of year adn weekNum as predictors
models[[1]] = lm(formula = LogUnits ~ pricePerUnit + weekNum + weekOfYear, data = data)
#logPrice, dummies of week of year and weekNum as predictors
models[[2]] = lm(formula = data$LogUnits ~ data$pricePerUnit + data$weekNum + dumWeek)
models[[3]] = lm(formula = data$LogUnits ~ data$LogPrice + data$weekNum + dumWeek)
#regression with interaction of week of year and LogPrice
models[[4]] = lm(formula = data$LogUnits ~ data$LogPrice*dumWeek)
#add weekNum on last model
models[[5]] = lm(formula = data$LogUnits ~ data$LogPrice*dumWeek+data$weekNum)
#quatratic price, weekNum and dumweek as predictors
models[[6]] = lm(formula = data$LogUnits ~ data$pricePerUnit+I(data$pricePerUnit^2)+dumWeek+data$weekNum)
#include interactionbetween logPrice and weekNum as predictor
models[[7]] = lm(formula = data$LogUnits ~ data$LogPrice*data$weekNum+dumWeek)
#
Q1 = as.data.frame(sapply(models, summary))

model = lm(formula = LogUnits ~ pricePerUnit, data = data)
summary(model)


#-----Q2-----
OptPrice = function(model){
  pricePerUnit = data.frame(pricePerUnit = seq(0,2,0.01))
  estimatedDemand = exp(predict(model, pricePerUnit))
  profit = (pricePerUnit[,1]-0.6)*estimatedDemand
  optimalPrice = pricePerUnit[which.max(profit),1]
  return(optimalPrice)
}

OptPrice(model)

#-----Q3-----
#calculate the bootstraped standard error of optimal price of 1000 and 2000 bootstrap samples.
#define a funtion to calculate.
#first parameter: number of bootstrap samples
#second parameter: sample size of each bootstrap sample
bootstrappedSE = function(nBootstraps,sampleSize){
  bsOptPrice = rep(NA,nBootstraps)
  for(bsNum in 1:nBootstraps){
    bsSample = data[sample(1:nrow(data), sampleSize, replace=TRUE),] 
    #re-estimate the demand model based on each bootstrap sample
    bsModel = lm(formula = LogUnits ~ pricePerUnit, data = bsSample)
    bsOptPrice[bsNum] = OptPrice(bsModel)
  }
  #return a list
  #first element: bootsrapped SE of optimal price
  #second element: optimal prices of each bootstrap sample
  return(list(var(bsOptPrice)^.5, bsOptPrice))
}

bootstrappedSE(1000, nrow(data))[[1]]
bootstrappedSE(2000, nrow(data))[[1]]

#plot histogram of the distribution of optimal price
bsOptPrice.df = data.frame(bsOptPrice = bootstrappedSE(1000, nrow(data))[[2]])
ggplot(bsOptPrice.df,aes(bsOptPrice))+geom_histogram(binwidth = 0.01)+
  labs(title = "bootstrap distribution of the optimal price--1000 bootstrap samples ")

bsOptPrice.df2 = data.frame(bsOptPrice = bootstrappedSE(2000, nrow(data))[[2]])
ggplot(bsOptPrice.df2,aes(bsOptPrice))+geom_histogram(binwidth = 0.01)+
  labs(title = "bootstrap distribution of the optimal price--2000 bootstrap samples ")

#-----Q4-----
sampleSize = seq(3000,5000,100)
#desiredSampleSize = 0
for (i in 1:length(sampleSize)){
  size = sampleSize[i]
  if (bootstrappedSE(1000, size)[[1]] < 0.01) break
}


#-----Q5-----
#the profit of 'true' demand model and 'true' optimal price
trueProfit = (0.83-0.6)*exp(predict(model, data.frame(pricePerUnit = 0.83)))

profitLoss = function(nBootstraps,sampleSize){
  #using the re-estimated demand model and OptPrice function to caldulate
  #the optimal prices for each bootstrap sample
  bt = bootstrappedSE(nBootstraps, sampleSize)
  bt.optPrice = bt[[2]]
  #calculate the expected profit of each bootstrap sample based on the 
  #demand model built from full sample--model.
  bt.profit = mean((bt.optPrice-0.6)*exp(predict(model, data.frame(pricePerUnit = bt.optPrice))))
  return(bt.profit - trueProfit)
}

#normal bootstrap sampler
profitLoss(1000, nrow(data))
profitLoss(1000, 1000)
profitLoss(1000, 4300)
#bootstrap with sample size of 2000

