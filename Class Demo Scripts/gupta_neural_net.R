# from Arpan Gupta youtube video "Neural Networks in R"

library(MASS)
library(neuralnet)
library(stats) ##  for stepwise regression

set.seed(123)
DataFrame <- Boston
str(DataFrame)
help("Boston")

## medv will be target variable; look at the distribution
hist(DataFrame$medv)

### will want to normalize to interval [0,1]
# start by computering the min and max of each variable
apply(DataFrame,2, range)  # the 2 applies the function to the COLUMNS

# then create vectors of max and min, and proceed to rescale each col
# Gupta does this within the original dataframe, but we'll create a 
# second, scaled df

maxValue <- apply(DataFrame,2,max)
minValue <- apply(DataFrame,2,min)

# Here comes the normalization step
# curious students should look up the documentation on the BASE R scale function
dfstd <- as.data.frame(scale(DataFrame, center = minValue, 
     scale = maxValue-minValue)) 
apply(dfstd,2,range)

hist(dfstd$medv)

### Now partition full set into 80% train and 20% test

n <- nrow(dfstd)
n1 <- round(.8*n,0)

ind <- sample(1:n, n1)
train <- dfstd[ind,]
test <- dfstd[-ind,]

### Before training a neural network, let's run a multiple regression 
# model to predict medv, using backward elimination to illustrate
# model performance.  We'll use the original data:
trainreg <- DataFrame[ind,]
testreg <- DataFrame[-ind,]

full <- lm(medv~., data=trainreg)
summary(full)

model1 <- step(full, data=trainreg, direction="backward")
summary(model1)

# now make predictions on test set
regpred <- predict(model1, testreg)
MSEr <- sum((regpred - testreg$medv)^2)/nrow(testreg)
MSEr  #  MSE for stepwise regression

# use regression model to plot actual vs. predicted
plot(testreg$medv, regpred, col='blue', main='Actual vs Predicted Regression',
     pch=1, cex=0.9, type="p", xlab="Actual", ylab="Predicted")
abline(0,1,col="orange")


### now fit a neural net
# Gupta starts with 13-4-2-1
## That is, 13 inputs
## 4 neurons in 1st hidden layer
## 2 neurons in 2nd hidden layer
## 1 is the output layer (medv)

## neural net command can't use formula like "medv ~ .", but needs all
## inputs named explicitly. Here is some code to build up the formula:
allVars <- colnames(dfstd)  # vector of column names
predcols <- allVars[!allVars%in%"medv"]  # all col names EXCEPT medv
predVars <- paste(predcols, collapse = "+")  # Paste function is great; Google it!
formnn <- as.formula(paste("medv~", predVars))

modelnn <- neuralnet(formula=formnn, hidden = c(4,2), 
     linear.output = T, data = train)

# plot the Neural Net
plot(modelnn)

# Predict using test data set
# use the compute function to make predictions
pred <- compute(modelnn, test[,predcols])
str(pred)

# Now unscale the predictions and acutals to evaluate model

predictions <- pred$net.result*(max(testreg$medv)-min(testreg$medv)) +
     min(testreg$medv)
actualValues <- (testreg$medv)

MSE <- sum((predictions - actualValues)^2)/nrow(test)
MSE  # MSE for neural net

plot(actualValues, predictions, col='blue', main='Actual vs Predicted',
     pch=1, cex=0.9, type="p", xlab="Actual", ylab="Predicted")
abline(0,1,col="orange")

## Gupta's demo uses 13-4-2-1. Let's try a simpler network formulation
##13-5-1

modelnn2 <- neuralnet(formula=formnn, hidden = c(5), 
                     linear.output = T, data = train)

# plot the Neural Net
plot(modelnn2)

# Predict using test data set
# use the compute function to make predictions
pred2 <- compute(modelnn2, test[,predcols])


# Now unscale the predictions and acutals to evaluate model

predictions2 <- pred2$net.result*(max(testreg$medv)-min(testreg$medv)) +
     min(testreg$medv)
actualValues <- (testreg$medv)

MSE2 <- sum((predictions2 - actualValues)^2)/nrow(test)
MSE2  # MSE for simpler neural net -- not as good!

plot(actualValues, predictions2, col='blue', main='Actual vs Predicted',
     pch=1, cex=0.9, type="p", xlab="Actual", ylab="Predicted")
abline(0,1,col="orange")

