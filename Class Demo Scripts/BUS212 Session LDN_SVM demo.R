# Code for BUS212 Session 5

# demonstrate three more classifiers
# Linear Discrim Analysis
# Support Vector machines
# Neural Network

# First load all packages

library(tidyverse)
library(MASS)  # Package MASS includes the LDA function
library(lattice)
library(e1071)  # for SVM model
library(caret)
library(GGally)
library(randomForest)


# First part of script based on iris dataset
#  Another way to split into train and test
n <- nrow(iris)
set.seed(752)
test_idx <- sample.int(n, size= round(0.2 * n))  # 20% test set
train <- iris[-test_idx,]  # train has all rows except the index 
test <- iris[test_idx,]


# Use Lattice Box-Whiskers plot for exploration & demo more R functionality
bwplot(Sepal.Length+Sepal.Width+Petal.Length+Petal.Width ~ Species, data=train,
       allow.multiple=T,   # allow multiple responses (lhs)
       outer=T,            # draw multiple plots separately
       pch=16, col="cyan", # plot character, color,
       alpha=0.5,          #   and semi-transparancy color for median
       scales="free")      # use scale individually for each panel

# plot the data with 2 predictors
ggplot(train, aes(x=Sepal.Width, y=Petal.Width, 
                   color=Species, shape=Species))+
        geom_point(size=3) +
        ggtitle("Iris Species Discrimination")

# Specify a model formula to use in models -- save time
form <- as.formula("Species ~ .") # potentially use all 4 features
# Linear Discriminant Analysis
fitlda <- lda(form, data=train, na.action="na.omit") 

pred_lda <- predict(fitlda, train)$class  # use lda model to predict using orig data
table(pred_lda)

confusionMatrix(pred_lda, train$Species)

pred_ldatest <- predict(fitlda, test)$class # apply to test data
confusionMatrix(pred_ldatest, test$Species)


# Now svm

svm1 <- svm(form, data=train)

summary(svm1)
plot(svm1, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# now predict
pred_svm <- fitted(svm1)
confusionMatrix(pred_svm, train$Species)

# assess on test data
pred_svmtest <- predict(svm1, test)
confusionMatrix(pred_svmtest, test$Species)

###################################################
#  Similar application to Boston Airbnb data
#  Read in just the numeric variables
#  For convenience I saved just the numeric columns in a csv file

airbnb_df <- read_csv("Data/mainnum.csv")
# Target variable is in next-to-last column "Commercial" -- must be a factor
airbnb_df$Commercial <- as.factor(airbnb_df$Commercial)

#####################
# explore correlations among predictors
# explore some correlations

ggcorr(airbnb_df[, c(1:14,16)], label=TRUE, cex=3)

#  remove highly colinear columns and others, just to reduce predictors
drop2 <- c(3,5,6,8:11, 16)
airbnb_df <- airbnb_df[-drop2]

#  NAs create problems with these methods, so let's drop them
airbnb_df <- na.omit(airbnb_df)

airbnb_df <- filter(airbnb_df, maximum_nights < 2000)  # remove crazy high value

ggpairs(airbnb_df, upper = list(continuous = wrap("cor", size = 4)))

# Partition as usual
set.seed(1948)  
train.index <- sample(c(1:dim(airbnb_df)[1]), dim(airbnb_df)[1]*.7)  
train.df <- airbnb_df[train.index, ]
valid.df <- airbnb_df[-train.index, ]

ggplot(train.df, aes(x=accommodates, y=host_since)) + geom_jitter(aes(color=Commercial), alpha=0.6)


##############################################
# Specify a model formula to use in models -- save time
form <- as.formula("Commercial ~ .") # potentially use all features

# lda commands

# Linear Discriminant Analysis
fitlda2 <- lda(form, data=train.df, na.action="na.omit") 

pred_lda2 <- predict(fitlda2, train.df)$class  # use lda model to predict using orig data
table(pred_lda2)

confusionMatrix(pred_lda2, train.df$Commercial)

pred_ldatest2 <- predict(fitlda2, valid.df)$class   # apply to test data
confusionMatrix(pred_ldatest2, valid.df$Commercial)


# Now svm

svm2 <- svm(form, data=train.df)

summary(svm2)

plot(svm2, airbnb_df, host_since ~ accommodates, grid = 75,
     slice = list(host_since = 2015, accommodates = 3, bedrooms = 1, 
          price=180, minimum_nights = 2, maximum_nights =1125, 
          availability_90 = 29))

# now predict
pred_svm2 <- fitted(svm2)  
confusionMatrix(pred_svm2, train.df$Commercial)

# assess on test data
pred_svmtest2 <- predict(svm2, valid.df)
confusionMatrix(pred_svmtest2, valid.df$Commercial)

####  Finally, create with a random forest model for comparison with these predictors
#
rf <- randomForest(form, data=train.df, ntree=500, 
     mtry=4, nodesize = 5, importance=TRUE)

varImpPlot(rf, type = 1)

rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$Commercial)
