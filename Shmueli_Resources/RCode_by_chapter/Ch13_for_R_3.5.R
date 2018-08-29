#### Table 13.1, 13.2

library(adabag)
library(rpart) 
library(caret)

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# transform Personal.Loan into categorical variable
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)

# partition the data
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# single tree
tr <- rpart(Personal.Loan ~ ., data = train.df)
pred <- predict(tr, valid.df, type = "class")
confusionMatrix(pred, valid.df$Personal.Loan)

# bagging
bag <- bagging(Personal.Loan ~ ., data = train.df)
pred <- predict(bag, valid.df, type = "class")
confusionMatrix(as.factor(pred$class), valid.df$Personal.Loan)

# boosting
boost <- boosting(Personal.Loan ~ ., data = bank.df)
pred <- predict(boost, valid.df, type = "class")
confusionMatrix(as.factor(pred$class), valid.df$Personal.Loan)



#### Table 13.9

library(uplift)
voter.df <- read.csv("Voter-Persuasion.csv")
# transform variable MOVED_AD to numerical
voter.df$MOVED_AD_NUM <- ifelse(voter.df$MOVED_AD == "Y", 1, 0)

set.seed(1)  
train.index <- sample(c(1:dim(voter.df)[1]), dim(voter.df)[1]*0.6)  
train.df <- voter.df[train.index, ]
valid.df <- voter.df[-train.index, ]

# use upliftRF to apply a Random Forest (alternatively use upliftKNN() to apply kNN). 
up.fit <- upliftRF(MOVED_AD_NUM ~ AGE + NH_WHITE + COMM_PT + H_F1 + REG_DAYS+ 
                     PR_PELIG + E_PELIG + POLITICALC  + trt(MESSAGE_A),
                   data = train.df, mtry = 3, ntree = 100, split_method = "KL",
                   minsplit = 200, verbose = TRUE)
pred <- predict(up.fit, newdata = valid.df)
# first colunm: p(y | treatment) 
# second colunm: p(y | control) 
head(data.frame(pred, "uplift" = pred[,1] - pred[,2]))

