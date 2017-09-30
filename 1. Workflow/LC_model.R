# Machine Learning ---------------------------------------------------------------------------
library(caTools)
library(ElemStatLearn)
library(rpart)

set.seed(510)
split <- sample.split(lc2$loan_status2, SplitRatio = 0.75)
training_set <- subset(lc2, split == TRUE)
test_set <- subset(lc2, split == FALSE)

# Logit Regression Training --------------------------------------------------------------------
logit <- function(traindata1, yvar){
  logit1 <- eval(substitute(glm(formula = yvar ~ .,
                family = binomial,
                data = traindata1)))
}

# Predicting the Test set results
predlogit<-function(logit, testdata, yvar, cutoff){
  prob_pred <- eval(substitute(predict(logit, type = 'response', newdata = testdata[,-yvar])))
  y_pred <- eval(substitute(ifelse(prob_pred > cutoff, 1, 0)))
  y_pred <- ifelse(y_pred > 0.5, 1, 0)
}

# Making the Confusion Matrix
cm <- function(testdata, predlogit, yvar){
  result <- table(testdata[,yvar],predlogit)
  return(result)
}

# Misclassification Rate 
misclasslogit <- function(cm){
  misclassrate <- (cm[1,2] + cm[2,1])/sum(cm)
  return(misclassrate)
}

# Unrestricted Model

logit1 <- logit(training_set,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test_set, 77, 0.5)
cm1 <- cm(test_set,predlogit1,77)
misc1 <- misclasslogit(cm1)

# Restricted Model #1
training2 <- training_set
training2$open_acc <- NULL
test2 <- test_set
test2$open_acc <-NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 76, 0.5)
cm2 <- cm(test2,predlogit1,76)
misc2 <- misclasslogit(cm2)

# Restricted Model #2
training2[c('mths_since_recent_bc_dlq_0-12','mths_since_recent_bc_dlq_13-24', 
            'mths_since_recent_bc_dlq_25-48', 'mths_since_recent_bc_dlq_49-84',
            'mths_since_recent_bc_dlq_85+')] <- NULL
test2[c('mths_since_recent_bc_dlq_0-12','mths_since_recent_bc_dlq_13-24', 
        'mths_since_recent_bc_dlq_25-48', 'mths_since_recent_bc_dlq_49-84',
        'mths_since_recent_bc_dlq_85+')] <- NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 71, 0.5)
cm3 <- cm(test2,predlogit1,71)
misc3 <- misclasslogit(cm3)

# Restricted Model #3
training2[c('acc_now_delinq')] <- NULL
test2[c('acc_now_delinq')] <- NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 70, 0.5)
cm4 <- cm(test2,predlogit1,70)
misc4 <- misclasslogit(cm4)

# Restricted Model #4
training2[c('num_actv_bc_tl')] <- NULL
test2[c('num_actv_bc_tl')] <- NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 69, 0.5)
cm5 <- cm(test2,predlogit1, 69)
misc5 <- misclasslogit(cm5)

# Restricted Model #5
training2[c('num_accts_ever_120_pd')] <- NULL
test2[c('num_accts_ever_120_pd')] <- NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 68, 0.5)
cm6 <- cm(test2,predlogit1, 68)
misc6 <- misclasslogit(cm6)

# # Logit Boost
# logitb <- LogitBoost(training2[,-68],training2[,68],nIter = 10)
# predb   = predict(logitb, training2[,-68])

# Decision Tree Training -----------------------------------------------------------------------
training_set$loan_status2 <- factor(training_set$loan_status2, levels = c(0, 1))
test_set$loan_status2 <- factor(test_set$loan_status2, levels = c(0, 1))
preddectree = rpart(formula = loan_status2 ~ ., data = training_set)
# Predicting the Test set results
pred_dt = predict(preddectree, newdata = test_set[-77], type = 'class')

cm_dt1 <- table(test_set[,77],pred_dt)
print(cm_dt1)

plot(preddectree)
text(preddectree)

# Logit Boost