# Machine Learning -------------------------------------------------------
library(caTools)
library(ElemStatLearn)
library(rpart)

set.seed(510)
split <- sample.split(lc2$loan_status2, SplitRatio = 0.75)
training_set <- subset(lc2, split == TRUE)
test_set <- subset(lc2, split == FALSE)

# Logit Regression Fitting
logit <- function(traindata1, yvar){
  logit1 <- substitute(glm(formula = yvar ~ .,
                family = binomial,
                data = traindata1))
}

logit1 <- logit(training_set,loan_status2)
summary(logit1)

# Predicting the Test set results
predlogit<-function(logit, testdata, yvar, cutoff){
  prob_pred <- predict(logit, type = 'response', newdata = testdata[,-yvar])
  y_pred <- ifelse(prob_pred > cutoff, 1, 0)
}

# Making the Confusion Matrix
cm <- table(test_set[, 77], y_pred)
print(cm)

misclass1 <- (cm[1,2] + cm[2,1])/sum(cm)
print(misclass1)


# Fitting Logistic Regression to the Training set 
# Unrestricted Model
logit1 <- glm(formula = loan_status2 ~ .,
                 family = binomial,
                 data = training_set)

summary(logit1)

# Predicting the Test set results
prob_pred <- predict(logit1, type = 'response', newdata = test_set[,-77])
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm <- table(test_set[, 77], y_pred)
print(cm)

misclass1 <- (cm[1,2] + cm[2,1])/sum(cm)
print(misclass1)





# Fitting Logistic Regression to the Training set (Restricted Model #1)
training2 <- training_set
training2$open_acc <- NULL
test2 <- test_set
test2$open_acc <-NULL

logit2 <- glm(formula = loan_status2 ~ .,
              family = binomial,
              data = training2)

summary(logit2)

# Predicting the Test set results
prob_pred2 <- predict(logit2, type = 'response', newdata = test_set[,-77])
y_pred2 <- ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm2 <- table(test_set[, 77], y_pred)
print(cm2)

misclass2 <- (cm2[1,2] + cm2[2,1])/sum(cm2)
print(misclass2)

# Fitting Logistic Regression to the Training set (Restricted Model #2)
training2[c('mths_since_recent_bc_dlq_0-12','mths_since_recent_bc_dlq_13-24', 
            'mths_since_recent_bc_dlq_25-48', 'mths_since_recent_bc_dlq_49-84',
            'mths_since_recent_bc_dlq_85+')] <- NULL
test2[c('mths_since_recent_bc_dlq_0-12','mths_since_recent_bc_dlq_13-24', 
        'mths_since_recent_bc_dlq_25-48', 'mths_since_recent_bc_dlq_49-84',
        'mths_since_recent_bc_dlq_85+')] <- NULL

logit2 <- glm(formula = loan_status2 ~ .,
              family = binomial,
              data = training2)

summary(logit2)

# Fitting Logistic Regression to the Training set (Restricted Model #3)
training2[c('acc_now_delinq')] <- NULL
test2[c('acc_now_delinq')] <- NULL

logit2 <- glm(formula = loan_status2 ~ .,
              family = binomial,
              data = training2)

summary(logit2)

# Fitting Logistic Regression to the Training set (Restricted Model #4)
training2[c('num_actv_bc_tl')] <- NULL
test2[c('num_actv_bc_tl')] <- NULL

logit2 <- glm(formula = loan_status2 ~ .,
              family = binomial,
              data = training2)

summary(logit2)

# Fitting Logistic Regression to the Training set (Restricted Model #5)
training2[c('num_accts_ever_120_pd')] <- NULL
test2[c('num_accts_ever_120_pd')] <- NULL

logit2 <- glm(formula = loan_status2 ~ .,
              family = binomial,
              data = training2)

summary(logit2)

# Predicting the Test set results
prob_pred2 <- predict(logit2, type = 'response', newdata = test_set[,-77])
y_pred2 <- ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm2 <- table(test_set[, 77], y_pred)
print(cm2)

misclass2 <- (cm2[1,2] + cm2[2,1])/sum(cm2)
print(misclass2)

# Fitting Decision Tree Classification to the Training set
dectree = rpart(formula = loan_status2 ~ ., data = training_set)

# Predicting the Test set results
y_pred = predict(dectree , newdata = test_set[-77], type = 'class')


# Logit Boost