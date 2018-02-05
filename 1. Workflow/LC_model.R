library(caTools)
library(ElemStatLearn)
library(rpart)
library(randomForest)
library(class)
library(e1071)
library(pROC)
library(ROCR)
library(caret)
library(naivebayes)
library(h2o)
library(plyr)
library(dplyr)

# Splitting Dataset into Training and Test Sets
set.seed(510)
split <- sample.split(lc2$loan_status2, SplitRatio = 0.75)
training_set <- subset(lc2, split == TRUE)
test_set <- subset(lc2, split == FALSE)

# Classification - Predicting Loan Default (1 = Defaulted, 0 = Not Defaulted)
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
}

# Making the Confusion Matrix
cm <- function(testdata, predlogit, yvar){
  result <- table(testdata[,yvar],predlogit)
  return(result)
}

# Misclassification Rate (misc_XXX) 
misclass <- function(cm){
  misclassrate <- (cm[1,2] + cm[2,1])/sum(cm)
  return(misclassrate)
}

# Unrestricted Logit Model
logit1 <- logit(training_set,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test_set, 77, 0.5)
cm1 <- cm(test_set,predlogit1,77)
misc1 <- misclass(cm1)

# Restricted Model #1
training2 <- training_set
training2$open_acc <- NULL
test2 <- test_set
test2$open_acc <-NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 76, 0.5)
cm2 <- cm(test2,predlogit1,76)
misc2 <- misclass(cm2)

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
misc3 <- misclass(cm3)

# Restricted Model #3
training2[c('acc_now_delinq')] <- NULL
test2[c('acc_now_delinq')] <- NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 70, 0.5)
cm4 <- cm(test2,predlogit1,70)
misc4 <- misclass(cm4)

# Restricted Model #4
training2[c('num_actv_bc_tl')] <- NULL
test2[c('num_actv_bc_tl')] <- NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 69, 0.5)
cm5 <- cm(test2,predlogit1, 69)
misc5 <- misclass(cm5)

# Restricted Model #5
training2[c('num_accts_ever_120_pd')] <- NULL
test2[c('num_accts_ever_120_pd')] <- NULL

logit1 <- logit(training2,loan_status2)
summary(logit1)
predlogit1 <- predlogit(logit1,test2, 68, 0.5)
cm6 <- cm(test2,predlogit1, 68)
misc6 <- misclass(cm6)

# Random Forest Training -----------------------------------------------------------------------
# 10 Trees
randforest <- randomForest(x = training_set[-77], y = training_set$loan_status2,
                          ntree = 10)
predrandforest <- predict(randforest, newdata = test_set[-77])
cm_rf1 <- cm(test_set,predrandforest,77)
misc_rf1 <- misclass(cm_rf1)

# 50 Trees
randforest <- randomForest(x = training_set[-77], y = training_set$loan_status2,
                          ntree = 50)
predrandforest <- predict(randforest, newdata = test_set[-77])
cm_rf2 <- cm(test_set,predrandforest,77)
misc_rf2 <- misclass(cm_rf2)

# 100 Trees
randforest <- randomForest(x = training_set[-77], y = training_set$loan_status2,
                          ntree = 100)
predrandforest <- predict(randforest, newdata = test_set[-77])
cm_rf3 <- cm(test_set,predrandforest,77)
misc_rf3 <- misclass(cm_rf3)

# XGBoost Training----------------------------------------------------------------------------------
lb <- as.numeric(training_set$loan_status2)-1

xgb <- xgboost(data = data.matrix(training_set[,-77]), 
               label = lb,
               nround = 50,
               eta = 0.1,
               max_depth = 15, 
               seed = 1989,
               eval_metric = "error",
               objective = "binary:logistic",
               subsample = 0.5, 
               colsample_bytree = 0.5, 
               silent = 0
)

predxgb <- predict(xgb,data.matrix(test_set[,-77]))
predxgb <- ifelse(predxgb > 0.5, 1, 0)
cm_xgb <- cm(test_set,predxgb,77)
misc_xgb <- misclass(cm_xgb)

names <- dimnames(data.matrix(training_set[,-77]))[[2]]
names <- colnames(training_set[,-77])

# Variable Importance
imatrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix = imatrix[1:30,])
xgb.plot.importance(importance_matrix = imatrix[50:76,])

print(imatrix)
# write.csv(imatrix, file = "imatrix.csv")

# Training with Fewer Features 
# Choosing More Important Variables Based on XGBoost Variable Importance
training3<- training_set[c("annual_inc", "delinq_2yrs", "dti", "emp_length_0-2", "emp_length_10+", "emp_length_3-5",
                           "emp_length_6-10", "home_ownership_MORTGAGE", "home_ownership_OTHER",
                           "home_ownership_OWN","inq_last_6mths","int_rate","issue_dsincelc","loan_amnt",
                           "num_accts_ever_120_pd","num_actv_bc_tl","num_actv_rev_tl","open_acc","term","total_acc",
                           "verification_status_Verified","verification_status_SourceVerified","loan_status2")]

test3<- test_set[c("annual_inc", "delinq_2yrs", "dti", "emp_length_0-2", "emp_length_10+", "emp_length_3-5",
                           "emp_length_6-10", "home_ownership_MORTGAGE", "home_ownership_OTHER",
                           "home_ownership_OWN","inq_last_6mths","int_rate","issue_dsincelc","loan_amnt",
                           "num_accts_ever_120_pd", "num_actv_bc_tl","num_actv_rev_tl","open_acc","term","total_acc",
                           "verification_status_Verified","verification_status_SourceVerified","loan_status2")]

rownum <- 23;
# k-NN Training------------------------------------------------------------------------------------------------------
predknn <- knn(train = training3[, -rownum],
             test = test3[, -rownum],
             cl = training3[, rownum],
             k =10)
cm_knn <- cm(test3,predknn,rownum)
misc_knn <- misclass(cm_knn)

# Naives Bayes Training---------------------------------------------------------------------------------------------
nb <- naiveBayes(x = training3[-rownum],
                y = training3$loan_status2)
prednb <- predict(nb, newdata = test3[-rownum])
cm_nb <- cm(test3,prednb,rownum)
misc_nb <- misclass(cm_nb)

# Artificial Neural Network Training--------------------------------------------------------------------------------
trainingfs <- training3
testfs <- test3
trainingfs[-23] <- scale(trainingfs[-23])
testfs[-23] <- scale(testfs[-23])

# Fitting ANN to the Training set
h2o.init(nthreads = -1)
annh2o <- h2o.deeplearning(y = 'loan_status2',
                              training_frame = as.h2o(trainingfs),
                              activation = 'Rectifier',
                              hidden = c(50,10),
                              epochs = 100,
                              train_samples_per_iteration = -2)

# Predicting the Test set results
predann <- h2o.predict(annh2o, newdata = as.h2o(testfs[-23]))
predann2 <- as.vector(predann$predict)

# Making the Confusion Matrix
cm_ann <- table(testfs[, 23], predann2)
misc_ann <- misclass(cm_ann)
h2o.shutdown() # Enter Y

# Model Selection --------------------------------------------------------------------------------------------------

# Restricted logit model has the highest accuracy
miscr <- c(misc6, misc_nb, misc_rf3, misc_xgb, misc_knn, misc_ann)
type <- c("restricted logit","naive bayes", "random forest", "xgboost", "k-nearest neighbors", "artificial neural network")
misctab<-data.frame(type,miscr)
misctab[order(misctab$miscr),]
rm(miscr)
rm(type)

# Area Under Curve
# Minimize your False Positive Rate & maximize your True Positive Rate

# Receiver Operating Characteristic Curve (ROC)
# Area Under Curve Calc
aucc <- c(auc(test_set$loan_status2,predlogit1), auc(test_set$loan_status2,predxgb),
  auc(test_set$loan_status2,ifelse(predknn==1,1,0)),auc(test_set$loan_status2,ifelse(prednb==1,1,0)), 
  auc(test_set$loan_status2,ifelse(predrandforest==1,1,0)), auc(test_set$loan_status2,ifelse(predann2==1,1,0)))
type <- c("restricted logit", "xgboost", "k-nearest neighbors", "naive bayes", "random forest", "artificial neural network")
auctab <- data.frame(type,aucc)
auctab <- auctab[order(auctab$aucc),]
rm(type)
rm(aucc)

tab <- inner_join(misctab, auctab)
# Artificial neural network is best with highest AUC while restricted logit model has the lowest misclassification rate.

# ROC Curves 
# Naive Bayes Model
predauc <- prediction(ifelse(prednb==1,1,0), test3$loan_status2)
roc <- performance(predauc,"tpr","fpr")
plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

# Restricted Logit Model
predauc <- prediction(ifelse(predlogit1==1,1,0), test_set$loan_status2)
roc <- performance(predauc,"tpr","fpr")
plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

# Artificial Neural Network Model
predauc <- prediction(ifelse(predann2==1,1,0), test3$loan_status2)
roc <- performance(predauc,"tpr","fpr")
plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

# K fold Cross Validation
# logit
foldslogit <- createFolds(training2$loan_status2, k = 20)
cv <- lapply(foldslogit, function(x) {
  training_fold = training2[-x, ]
  test_fold = training2[x, ]
  classifier = glm(formula = loan_status2 ~ .,family = binomial, data = training_fold)
  y_pred = predict(classifier, type = 'response', newdata = test_fold[-68])
  y_pred <- ifelse(y_pred > 0.5, 1, 0)
  cm1 = table(test_fold[, 68], y_pred)
  accuracy = (cm1[1,1] + cm1[2,2]) / (cm1[1,1] + cm1[2,2] + cm1[1,2] + cm1[2,1])
  return(accuracy)
})
accuracy_logit <- mean(as.numeric(cv))

foldsnb <- createFolds(training3$loan_status2, k = 10)
cv <- lapply(foldsnb, function(x) {
  training_fold = training3[-x, ]
  test_fold = training3[x, ]
  classifier = naiveBayes(x = training_fold,
             y = training_fold$loan_status2)
  y_pred = predict(classifier, newdata = test_fold[-rownum])
  cm2 = table(test_fold[, rownum], y_pred)
  accuracy= (cm2[1,1] + cm2[2,2]) / (cm2[1,1] + cm2[2,2] + cm2[1,2] + cm2[2,1])
  return(accuracy)
})
accuracy_nb <- mean(as.numeric(cv))

foldsxgb <- createFolds(training_set$loan_status2, k = 10)
cv <- lapply(foldsxgb, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  lb <- as.numeric(training_fold$loan_status2)-1
  
  classifier <- xgboost(data = data.matrix(training_fold[,-77]), 
                        label = lb,
                        nround = 50,
                        eta = 0.1,
                        max_depth = 15, 
                        seed = 1989,
                        eval_metric = "error",
                        objective = "binary:logistic",
                        subsample = 0.5, 
                        colsample_bytree = 0.5, 
                        silent = 0
  )
  y_pred <- predict(xgb,data.matrix(test_fold[,-77]))
  y_pred <- ifelse(y_pred > 0.5, 1, 0)
  cm3 <- table(test_fold[, 77], y_pred)
  accuracy<- (cm3[1,1] + cm3[2,2]) / (cm3[1,1] + cm3[2,2] + cm3[1,2] + cm3[2,1])
  return(accuracy)
})
accuracy_xgb <- mean(as.numeric(cv))

# Grid Search to find best hyperparameter
# names(getModelInfo())

classifiernb <- train(form = loan_status2 ~ ., data = training3, method = 'naive_bayes')
classifiernb
classifiernb$bestTune

classifierxgb = train(form = loan_status2 ~ ., data = training3, method = 'xgbTree')
classifierxgb
classifierxgb$bestTune

