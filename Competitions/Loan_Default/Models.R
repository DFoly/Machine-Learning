#################################
### Models
#################################
load('train_Cleaned.RData', envir=.GlobalEnv)
train <- read.csv(file.choose(), header=T)
#############################################################
# training and test sets
#############################################################
New_data_train$gp <- runif(dim(New_data_train)[1]) # generate random sample
testSet <- subset(New_data_train, New_data_train$gp  <= 0.1)
trainingSet <- subset(New_data_train, New_data_train$gp > 0.1)
#############################
## Simple logistic regression
#############################
lreg <- glm(Loan_Status ~ Credit_History, family = binomial(link = 'logit')
            ,data = trainingSet)

summary(lreg)
anova(lreg, test = 'Chisq')

fitted.results <- predict(lreg, newdata = testSet[,-1], type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testSet$Loan_Status)
print(paste('Accuracy',1-misClasificError))

##########################
# Logistic Regression 2
##########################

lreg2 <- glm(Loan_Status ~ Credit_History + Married + Semiurban +
               DebtIncome, family = binomial(link = 'logit')
            ,data = trainingSet)

summary(lreg2)
anova(lreg, test = 'Chisq')

fitted.result2 <- predict(lreg2, newdata = testSet[,-1], type = 'response')
fitted.results2 <- ifelse(fitted.results2 > 0.5,1,0)

misClasificError <- mean(fitted.results2 != testSet$Loan_Status)
print(paste('Accuracy',1-misClasificError))




#######################
###  SVM
######################
library(e1071)
model_svm <- svm(Loan_Status ~ Married + Dependents +
                   Credit_History + Property_Area, data = trainingSet)

fitted <- predict(model_svm, newdata = testSet[,-1], type = 'response')
fitted_svm <- ifelse(fitted > 0.5,1,0)

misClasificError_svm <- mean(fitted_svm != testSet$Loan_Status)
print(paste('Accuracy',1-misClasificError_svm))

## tune the parameters with a grid search algorithm
tuned <- tune(svm, Loan_Status ~ Married + Dependents +
                Credit_History + Property_Area, data = trainingSet,
              ranges = list(epsilon = seq(0,1,0.1)))
print(tuned)
plot(tuned)
# epsilon 0.5 is the best
# extract bestmodel
best_model <- tuned$best.model
fitted_svm2 <- predict(best_model,newdata = test[,-1], type = 'response')
fitted_svm2 <- ifelse(fitted_svm2 > 0.5,1,0)

misClasificError_svm2 <- mean(fitted_svm2 != testSet$Loan_Status)
print(paste('Accuracy',1-misClasificError_svm2))




##################################
#  boosting
#################################
# Best result from Boosting
# best model 3,5,8,10 which is married, credit_History, Suburban and DebttoIncome
library(gbm)
model <- gbm.fit(x = trainingSet[,c(3,5,8,10)],
                 y = trainingSet$Loan_Status, distribution = 'bernoulli',
                 n.trees = 5000,
                 shrinkage = 0.1,
                 interaction.depth = 3,
                 n.minobsinnode = 10,
                 nTrain = 557,
                 verbose = TRUE)
gbm.perf(model)
summary(model)

## Save results
ID <- as.data.frame(New_data_test$Loan_ID)
results <- cbind(ID, fitted_gbm)

# need to submit as Y or N
results$fitted_gbm <- ifelse(fitted_gbm == 1, 'Y', 'N')

setwd("C:/Users/dfoley/Dropbox/Hackathon/Loan_default")
write.csv(results, file = "results_gbm_3.csv",  row.names = FALSE)