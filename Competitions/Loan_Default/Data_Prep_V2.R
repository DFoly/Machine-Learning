train <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Loan_default/train_u6lujuX_CVtuZ9i.csv",
                  header = T, na.strings=c("",".","NA"))

test <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Loan_default/test_Y3wMUE5_7gLdaTN.csv",
                 header = T, na.strings=c("",".","NA"))

train <- read.csv("~/Dropbox/Hackathon/Loan_default/train_u6lujuX_CVtuZ9i.csv")
test <- read.csv("~/Dropbox/Hackathon/Loan_default/test_Y3wMUE5_7gLdaTN.csv")

str(train)
str(test)
summary(train)

###############################
# Income Variable
######################################
train$Income <- train$ApplicantIncome + train$CoapplicantIncome
summary(train$Income)
test$Income <- test$ApplicantIncome + test$CoapplicantIncome

train$Income_Norm <- scale(train$Income)
colMeans(train$Income_Norm) # mean 0
test$Income_Norm <- scale(test$Income)

#################################
### replace outliers with median
#################################
train$Income_1 <- ifelse(train$Income >= 10000, 
                         median(train$Income, na.rm = TRUE),train$Income)

test$Income_1 <- ifelse(test$Income >= 10000, 
                        median(test$Income, na.rm = TRUE),test$Income)

# replace anything above 30,000 with median
train$LoanAmount <- ifelse(is.na(train$LoanAmount), 
                           median(train$LoanAmount, na.rm = TRUE),train$LoanAmount)

test$LoanAmount <- ifelse(is.na(test$LoanAmount), 
                          median(test$LoanAmount, na.rm = TRUE),test$LoanAmount)

# replace outliers with median
train$LoanAmount_1 <- ifelse(train$LoanAmount >= 250, 
                             median(train$LoanAmount, na.rm = TRUE),train$LoanAmount)
test$LoanAmount_1 <- ifelse(test$LoanAmount >= 250, 
                            median(test$LoanAmount, na.rm = TRUE),test$LoanAmount)

# standardise data
train$Loan_Norm <- scale(train$LoanAmount_1)
colMeans(scaled.income) # mean 0
apply(scaled.income, 2, sd)
test$Loan_Norm <- scale(test$LoanAmount_1)

#####################
### Debt to income
####################
train$DebtIncome <- train$Loan_Norm/train$Income_Norm
test$DebtIncome <- test$Loan_Norm/test$Income_Norm

# original data 1 = yes, 2 = No
train$Loan_Status <- ifelse(train$Loan_Status == 'Y', 1, 0)



##########################################
## Impute categorical variables
##########################################
library(mice)
md.pattern(train)

# impute categorical data
imputed_Data <- mice(train[,c(2,3,6,11)], m=5, maxit = 50, method = 'logreg', seed = 500)
summary(imputed_Data)
Data.imp <-complete(imputed_Data,4)

imputed_Data_test <- mice(test[,c(2,3,6,11)], m=5, maxit = 50, method = 'logreg', seed = 500)
summary(imputed_Data_test)
Data.imp_test <-complete(imputed_Data_test,4)
# replace NA with 0 in dependent category

train$Dependents[is.na(train$Dependents)] <- 0
#train$Dependents[train$Dependents == 3+] <- 3
test$Dependents[is.na(test$Dependents)] <- 0

train$Dependents[train$Dependents %in% c(1,2)] <- 1 
test$Dependents[test$Dependents %in% c(1,2)] <- 1  


# model improves if we make semiurban separate variable
train$Semiurban <- ifelse(train$Property_Area == 'Semiurban',1,0)
test$Semiurban <- ifelse(test$Property_Area == 'Semiurban',1,0)

# Loan Term
train$Loan_Amount_Term <- ifelse(train$Loan_Amount_Term < 360, 'Short_Term', 'Long_Term')
test$Loan_Amount_Term <- ifelse(test$Loan_Amount_Term < 360, 'Short_Term', 'Long_Term')



# Make new dataset
attach(train)
New_data_train <- data.frame(Loan_ID, Data.imp, Loan_Norm, Loan_Amount_Term,
                             Semiurban, Dependents, DebtIncome, Loan_Status)
detach(train)

attach(test)
New_data_test <- data.frame(Loan_ID, Data.imp_test, Loan_Norm, Loan_Amount_Term,
                            Semiurban, Dependents, DebtIncome)
detach(test)

#############################################################
# training and test sets
#############################################################
New_data_train$gp <- runif(dim(New_data_train)[1]) # generate random sample
testSet <- subset(New_data_train, New_data_train$gp  <= 0.1)
trainingSet <- subset(New_data_train, New_data_train$gp > 0.1)
#############################################################
setwd("C:/Users/dfoley/Desktop/GitHub/Competitions/Loan_Default")
save(New_Data_train, file = 'train_v2.RData')
save(New_Data_test, file = 'test_v2.RData')
