train <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Loan_default/train_u6lujuX_CVtuZ9i.csv",
                  header = T, na.strings=c("",".","NA"))

test <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Loan_default/test_Y3wMUE5_7gLdaTN.csv",
                 header = T, na.strings=c("",".","NA"))
train <- read.csv("~/Dropbox/Hackathon/Loan_default/train_u6lujuX_CVtuZ9i.csv")
test <- read.csv("~/Dropbox/Hackathon/Loan_default/test_Y3wMUE5_7gLdaTN.csv")


str(train)
dim(train)
head(train$Loan_Status)

##########################################
### clean data
#########################################
## recode variables in training and test sets
train$Gender <- ifelse(!is.na(train$Gender) & train$Gender == 'Male', 1, 0)
train$Married <- ifelse(!is.na(train$Married) & train$Married == 'Yes', 1, 0)
train$Education <- ifelse(!is.na(train$Education) & train$Education == 'Graduate', 1, 0)
train$Self_Employed <- ifelse(!is.na(train$Self_Employed) & train$Self_Employed == 'Yes',1,0)


## recode variables in training and test sets
test$Gender <- ifelse(!is.na(test$Gender) & test$Gender == 'Male', 1, 0)
test$Married <- ifelse(!is.na(test$Married) & test$Married == 'Yes', 1, 0)
test$Education <- ifelse(!is.na(test$Education) & test$Education == 'Graduate', 1, 0)
test$Self_Employed <- ifelse(!is.na(test$Self_Employed) & test$Self_Employed == 'Yes',1,0)



# create low medium and high Income
train$CoapplicantIncome <- cut(train$CoapplicantIncome, c(0,1000,2000,50000), 
                               labels=c('Low','Medium', 'High'),
                               include.lowest=T)

test$CoapplicantIncome <- cut(test$CoapplicantIncome, c(0,1000,2000,50000), 
                              labels=c('Low','Medium', 'High'),
                              include.lowest=T)

train$ApplicantIncome <- cut(train$ApplicantIncome, c(0,2500,6000,100000), 
                             labels=c('Low','Medium', 'High'),
                             include.lowest=T)

test$ApplicantIncome <- cut(test$ApplicantIncome, c(0,2500,6000,100000), 
                            labels=c('Low','Medium', 'High'),
                            include.lowest=T)

train$Income <- train$ApplicantIncome + train$CoapplicantIncome
train$Income_1 <- cut(train$Income, c(0,3000,8000,100000), 
                      labels=c('Low','Medium', 'High'),
                      include.lowest=T)

test$Income <- test$ApplicantIncome + test$CoapplicantIncome
test$Income_1 <- cut(test$Income, c(0,3000,8000,100000), 
                     labels=c('Low','Medium', 'High'),
                     include.lowest=T)




# replace loan amount = na with median
train$LoanAmount <- ifelse(is.na(train$LoanAmount), 
                           median(train$LoanAmount, na.rm = TRUE),train$LoanAmount)

test$LoanAmount <- ifelse(is.na(test$LoanAmount), 
                          median(test$LoanAmount, na.rm = TRUE),test$LoanAmount)

#debt to income 
train$DebtIncome <- train$LoanAmount/train$Income
test$DebtIncome <- test$LoanAmount/test$Income

# replace NA with 0 in dependent category
train$Dependents[is.na(train$Dependents)] <- 0
#train$Dependents[train$Dependents == 3+] <- 3
test$Dependents[is.na(test$Dependents)] <- 0

# replace Loan_Amount_Term NA with median
train$Loan_Amount_Term <- ifelse(is.na(train$Loan_Amount_Term), 
                                 median(train$Loan_Amount_Term, na.rm = TRUE),train$Loan_Amount_Term)
test$Loan_Amount_Term <- ifelse(is.na(test$Loan_Amount_Term), 
                                median(test$Loan_Amount_Term, na.rm = TRUE),test$Loan_Amount_Term)



# Credit_History, NA's with 0
summary(train$Credit_History)
table(train$Credit_history)
train$Credit_History <- ifelse(is.na(train$Credit_History), 0 , 
                               train$Credit_History)

test$Credit_History <- ifelse(is.na(test$Credit_History), 0 , 
                              test$Credit_History)
# makes outcome 0 or 1 for logit - 0 = No, 1 = Yes
# original data 1 = yes, 2 = No
train$Loan_Status <- ifelse(train$Loan_Status == 'Y', 1, 0)

setwd("C:/Users/dfoley/Desktop/GitHub/Competitions/Loan_Default")
save(train, file = 'train_cleaned.RData')
save(test, file = 'test_cleaned.RData')
