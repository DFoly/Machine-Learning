################################
## Titanic Attampt 1
################################
train <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Titanic/Data/train.csv")
test <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Titanic/Data/test.csv")

train <- read.csv("~/Dropbox/Hackathon/Titanic/Data/train.csv", na.string = c('NA', ''))
test <- read.csv("~/Dropbox/Hackathon/Titanic/Data/test.csv", na.string = c('NA', ''))

str(train)
summary(train)

#######################
# Initial Exploration
######################
library(ggplot2)

table(train$Survived) # most died
barplot(table(train$Survived),
        names.arg = c('Perished', 'Survived'),
        main = 'Died or Survived', col = 'black')




ggplot(train, aes(x =  Age, y = Sex)) + 
  geom_point()

# Survival by Pclass
# more likely to Die if in lower classes
ggplot(train, aes(as.factor(Pclass), fill=as.factor(Survived)))+ 
  geom_bar()

# Survival by Sex
ggplot(train, aes(as.factor(Sex), fill=as.factor(Survived)))+geom_bar()

#Survival by Age
ggplot(train, aes(x = Age), fill=as.factor(Survived))+geom_bar()

# Embarked, wihtout missing values
# Most ppl left from S
ggplot(train[train$Embarked != '',], aes(x = Embarked), fill=as.factor(Survived))+geom_bar()



barchart <- ggplot(train, aes(as.factor(Pclass), 
                                fill=as.factor(Survived)))+geom_bar()

## create labels for Axis
barchart+xlab("Passenger Class")+ylab("Number of Passengers")+ 
  ggtitle("Survival by Passenger Class and Gender")+
  scale_fill_discrete(name = "", labels = c("Died", "Survived"))


####################################################
## Looks like Sex Age and Pclass matter most
####################################################
# We could probably use Name to construct variablw
# Family may be important

#######################
# Missing Values
#######################
library(Amelia)
missmap(train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
# Age is missing alot
# Cabin missing most data
# could maybe identify Cabin from Second Name or ticket later

summary(train$Age) # 177 NA's

# Maybe we can use other variables to estimate Age?
# Could use Name title as that depends on Age
head(train$Name, n =10L)

#####################
## New data set
#####################
Survived = train$Survived
test$Survived = NA
train_new = rbind(train, test)


###########################
## Extract titles
#########################
library(stringr)
# pulls out names after comma into one string
# [[1]][2] pulls out last name and title, [[1]][1] qould just pull last name
train_new$Title <- sapply(train_new$Name, function(x) str_split(x,', ')[[1]][2])
# pulls out title from Title column
train_new$Title <- sapply(train_new$Title, function(x) str_split(x, '\\. ')[[1]][1])
unique(train_new$Title)

# want titles that are missing an Age
library(Hmisc)
# bystats gives missing values
bystats(train_new$Age, train_new$Title,
        fun = function(x) c(Mean = mean(x), Median = median(x)))

# We can create a function that applies impute from Hmisc
# to median value of Agwe based on Title
boxplot(train_new$Age ~ train_new$Title, col = 'blue')

titles.na.train <- c('Dr', 'Master', 'Mrs', 'Miss', 'Mr', 'Ms')



##############################################################
#imputeMed <- function(impute.var, filter.var, var.levels){
#  for (v in var.levels){
#    impute.var[which(filter.var == v)] <- impute(impute.var[
#      which(filter.var == v)])
#  }
#  return(impute.var)
#}

#train_new$Age[which(train_new$Title == 'Dr')]
#train_new$Age <- imputeMed(train_new$Age, train_new$Title, 
#                           titles.na.train)
#train_new$Age[which(train_new$Title == 'Dr')]
#summary(train_new$Age) # 1 NA
## passengerID 980 - Ms
#train_new[which(is.na(train_new$Age)),] 
#train_new[which(train_new$PassengerId == 980),]

# replace with median age from Title Ms
#train_new$Age[which(train_new$PassengerId == 980)] <- median(train_new$Age[which(train_new$Title =='Ms')],na.rm = TRUE)
#summary(train_new$Age)

#################################
# Put Title into smaller groups
#################################
#   Mr:     For men above 14.5 years
#   Master: For boys below and equal to 14.5 years
#   Miss:   For girls below and equal to 14.5 years
#   Ms:     For women above 14.5 years, maybe unmarried
#   Mrs:    For married women above 14.5 years



train_new[(train_new$Title == "Mr" & train_new$Age <= 14.5 & !is.na(train_new$Age)),]$Title = "Master"


train_new[train_new$Title == "Capt"|
            train_new$Title == "Col"|
            train_new$Title == "Don"|
            train_new$Title == "Major"|
            train_new$Title == "Rev"|      
            train_new$Title == "Jonkheer"|
            train_new$Title == "Sir",]$Title = "Mr"


# group to single women
train_new[train_new$Title == "Dona"|
            train_new$Title == "Mlle"|
            train_new$Title == "Mme",]$Title = "Ms"


# Categories Lady and Countess as a married woman
train_new[train_new$Title == "Lady"| train_new$Title == "the Countess",]$Title = "Mrs"


# Categorise doctors as per their sex
train_new[train_new$Title == "Dr" & train_new$Sex == "female",]$Title = "Ms"
train_new[train_new$Title == 'Dr' & train_new$Sex == 'male',]$Title = 'Mr'

train_new$Title = as.factor(train_new$Title)
train_new$Title <- droplevels(train_new$Title)
summary(train_new$Title)


##################################
## Embarked
##################################
summary(train_new$Embarked) # 2 missing
train_new[which(train_new$Embarked == ''),]
train_new$Embarked[which(is.na(train_new$Embarked))] <- 'S'
# wont let me replace with S



##########################
## Family Size
##########################
# create big or small family
summary(train_new$SibSp)
summary(train_new$Parch)
###########################################
# define 3 or less ppl in family as small
############################################
train_new$FamilySize <- ifelse(train_new$SibSp + train_new$Parch + 1 <= 3,1,0)

######################################################################
# identify ladies with children as they are more likely to survive
######################################################################
train_new$Mother <- ifelse(train_new$Title == 'Mrs' & train_new$Parch > 0,1,0)

##########################
# ppl travelling solo
##########################
train_new$solo <- ifelse(train_new$SibSp + train_new$Parch + 1 == 1,1,0)

#################
## Family Name
#################
train_new$FamilyName <- sapply(train_new$Name, function(x) str_split(x, ',')[[1]][1])
# many ppl with same name so we could use ticket number to identify them
Family.Ticket <- train_new[train_new$solo == 0, c('FamilyName', 'Ticket')]
#alphabetical order
Family.Ticket <- Family.Ticket[order(Family.Ticket$FamilyName),]
# looks like Families have same ticket number
# so we could put ticket number digits on end of family name
train_new$FamilyName <- paste(train_new$FamilyName, str_sub(train_new$Ticket, -3,-1) ,sep = "")


#####################
### Family Survived
#####################
train_new$FamilySurvived = 0
# variable of passengers with families
Families <- train_new[(train_new$SibSp + train_new$Parch) > 0 ,]

# Group families by Name and number of survivals in family
Survival.FamName <- aggregate(as.numeric(Families$Survived),
                              by = list('FamilyName'= Families$FamilyName),
                              FUN = sum, na.rm  = TRUE)

# Assume Family survived if at least one member survived
FamilySurvival <- Survival.FamName[Survival.FamName$x > 0,]$FamilyName
# Match Family names that survived with main dataset and male
# FamilySurvived column 1
train_new[apply(train_new,1,function(x){
  ifelse(x['FamilyName'] %in% FamilySurvival, TRUE, FALSE)
}),]$FamilySurvived = 1


###########################
## Fare
###########################
summary(train_new$Fare)
train_new[which(train_new$Fare == 0),]
# all seem to be males so replace with median
boxplot(train_new$Fare ~ train_new$Pclass)
# seem to be from variety of classes so just replace with overall median
# medmale <- median(train_new$Fare[which(train_new$Sex == 'male')],na.rm = TRUE)
train_new$Fare[which(train_new$Fare == 0)] = median(train_new$Fare, na.rm = TRUE)
# replace NA with median
train_new$Fare[which(is.na(train_new$Fare))] = median(train_new$Fare, na.rm = TRUE)



#################################
# split age in categories
#################################
train_new$AgeClass <- ifelse(train_new$Age <= 10,1,
                             ifelse(train_new$Age > 10 & train_new$age <= 20, 2,
                                    ifelse(train_new$Age > 20 & train_new$Age < 35,3,4)))
train_new$AgeClass <- as.factor(train_new$AgeClass)

# This isnt working
train_new[is.na(train_new$AgeClass),]$AgeClass = ifelse(train_new[is.na(train_new$AgeClass),]$Age<=10,1,
ifelse(train_new[is.na(train_new$AgeClass),]$Age>10 & train_new[is.na(train_new$AgeClass),]$Age<=20,2,
ifelse(train_new[is.na(train_new$AgeClass),]$Age>20 & train_new[is.na(train_new$AgeClass),]$Age<=35,3,4)))

#######################################
## Imputting  Age missing values
#######################################
library(mice)
age.df <- mice(train_new[, !names(train_new) %in% 
                           c("Survived", "Name", "PassengerId", "Ticket", "AgeClass", "Cabin", "FamilyName")],
                            m=8,maxit=8,meth='pmm',seed=123)
head(age.df$imp$Age)

# Check if the imputed data distribution follows the existing age distribution.
# check each dataset to see which dist looks the best
# looks like number 6 is
ggplot(train_new,aes(x=Age)) + 
  geom_density(data=data.frame(train_new$PassengerId, complete(age.df,6)), alpha = 0.2, fill = "blue")+
  geom_density(data=train_new, alpha = 0.2, fill = "Red")+
  labs(title="Age Distribution")+
  labs(x="Age")

train_new_imp <- data.frame(train_new$PassengerId, complete(age.df,6))
train_new$Age = train_new_imp$Age



# make females over 15 Ms
train_new[train_new$Title == 'Miss' & train_new$Age > 14.5 ,]$Title = 'Ms'
# Check if titles and age are as required.
table(train_new$Title, train_new$Age > 14.5)
train_new$Mr <- ifelse(train_new$Title == 'Mr',1,0)

###########################################
## To impute Cabin Numbers
## Based on families staying in same cabin
###########################################
summary(train_new$Cabin)
train_new$Cabin <-as.character(train_new$Cabin)

train_new$Cabin[train_new$Pclass == 1]
train_new$Cabin[train_new$Pclass == 2]
train_new$Cabin[train_new$Pclass == 3]

train_new[train_new$Pclass == 1,]$Cabin = 'A'
train_new[train_new$Pclass == 2,]$Cabin = 'B'
train_new[train_new$Pclass == 3,]$Cabin = 'C'

train_new$Cabin <- as.factor(train_new$Cabin)


####################
## Fare per person

train_new$FPP <- train_new$Fare/(train_new$SibSp + train_new$Parch + 1)
train_new$Fare_norm <- scale(train_new$Fare)
train_new$Age_norm <- scale(train_new$Age)
train_new$AgeClass <- train_new$Age * train_new$Pclass
train_new$AgeClass_norm <- scale(train_new$AgeClass)

train_new$CabinA <- ifelse(train_new$Cabin == 'A',1,0)
train_new$CabinB <- ifelse(train_new$Cabin == 'B',1,0)
train_new$CabinC <- ifelse(train_new$Cabin == 'C',1,0)

train_new$Sex <- ifelse(train_new$Sex == 'male',1,0)
###################################
## Complete Datset
###################################
summary(train_new)
save(train_new, file = 'train_cleaned.RData')

# Data is cleaned up and NA's are gone

##################################
## Training and test Set
#################################
# subset before NA's: NA's begin at 892
train  = train_new[1:891,]
test = train_new[892:1309,]

# training and test sets for non Na data
train$gp <- runif(dim(train)[1])
testSet <- subset(train, train$gp  <= 0.1)
trainingSet <- subset(train, train$gp > 0.1)

library(plyr)
trainingSet$Survived <- as.factor(trainingSet$Survived)
trainingSet$Survived <- as.factor(mapvalues(trainingSet$Survived, c("0", "1"), c("No","Yes")))
trainingSet$PassengerId = NULL

##############################
# Logistic regression
##############################

reg1 <- glm(Survived ~ Sex + Age + Pclass +
              FamilySize + Title + Parch + SibSp +
              FamilySurvived,  family = binomial(link = 'logit') ,
            data = trainingSet)
summary(reg1)

fitted.results <- predict(reg1, newdata = testSet, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testSet$Survived)
print(paste('Accuracy',1-misClasificError))

fitted.results.submit <- predict(reg1, newdata = test, type = 'response')
fitted.results.submit <- ifelse(fitted.results.submit > 0.5,1,0)

ID <- as.data.frame(test$PassengerId)
results <- cbind(ID, fitted.results.submit)
names(Results) <- c('PassengerID', 'Survived')

setwd("C:/Users/dfoley/Dropbox/Hackathon/Titanic")
write.csv(results, file = "results_1stAttempt.csv", row.names = FALSE)

##########################################
## Logistic part 2
##########################################

reg_sm <- glm(Survived ~ Pclass + Sex + Age + FamilySize + Mr +
                FamilySurvived,family = binomial(link = 'logit'),
             data = trainingSet)
summary(reg_sm)
fitted.results11 <- predict(reg_sm, newdata = testSet, type = 'response')
fitted.results11 <- ifelse(fitted.results11 > 0.5,1,0)

misClasificError <- mean(fitted.results11 != testSet$Survived)
print(paste('Accuracy',1-misClasificError))




reg11 <- glm(Survived ~  Pclass + 
              FamilySize + Mr + Parch + SibSp +
              FamilySurvived,  family = binomial(link = 'logit') ,
            data = trainingSet)
summary(reg11)

fitted.results11 <- predict(reg11, newdata = testSet, type = 'response')
fitted.results11 <- ifelse(fitted.results11 > 0.5,1,0)

misClasificError12 <- mean(fitted.results11 != testSet$Survived)
print(paste('Accuracy',1-misClasificError12))


reg_new <- glm(Survived ~ Pclass + Age + Title + FamilySize + FamilySurvived +
                    FPP + CabinA  + Fare_norm + AgeClass,
                    family = binomial(link = 'logit') ,
                    data = trainingSet)
summary(reg_new)
anova(reg_new)

fitt <- predict(reg_new, newdata = testSet, type = 'response')
fitt <- ifelse(fitt > 0.5,1,0)

misClasificError_new <- mean(fitt != testSet$Survived)
print(paste('Accuracy',1-misClasificError_new))




#########################
## Ridge Model
#########################
library(glmnet)
x.m = data.matrix(trainingSet[,c(3,5,6,7,8,12,13,14,18)])
y.m = trainingSet$Survived
cvfit.m.ridge = cv.glmnet(x.m, y.m,
                          family = "binomial", 
                          alpha = 0,
                          type.measure = "class")
plot(cvfit.m.ridge, main = "Ridge")
summary(cvfit.m.ridge)
coef(cvfit.m.ridge, s = "lambda.min")


pred2 = predict(cvfit.m.ridge, s = 'lambda.min',
               newx = data.matrix(testSet[,c(3,5,6,7,8,12,13,14,18)]), 
               type="class")

misClasificError_ridge <- mean(pred2 != testSet$Survived)
print(paste('Accuracy',1-misClasificError_ridge))



###################################
## RandomForest
###################################
library(randomForest)
set.seed(111)
rf1 <- randomForest(as.factor(Survived) ~ Pclass +
                      FamilySize + Title + Parch + SibSp +
                      FamilySurvived, data = trainingSet,
                    importance = TRUE,
                    ntree = 2000)
varImpPlot(rf1)

#################################
## Test Set Prediction
##################################
res <- predict(rf1, newdata = testSet, type = 'response')
res <- ifelse(res == 'Yes',1,0)

misClasificError_rf <- mean(res != testSet$Survived)
print(paste('Accuracy',1-misClasificError_rf))




Prediction <- predict(rf1, test)
Prediction <- ifelse(Prediction == 'Yes',1,0)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)




################################
## Ensemble
################################
# Best Result with this model
library(party)
set.seed(333)
fit1 <- cforest(as.factor(Survived) ~ Sex + Age + Pclass +
                  FamilySize + Title + Parch + SibSp +
                  FamilySurvived,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction1 <- predict(fit1, test, OOB=TRUE, type = "response")
misClasificError_cf <- mean(Prediction1 != testSet$Survived)
print(paste('Accuracy',1-misClasificError_cf))

submit1 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction1)
setwd("C:/Users/dfoley/Dropbox/Hackathon/Titanic")
write.csv(submit1, file = "cforest.csv", row.names = FALSE)


# with new variables
fit2 <- cforest(as.factor(Survived) ~ Pclass + Age + Title + FamilySize + FamilySurvived +
                              FPP + CabinA  + Fare_norm + AgeClass + SibSp + Parch,
                data = train, 
                controls=cforest_unbiased(ntree=2000, mtry=3))

Predict <- predict(fit2, testSet, OOB=TRUE, type = "response")
misClasificError_cf <- mean(Predict != testSet$Survived)
print(paste('Accuracy',1-misClasificError_cf))
# 0.8709

Prediction1 <- predict(fit2, test, OOB=TRUE, type = "response")
misClasificError_cf <- mean(Prediction1 != testSet$Survived)
print(paste('Accuracy',1-misClasificError_cf))

submit1 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction1)
setwd("C:/Users/dfoley/Dropbox/Hackathon/Titanic")
setwd("~/Dropbox/Hackathon/Titanic")
write.csv(submit1, file = "cforest_new.csv", row.names = FALSE)
