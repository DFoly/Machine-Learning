####################################
## Titanic Dataset
####################################
train <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Titanic/Data/train.csv")
test <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Titanic/Data/test.csv")

## Libraries
library(plyr)
library(rpart)
library(caret)
library(caTools)
library(mice)
library(stringr)
library(Hmisc)
library(ggplot2)
library(vcd)
library(ROCR)
library(pROC)
library(VIM)
library(glmnet) 

str(train)
summary(train)

# Survivors by Sex
table(train$Survived, train$Sex)

####################################
## Plot survivors by sex and age
####################################
ggplot(train, aes(x=Age, y = PassengerId, color = as.factor(Survived)))+
  geom_point() + facet_grid(Sex ~.) +
  ggtitle('Survival vs Passengers Age') +
  xlab('Age') +
  theme(legend.position = 'none') +
  scale_colour_manual(values = c('#FF0000','#0000FF'))

# we can see that red indicates death
# mostly young men died
# could create a feature based on Age

#########################################
### graph based on passenger class Pclass
#########################################
ggplot(train[train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Pclass) +
  ggtitle("Survival vs Passenger's Pclass and Port of Embarkation")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#FF0000","#0000FF"))

# alot of 3rd class passengers died an borded in southampton

ggplot(train[train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Sex) +
  ggtitle("Survival vs Passenger's Sex and Port of Embarkation")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#FF0000","#0000FF"))

####################################
#### Travelling with family matter?
####################################
# vcd package
mosaic( ~ Sex + (Age > 15) + (SibSp + Parch > 0) + Survived,
        data = train[complete.cases(train),],
        shade = T, legend = T)
# values over 2 indicate correlation
# e.g looks like Sex and age have an affect on Survival

Survived = train$Survived
test$Survived = NA
all = rbind(train, test)

#################################
## feature engineering
#################################
# Title of passenger
# split strings
# separate by comma
all$Title = sapply(all$Name,function(x) str_split(x,', ')[[1]][2])
# takes word before full stop
all$Title = sapply(all$Title,function(x) str_split(x,'\\. ')[[1]][1])

as.data.frame(
  cbind("Title" = unique(all$Title), 
        "No_of_passengers" = sapply(unique(all$Title), function(x) nrow(all[all$Title == x,])),
        "Age_missing" = sapply(unique(all$Title), function(x) nrow(all[all$Title == x & is.na(all$Age),])),
        "Minimum_Age" = sapply(unique(all$Title), function(x) min(all[all$Title == x,'Age'], na.rm = TRUE)),
        "Maximum_Age" = sapply(unique(all$Title), function(x) max(all[all$Title == x,'Age'], na.rm = TRUE))), row.names = F)

#############################################
## We can make these into smaller categories
#############################################
#   Mr:     For men above 14.5 years
#   Master: For boys below and equal to 14.5 years
#   Miss:   For girls below and equal to 14.5 years
#   Ms:     For women above 14.5 years, maybe unmarried
#   Mrs:    For married women above 14.5 years

all[(all$Title == 'Mr' & all$Age <= 14.5 & ! is.na(all$Age)),]$Title = 'Master'
all[all$Title == 'Capt' |
      all$Title == 'Col'|
      all$Title == "Don"|
      all$Title == "Major"|
      all$Title == "Rev"|      
      all$Title == "Jonkheer"|
      all$Title == "Sir",]$Title = "Mr"

# None of these women are travelling with family, hence can be categorised as single women for this analysis
all[all$Title == "Dona"|
      all$Title == "Mlle"|
      all$Title == "Mme",]$Title = "Ms"

# Categories Lady and Countess as a married woman
all[all$Title == "Lady"| all$Title == "the Countess",]$Title = "Mrs"

# Categorise doctors as per their sex
all[all$Title == "Dr" & all$Sex == "female",]$Title = "Ms"
all[all$Title == "Dr" & all$Sex == "male",]$Title = "Mr"

all$Title = as.factor(all$Title)
all$Title <- droplevels(all$Title)
summary(all$Title)

######################################
## Family Size
######################################
all$FamilySize =  ifelse(all$SibSp + all$Parch + 1 <= 3,1,0)

## Mother
all$Mother <- ifelse(all$Title == 'Mrs' & all$Parch >0 ,1,0)

# Single
all$Single <- ifelse(all$SibSp + all$Parch + 1 == 1,1,0)

# Familt name
all$FamilyName = sapply(all$Name, function(x) str_split(x, ', ')[[1]][1])
# many families with same name so we can distinguish them by ticketnumber
# all$Single == 0 is all non single ppl
Family.Ticket <- all[all$Single == 0, c('FamilyName', 'Ticket')]
# alphabetical order
Family.Ticket <- Family.Ticket[order(Family.Ticket$FamilyName),]
head(Family.Ticket)
# join last 3 digits on ticket number to FamilyName
all$FamilyName <- paste(all$FamilyName, str_sub(all$Ticket, -3,-1), sep = '')


#################################
## Family Survived
#################################
all$FamilySurvived = 0
Families <- all[(all$Parch + all$SibSp) > 0,]

# Group families by their family name and number of survivals in the family
Survival.GroupByFamilyName = aggregate(as.numeric(Families$Survived), 
                                       by=list("FamilyName" = Families$FamilyName), 
                                       FUN=sum, na.rm=TRUE)

# Family is considered to have survived if atleast one member survived
FamilyWithSurvival = Survival.GroupByFamilyName[Survival.GroupByFamilyName$x > 0,]$FamilyName
all[apply(all, 1, function(x){ifelse(x["FamilyName"] %in% FamilyWithSurvival,TRUE,FALSE)}),]$FamilySurvived = 