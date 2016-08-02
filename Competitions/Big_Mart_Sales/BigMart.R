##################################
## Big Mart Sales
##################################
train <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_sales/train.csv",
                  header = T, na.strings=c("",".","NA"))

test <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_sales/test.csv",
                 header = T, na.strings=c("",".","NA"))

train <- read.csv("~/Dropbox/Hackathon/Big_Mart_Sales/train_modified.csv")
test <- read.csv("~/Dropbox/Hackathon/Big_Mart_Sales/test_modified.csv")

# perform operations on whole data set
# remember to split at row 5681
data <- rbind(train[,1:11], test)
summary(data);str(data)

# missing values in item weight and outlet size
# need to impute these.

#################################
# plot correlations of variables
#################################
 temp <- cor(train[,unlist(lapply(train, is.numeric))], data$Item_Outlet_sales)
# doesnt seem to help too much

# replace item weight NA's with median weight
data$Item_Weight <- ifelse(is.na(data$Item_Weight), median(data$Item_Weight, na.rm = TRUE), data$Item_Weight)
summary(data$Item_Weight)

# could use outlet type to figure out size
# Grocery store is more likely to be small
library(ggplot2)
ggplot(data, aes(x = Outlet_Size, fill = factor(Outlet_Type))) + geom_bar()
# looks like majority of super markets type 1 and grocery stores are small
# so we could replace with with small


data[(is.na(data$Outlet_Size) & data$Outlet_Type == 'Grocery Store'),]$Outlet_Size = 'Small'
data[(is.na(data$Outlet_Size) & data$Outlet_Type == 'Supermarket Type1'),]$Outlet_Size = 'Small'
# redo plot
ggplot(data, aes(x = Outlet_Size, fill = factor(Outlet_Type))) + geom_bar()

# split into training and test again
train1 <- data[1:8523,-1]
train1$Outlet_Sales <- train[,12]

test1 <- data[8524: nrow(data),]


reg1 <- lm(Outlet_Sales ~., data = train1)
summary(reg1)
# terrible fit.
# need to so some more data cleaning and feature engineering

# boxplot of sales by outlet type
ggplot(train1, aes(x = factor(Outlet_Type), y = Outlet_Sales)) +
  geom_boxplot() + geom_hline(aes(yintercept = mean(Outlet_Sales)),
                              linetype = 'dashed', lwd = 1)
# supermarket type 3 has the most sales
# probably as they are the biggest
# There are also quite a few outliers so may want to log transform 
# and standardise
mean(data$Outlet_Sales[train1$Outlet_Type == 'Grocery Store'])
mean(data$Outlet_Sales[train1$Outlet_Type == 'Supermarket Type1'])
mean(data$Outlet_Sales[train1$Outlet_Type == 'Supermarket Type2'])
mean(data$Outlet_Sales[train1$Outlet_Type == 'Supermarket Type3'])

#################
# Item visibility
#################
# repllace 0 with mean
summary(data$Item_Visibility)
data$Item_Visibility <- ifelse(data$Item_Visibility == 0, 
                               mean(data$Item_Visibility), data$Item_Visibility)
summary(data$Item_Visibility)

# create ratio of item visibility / avg visibility
# idea being that higher ratios will be more visible and therefore sell more

data$Visibility_Ratio <- data$Item_Visibility/mean(data$Item_Visibility)
summary(data$Visibility_Ratio)


############################################
# create categories using Item Identifier
############################################
# all items seem to begin iwth either FD, DR and NC
# which could be food, drinks and non consumables respectively
data$Ident <- (substr(data$Item_Identifier, 1,2))
summary(data$Ident)
data$Ident[data$Ident == 'FD'] <- 'Food'
data$Ident[data$Ident == 'NC'] <- 'Non_Consumables'
data$Ident[data$Ident == 'DR'] <- 'Drinks'
data$Ident <- as.factor(data$Ident)

# calculate years store has been active
data$Years_Active <- 2013 - data$Outlet_Establishment_Year


#####################################
## change fat content - looks like we have multiple categories
# that mean the same thing e.g LF low fat and Low Fat
# and reg Regular
summary(data$Item_Fat_Content)
data$Item_Fat_Content <- as.character(data$Item_Fat_Content)
data$Item_Fat_Content[data$Item_Fat_Content == 'LF'] <- 'Low Fat'
data$Item_Fat_Content[data$Item_Fat_Content == 'low fat'] <- 'Low Fat'
data$Item_Fat_Content[data$Item_Fat_Content == 'reg'] <- 'Regular'
data$Item_Fat_Content <- as.factor(data$Item_Fat_Content)


#################################################################
# Now do one hot encoding aka rather than categorical variables
# we have separate dummies for each category
# This can sometimes capture more information
#################################################################
# want to perform this for
# Item_Fat_Content
# Outlet_Location_Type
# Outlet_Size
# Ident
# Outlet_Type
# Outlet
dummies = c('Item_Fat_Content', 'Outlet_Location_Type', 
            'Outlet_Size', 'Ident', 'Outlet_Type',
            'Outlet')

#for(t in dummies){
#  data[paste('dummies', t, sep = '')] <- ifelse(data$dummies == t,1,0)
#}

data$IFC_Low <- ifelse(data$Item_Fat_Content == 'Low Fat', 1,0)
data$ITF_Reg <- ifelse(data$Item_Fat_Content == 'Regular', 1,0)

data$OT_Groc <- ifelse(data$Outlet_Type == 'Grocery Store',1,0)
data$OT_S1 <- ifelse(data$Outlet_Type == 'Supermarket Type1',1,0)
data$OT_S2 <- ifelse(data$Outlet_Type == 'Supermarket Type2',1,0)
data$OT_S3 <- ifelse(data$Outlet_Type == 'Supermarket Type3',1,0)


data$OSSmall <- ifelse(data$Outlet_Size == 'Small',1,0)
data$OSMed <- ifelse(data$Outlet_Size == 'Medium',1,0)
data$OSHigh <- ifelse(data$Outlet_Size == 'High',1,0)


data$Drinks <- ifelse(data$Ident == 'Drinks',1,0)
data$Food <- ifelse(data$Ident == 'Food',1,0)
data$NC <- ifelse(data$Ident == 'Non_Consumables',1,0)


data$Tier1 <- ifelse(data$Outlet_Location_Type =='Tier1',1,0)
data$Tier2 <- ifelse(data$Outlet_Location_Type =='Tier2',1,0)
data$Tier3 <- ifelse(data$Outlet_Location_Type =='Tier3',1,0)

data$Outlet <- data$Outlet_Identifier

data$Outlet1 <- ifelse(data$Outlet == 'OUT010', 1,0)
data$Outlet2 <- ifelse(data$Outlet == 'OUT013', 1,0)
data$Outlet3 <- ifelse(data$Outlet == 'OUT017', 1,0)
data$Outlet4 <- ifelse(data$Outlet == 'OUT018', 1,0)
data$Outlet5 <- ifelse(data$Outlet == 'OUT019', 1,0)
data$Outlet6 <- ifelse(data$Outlet == 'OUT027', 1,0)
data$Outlet7 <- ifelse(data$Outlet == 'OUT035', 1,0)
data$Outlet8 <- ifelse(data$Outlet == 'OUT045', 1,0)
data$Outlet9 <- ifelse(data$Outlet == 'OUT046', 1,0)

############################################
## split into training and test sets
############################################
train1 <- data[1:8523,-1]
train1$Outlet_Sales <- data[1:8523,12]

test1 <- data[8524: nrow(data),]

predictors <- data.frame(train1$IFC_Low, train1$ITF_Reg, train1$OT_Groc, train1$OT_S1,
                         train1$OT_S2, train1$OT_S3, train1$OSSmall, train1$OSMedium,
                         train1$OSHigh, train1$Drinks ,train1$Food, train1$NC,
                         train1$Tier1, train1$Tier2, train1$Tier3,
                         train1$Outlet, train1$Outlet1, train1$Outlet2, train1$Outlet3, train1$Outlet4,
                         train1$Outlet5, train1Outlet6, train1$Outlet7, train1$Outlet8 ,train1$Outlet9)

reg2 <- lm(Outlet_Sales ~ IFC_Low + ITF_Reg + OT_Groc +OT_S1 + 
             OT_S2 + OT_S3 + OSSmall + OSMedium + OSHigh + Drinks +
             Food + NC + Tier1 + Tier2 + Tier3 + Outlet +
             Outlet1 +  Outlet2  + Outlet3 + Outlet4  + Outlet5  + Outlet5 
           + Outlet7  + Outlet8  + Outlet9 + )


library(xgboost)
param <- list('booster' = "gblinear", 'objective' = 'reg:linear','eval_metric' = 'rmse',
              'eta' = 0.4, 'max.depth'  = 5)
param1 <- list( 'objective' = 'reg:linear','eval_metric' = 'rmse',
              'eta' = 0.4, 'max.depth'  = 5)
# getting error as all variables must be numeric
train$Item_MRP <- as.numeric(train$Item_MRP)
train$Item_Outlet_Sales <- as.numeric(train$Item_Outlet_Sales)
train$Item_Visibility <- as.numeric(train$Item_Visibility)
train$Item_Weight <- as.numeric(train$Item_Weight)
train$Item_Visibility_Ratio <- as.numeric(train$Item_Visibility_Ratio)
train$Outlet_Years <- as.numeric(train$Outlet_Years )

test$Item_MRP <- as.numeric(test$Item_MRP)
test$Item_Visibility <- as.numeric(test$Item_Visibility)
test$Item_Weight <- as.numeric(test$Item_Weight)
test$Item_Visibility_Ratio <- as.numeric(test$Item_Visibility_Ratio)
test$Outlet_Years <- as.numeric(test$Outlet_Years)


best.cv = xgb.cv(param = param, data = as.matrix(train[,-c(1,3,6)]), 
                 label = train[,3], nfold = 10, nround = 200)

bst <- xgboost(param = param, data = as.matrix(train[,-c(1,3,6)]), 
               label = train[,3], nround = 100)
pred <- predict(bst, as.matrix(test[,-c(1,5)]))

ID <- as.data.frame(test$Item_Identifier)
ID_Out <- as.data.frame(test$Outlet_Identifier )
results <- data.frame(ID, pred, ID_Out)
names(results) <- c('Item_Identifier', 'Item_Outlet_Sales ', 'Outlet_Identifier')

setwd("C:/Users/dfoley/Dropbox/Hackathon/Loan_default")
setwd("~/Dropbox/Hackathon/Big_Mart_Sales")
write.csv(results, file = 'xgboost2.csv', row.names = FALSE)


bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")



