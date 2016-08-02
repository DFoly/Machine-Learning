################
# Big_Mart_sales GBM
################
train<- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_Sales/train_modified.csv")
test <- read.csv("C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_Sales/test_modified.csv")

library(caret)
library(Metrics)
library(gbm)
set.seed(999)

fitControl <- trainControl(method = 'repeatedcv', number =4, repeats = 4)

predictors <- c(Item_MRP,
 Item_Visibility,
                Item_Weight,
                Item_Visibility_Ratio,
                Outlet_Years,
                Item_Fat_Content_0,
                Item_Fat_Content_1,
                Item_Fat_Content_2,
                Outlet_Location_Type_0,
                Outlet_Location_Type_1,
                Outlet_Location_Type_2,
                Outlet_Size_0,
                Outlet_Size_1,
                Outlet_Size_2,
                Outlet_Type_0,
                Outlet_Type_1',
                'Outlet_Type_2',
                'Outlet_Type_3',
                'Item_Type_Combined_0',
                'Item_Type_Combined_1',
                'Item_Type_Combined_2',
                'Outlet_0',
                'Outlet_1',
                'Outlet_2',
                'Outlet_3',
                'Outlet_4',
                'Outlet_5',
                'Outlet_6',
                'Outlet_7',
                'Outlet_8',
                'Outlet_9)
predictors = train[,-1]
formula = Item_Outlet_Sales ~ train[,-1]


gbm_formula <- as.formula(paste0('Item_Outlet_Sales ~' ,paste(colnames(train[, -c(1,3)]),collapse = '+'  )))


GBM_model = gbm(gbm_formula ,data=train, n.trees=50,shrinkage=0.05 ,cv.folds=10)
best.iter <- gbm.perf(GBM_model,method="cv")
summary(GBM_model)
Item_Outlet_Sales <- predict.gbm(GBM_model,test,best.iter)

ID <- as.data.frame(test$Item_Identifier)
OutletID <- as.data.frame(test$Outlet_Identifier)
result <- as.data.frame(Item_Outlet_Sales)



results <- cbind(ID,result ,OutletID )
setwd("C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_Sales")
write.csv(results, file = 'gbmalg2.csv', row.names = FALSE)
