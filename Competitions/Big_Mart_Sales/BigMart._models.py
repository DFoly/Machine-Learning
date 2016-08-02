# -*- coding: utf-8 -*-
"""
Created on Fri Jul 29 12:14:16 2016

@author: dfoley
"""
#==============================================================================
# Machine learning modelling on our cleaned dataset
#==============================================================================
import pandas as pd


train = pd.read_csv('C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_Sales/train_modified.csv')
test = pd.read_csv('C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_Sales/test_modified.csv')

train.columns


# make a function which takes in algorithm and performs cross validation
# define target and ID columns

target = 'Item_Outlet_Sales'
IDcol = ['Item_Identifier', 'Outlet_Identifier']
from sklearn import cross_validation, metrics
import numpy as np

def modelfit(alg, dtrain, dtest, predictors, target, IDcol, path ,filename):
    
    # fit algorithm
    alg.fit(dtrain[predictors], dtrain[target])
    
    # predict
    dtrain_predictions = alg.predict(dtrain[predictors])
    
    # cross validation
    cv_score = cross_validation.cross_val_score(alg, dtrain[predictors], dtrain[target],
                                                cv = 20, scoring = 'mean_squared_error')
    #RMSE    
    cv_score = np.sqrt(np.abs(cv_score))
    
    # print model report
    print('\nModel Report')
    print('RMSE: %.4g'% np.sqrt(metrics.mean_squared_error(dtrain[target].values, dtrain_predictions)))
    print ("CV Score : Mean - %.4g | Std - %.4g | Min - %.4g | Max - %.4g" % (np.mean(cv_score),
                                                                              np.std(cv_score),np.min(cv_score),np.max(cv_score)))
    
#test set predicions                                                                          
    dtest[target] = alg.predict(dtest[predictors])
    
    # export submission file
    IDcol.append(target)
    submission = pd.DataFrame({x : dtest[x] for x in IDcol})
    submission.to_csv(path + filename, index = False)
    
#==============================================================================
# We will try linear regression, lasso and Ridge regression first
#==============================================================================

from sklearn.linear_model import LinearRegression, Ridge, Lasso

path = 'C:/Users/dfoley/Dropbox/Hackathon/Big_Mart_Sales/'

# put all columns except target variable in predictors
predictors = [x for x in train.columns if x not in [target] + IDcol]

# print predictors
alg1 = LinearRegression(normalize = True)
modelfit(alg1, train, test, predictors, target, IDcol, path,'alg1.csv')
coef1 = pd.Series(alg1.coef_, predictors). sort_values()
coef1.plot(kind = 'bar', title = 'Model Coefficients')


predictors = [x for x in train.columns if x not in [target] + IDcol]
alg2 = Ridge(alpha = 0.03, normalize = True)
modelfit(alg2, train, test, predictors, target, IDcol, path,'alg2.csv')
coef2 = pd.Series(alg2.coef_, predictors).sort_values()
coef2.plot(kind ='bar', title = 'Model Coeffcients')


# Decisison Tree
from sklearn.tree import DecisionTreeRegressor

predictors = [x for x in train.columns if x not in [target] + IDcol]
alg3 = DecisionTreeRegressor(max_depth = 15, min_samples_leaf = 100)
modelfit(alg3, train, test, predictors, target, IDcol, path, 'alg3.csv')
coef3 = pd.Series(alg3.feature_importances_, predictors).sort_values(ascending = False)
coef3.plot(kind = 'bar', title = 'Fetaure Importances')



#Random Forest
from sklearn.ensemble import RandomForestRegressor
predictors = [x for x in train.columns if x not in [target]+IDcol]
alg5 = RandomForestRegressor(n_estimators = 400, max_depth = 6, min_samples_leaf= 100, n_jobs = 4)
modelfit(alg5, train, test, predictors, target, IDcol,path, 'alg5.csv')
coef5 = pd.Series(alg5.feature_importances_, predictors).sort_values(ascending = False)
coef5.plot(kind = 'bar', title = 'Feature IMportances')

#########################
# Tune hyperparamters
#########################
from sklearn.grid_search import GridSearchCV
param_grid = {'learning_rate': [0.1,0.05,0.02,0.01],
              'max_depth': [4,6],
              'min_samples_leaf': [3,5,9,17],
              'max_features': [1.0,0.3,0.1]}
              
est = GradientBoostingRegressor(n_estimators = 3000)
gs_cv = GridSearchCV(est, param_grid).fit(train[predictors], train[target]) 
gs_cv.best_params_             


# Ensemble GB Regression
from sklearn.ensemble import GradientBoostingRegressor
predictors = [x for x in train.columns if x not in [target]+IDcol]
alg7 = GradientBoostingRegressor(learning_rate = 0.4,n_estimators = 400, subsample = 0.5 
        ,max_depth = 6, max_features = 5,min_samples_leaf= 100)
modelfit(alg7, train,test, predictors, target, IDcol, path, 'alg7.csv')
coef6 = pd.Series(alg6.feature_importances_, predictors).sort_values(ascending = False)
coef6.plot(kind = 'bar', title = 'Fetaure Importances')


# Next try XGboost
import numpy as np
import xgboost as xgb
#from xgboost.sklearn import XGBClassifier
data = np.random.rand(5,10) # 5 entities, each contains 10 features
label = np.random.randint(2, size=5) # binary target
dtrain = xgb.DMatrix( data, label=label)

dtest = dtrain

param = {'bst:max_depth':2, 'bst:eta':1, 'silent':1, 'objective':'binary:logistic' }
param['nthread'] = 4
param['eval_metric'] = 'auc'

evallist  = [(dtest,'eval'), (dtrain,'train')]

num_round = 10
bst = xgb.train( param, dtrain, num_round, evallist )