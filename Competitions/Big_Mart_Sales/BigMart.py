# -*- coding: utf-8 -*-
"""
Created on Thu Jul 28 14:20:54 2016

@author: dfoley

Competition to predict Sales from a super market

"""
import numpy as np
import pandas as pd

# READ files 

train = pd.read_csv('C:/Users/dfoley/Desktop/GitHub/Competitions/Big_Mart_Sales/train.csv')
test = pd.read_csv('C:/Users/dfoley/Desktop/GitHub/Competitions/Big_Mart_Sales/test.csv')

train.head()

# combine train and test sets for feature engineering
train['source'] = 'train'
test['source'] = 'test'

#train.append(test)
data = pd.concat([train, test], ignore_index = True)
print(test.shape,train.shape, data.shape)


# check for missing values
data.apply(lambda x: sum(x.isnull()))
data.describe()

"""
item_Visibility 0 seems quite strange, may need to impute that
Could convert year estiablished into years in operation which may have better predictive power
we will need to fix NaN values
"""

data.apply(lambda x: len(x.unique()))
# from this we see there are 1559 different products and 10 different stores

"""
Lets look at the relative frequency of the variables
"""
#Filter categorical variables
categorical_columns = [x for x in data.dtypes.index if data.dtypes[x]=='object']
#Exclude ID cols and source:
categorical_columns = [x for x in categorical_columns if x not in ['Item_Identifier','Outlet_Identifier','source']]
#Print frequency of categories
for col in categorical_columns:
    print ('\nFrequency of Categories for varible %s'%col)
    print (data[col].value_counts())  
    
"""
We can see from this that we can aggregate some of the fat content variables 
as they are just spelled differently.
We could also probably make Item_type int smaller cateogories like food, drink and non consumables
Could combine the supermarkets together
"""  
 # Determine average weight per item
# 2 different methods: prefer 2nd one
item_avg_weight = data.pivot_table(values = 'Item_Weight', index = 'Item_Identifier' )

item_avg_weight = data.groupby('Item_Identifier')['Item_Weight'].mean()

#Get a boolean variable specifying missing Item_Weight values
miss_bool = data['Item_Weight'].isnull() 

#Impute data and check #missing values before and after imputation to confirm
print ('Orignal #missing: %d'% sum(miss_bool))
data.loc[miss_bool,'Item_Weight'] = data.loc[miss_bool,'Item_Identifier'].apply(lambda x: item_avg_weight[x])
print ('Final #missing: %d'% sum(data['Item_Weight'].isnull()))


# Replace Outlet_Size missing values with mode i.e. most frequent value
from scipy.stats import mode

# Find mode for each type
outlet_size_mode = data.pivot_table(values='Outlet_Size', columns='Outlet_Type',aggfunc=(lambda x:mode(x).mode[0]))
data.dtypes

data[(data['Outlet_Size'] == 'Small') & (data['Outlet_Type'] == 'Grocery Store')]
data.loc[(data['Outlet_Size'] == 'Small') & (data['Outlet_Type'] == 'Grocery Store')] 

# Take out missing outlet_size that is a grocery store and replaace with Small
# returns boolean if we leave out []
miss_bool = (data['Outlet_Size'].isnull()) & (data['Outlet_Type'] == 'Grocery Store')
#Impute data and check #missing values before and after imputation to confirm
print ('\nOrignal #missing: %d'% len(miss_bool))

data.loc[miss_bool, 'Outlet_Size'] = 'Small'

# all mjissing values now just in Supermarket Type 1
data['Outlet_Size'].isnull()
print(sum((data['Outlet_Size'].isnull()) & (data['Outlet_Type'] == 'Supermarket Type1')))
print(sum((data['Outlet_Size'].isnull()) & (data['Outlet_Type'] == 'Supermarket Type2')))
print(sum((data['Outlet_Size'].isnull()) & (data['Outlet_Type'] == 'Supermarket Type3')))


miss_bool1 = (data['Outlet_Size'].isnull()) & (data['Outlet_Type'] == 'Supermarket Type1')
data.loc[miss_bool1, 'Outlet_Size'] = 'Small'
print(sum(data['Outlet_Size'].isnull()))



"""
Feature Engineering
We will combine tidy up some variables and perform one hoe encoding
"""
# mean by Outlet_Type
# seems to be variation between outlets so do not aggregate supermarkets
data.groupby('Outlet_Type')['Item_Outlet_Sales'].mean()
data.groupby('Outlet_Type').Item_Outlet_Sales.mean()


# Fix Item_Visibility since 0 makes little sense
Vis_avg = data.groupby('Item_Identifier').Item_Visibility.mean()

# create boolean index where visibilitry is zero and replace with mean
miss_bool2 =  (data['Item_Visibility'] == 0)
print ('Number of 0 values initially: %d'%sum(miss_bool))
data.loc[miss_bool2, 'Item_Visibility'] = data.loc[miss_bool2, 'Item_Identifier'].apply(lambda x: Vis_avg[x])
print ('Number of 0 values after modification: %d'%sum(data['Item_Visibility'] == 0))

#==============================================================================
# we think products with taht are more visible will sell more
# and we can compare this across stores to see if it matters
# make new variable as ratio of item visibility to mean
#==============================================================================
data['Item_Visibility_Ratio'] = data.apply(lambda x: x['Item_Visibility']/Vis_avg[x['Item_Identifier']], axis = 1)

print(data['Item_Visibility_Ratio'].describe())


#==============================================================================
# Create a new cateogry of type of item, we will make this broad.
# looks like items can be divided into food, drinks and non consumeables
#==============================================================================
# extract first two charactwers of iTEM iDENTIFIER, 2 methods

data['Item_Type_Combined'] = data['Item_Identifier'].apply(lambda x: x[0:2])
data['Item_Type_Combined'] = data['Item_Type_Combined'].map({'FD': 'Food',
                                                                'DR': 'Drinks',
                                                                'NC': 'Non_Consumeables'})
data['Item_Type_Combined']= data['Item_Type_Combined'].str.replace('DR', 'Drinks')
data['Item_Type_Combined'] = data['Item_Type_Combined'].str.replace('FD', 'Food')
data['Item_Type_Combined'] = data['Item_Type_Combined'].str.replace('NC', 'Non_Consumeables')
                                                                
data['Item_Type_Combined'].value_counts()


#==============================================================================
# Next we can calculate how long the store has been in operation
#==============================================================================

data['Outlet_Years'] = 2013 - data['Outlet_Establishment_Year']
data['Outlet_Years'].describe()



#==============================================================================
# Next we modify Item fat content since there are duplicates spelt differently
#==============================================================================
print('Original Categories:')
print(data['Item_Fat_Content'].value_counts())

print('Modified Categories: ')
data['Item_Fat_Content'] = data['Item_Fat_Content'].str.replace('low fat', 'Low Fat')
data['Item_Fat_Content'] = data['Item_Fat_Content'].str.replace('LF', 'Low Fat')
data['Item_Fat_Content'] = data['Item_Fat_Content'].str.replace('reg', 'Regular')
print(data['Item_Fat_Content'].value_counts())

# make non consumables separate category
data.loc[data['Item_Type_Combined'] == 'Non_Consumeables', 'Item_Fat_Content'] = 'Non_edible'
data['Item_Fat_Content'].value_counts()



#==============================================================================
# Now we perform one hot encoding aka make dummy variables for categorical variables
#==============================================================================
from sklearn.preprocessing import LabelEncoder
le = LabelEncoder()

# transforns different ideinifiers intop labels, there are 8 unique identifiers
data['Outlet'] = le.fit_transform(data['Outlet_Identifier'])

# variables we are making dummies for
var_mod = ['Item_Fat_Content','Outlet_Location_Type','Outlet_Size','Item_Type_Combined','Outlet_Type','Outlet']
le = LabelEncoder()
# loop through variables and get labels
for variable in var_mod:
    data[variable] = le.fit_transform(data[variable])
    
    # one hot encoding
    
data = pd.get_dummies(data, columns = ['Item_Fat_Content','Outlet_Location_Type','Outlet_Size','Outlet_Type',
                              'Item_Type_Combined','Outlet'])
           
          
data.dtypes
data[['Item_Fat_Content_0','Item_Fat_Content_1','Item_Fat_Content_2']].head(10)