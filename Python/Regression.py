# -*- coding: utf-8 -*-
"""
Created on Wed Jul 27 12:57:30 2016

@author: dfoley
"""
# required libraries
from sklearn import linear_model
from pandas import read_csv, DataFrame
import numpy as np
from scipy.stats import t
import matlplotlib.pyplot as plt
#import seaborn
# need to install seaborn



class linear_regression(object):
    """ Fit linear model to the data.
    Parameters
    ----------
    x : numpy array or sparse matrix of shape [n_samples,n_features]
        Independent variable defined in the column of data argument below.
    y : numpy array of shape [n_samples, n_targets]
        Dependent variable defined in the column of the data argument below.
    data: pandas DataFrame or str instance (local path/directory of the data)
        Data frame with columns x and y defined above.
    intercept: boolean, default False
        Toggle intercept of the model.
    
    Examples
    --------
    >>> model = linear_regression('height', 'weight', data = 'women.csv')
    >>> print model
    >>> model = linear_regression('height', 'weight', data = 'women.csv', intercept = True)
    >>> print model
    """
    
    def __init__(self, x, y, data, intercept = False):
        self.intercept = intercept
        self.x =str(x)
        self.y = str(y)
        
        if isinstance(data,str):
            self.data = read_csv(data)
        else:
            if isinstance(data, DataFrame):
                self.data = data
            else:
                raise TypeError('%s should be a pandas.Dataframe instance' % data)
        
        # independent variable , make a row vector   
        # initialise linearRegression model form sklearn
        # may implement anlaytical solution later
        self.indv =  np.array(self.data.ix[:,x]).reshape((len(self.data), 1))
        self.depv = np.array(self.data[:,y]).reshape((len(self.data),1))
        _regr_ = linear_model.LinearRegression(fit_intercept = self.intercept)
        self.fit = _regr_.fit(self.indv, self.depv)
        
    def __str__(self):
        # format how results print
        if self.intercept is True:
            _model_ = 'Model:\n' \
                    '\t(%s) = %6.3f + %6.3f *(%s) + error' % (self.y, self.fit.intercept_, self.fit.coed_, self.x)
            _summary_ = 'Summary:\n\t\t\t\tEstimates\n' \
                        '\t(Intercept)\t %8.3f\n' \
                        '\t%s\t\t %8.3f' % (self.fit.intercept_, self.x, self.fit.coef_)
        else:
            _model_ = 'Model:\n' \
                    '\t(%s) = %6.3f * (%s) = error' % (self.y, self.fit.coef_, self.x)
            _summary_ = 'Summary:\n\t\t\tEstimates\n' \
                        '\t%s\t\t %8.3f' % (self.x, self.fit.coef_)      
        return '%s\n\n%s' % (_model_, _summary_)
        
    def predict(self, x = None, plot = False, conf_level = 0.95, sav_fig = False, filename = 'figure',
                fig_format = '.pdf'):
                    
        """        Parameters
        ----------
        x : numpy array or sparse matrix of shape [n_samples,n_features], default None
            Independent variable, if set to None, the original X (predictor) variable
            of the model will be used.
        plot : boolean, default False
            Toggle plot of the data points along with its predicted values and confidence interval.
        conf_level: float between 0 and 1, default 0.95
            Confidence level of the confidence interval in plot. Enabled if plot is True.
        save_fig: boolean, default False
            Toggle to save plot.
        filename: str, default 'figure'
            Name of the file if save_fig is True.
        fig_format: str, default 'pdf'
            Format of the figure if save_fig is True, choices are: 'png', 'ps', 'pdf', and 'svg'.
        Examples
        --------
        >>> from pandas import DataFrame
        >>> from numpy import random.normal
        >>> df = {'x': random.normal(50, 25, 5), 'y': random.normal(50, 25, 5)}
        >>> model = linear_regression('height', 'weight', data = 'women.csv')
        >>> model.predict()
        Returns
        --------
        _res_df_: pandas DataFrame of shape [n_samples,n_features]
            A DataFrame of columns (features) 'Predicted', 'Lower' (Confidence Limit), 'Upper' (Confidence Limit)
        See Also
        --------
        sklearn.linear_model.LinearRegression.predict
            Predict using the linear model
        """
        
        if x is not None and isinstance(x, np.ndarray) and len(x.shape) is 1: