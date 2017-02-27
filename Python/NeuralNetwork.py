# -*- coding: utf-8 -*-
"""
Created on Wed Jul 27 10:21:04 2016

"""
import numpy as np

class BackPropagationNetwork:
    
    layerCount  = 0
    shape = None
    # store weights as matrices within a list
    weights = []
    
    def __init__(self,layerSize):
        """initlaise the network """
        
        
        self.layerCount = len(layerSize) - 1
        self.shape = layerSize
        
        # Input/Output data from the last run
        self._layerInput = []
        self._layerOuput = []
        
        # Create the Weight arrays
        # We will use slicing with zip function
        # which matches previous layer with current layer
        # remeber bias node aka constant
        # [:-1] all but last element
        # [1:] same as 2:end in matlab
        for (l1,l2) in zip(layerSize[:-1], layerSize[1:]):
            self.weights.append(np.random.normal(scale = 0.01, size = (l2,l1+1)))
            # organise size so matrices conform i.e. l2 (nrows) needs to be size of current layer
            # l1 is number of columns sized of previous layer +1 for bias node
    
    # Run method
    def Run(self, input):
        """ Run the network based on the input data """
        
        lnCases = input.shape[0]
        
        # clear out the previous intermediate value lists        
        self._layerInput = []
        self._layerOutput = []
        
        # Run model
        # multiplies data by weights and uses transfer function to give output
        for index in range(self.layerCount):
            if index  == 0:
                # weights[0] is 1st element of list but is a matrix
                layerInput = self.weights[0].dot(np.vstack([input.T, np.ones([1,lnCases])]))
            else:
                layerInput  = self.weights[index].dot(np.vstack([self._layerOutput[-1], np.ones([1,lnCases])]))
        
            self._layerInput.append(layerInput)
            self._layerOuput.append(self.sgm(layerInput))
        
        # returns in same form as input
        return self._layerOuput[-1].T
            
        
    # Transfer functions
    # sigmoid function
    def sgm(self,x, Derivative  = False):
        if not Derivative:
            return 1/(1+np.exp(-x))
        else:
            out = self.sgm(x)
            return out * (1- out)
        
        
# if run as a script create a test object
if __name__ == '__main__':
    bpn =  BackPropagationNetwork((2,2,1))
    print(bpn.shape) 
    print(bpn.weights)
    print(bpn.layerCount)

    lvInput = np.array([[0,0], [1,1], [-1,0.5]])
    lvOutput = bpn.Run(lvInput)
    
    print("Input: {0}nOuput: {1}".format(lvInput, lvOutput))
