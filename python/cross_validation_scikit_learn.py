
# coding: utf-8

# # Cross-validation
#
### in this cell we import necessary libraries for the demo
import numpy as np                  # a fundamental numerical linear algebra library
import matplotlib.pyplot as plt     # a basic plotting library
import pandas as pd                 # a basic data handling library
import sys
from sklearn.metrics import accuracy_score   # an accuracy scoring function from scikit learn
sys.path.append('utils')            # append our utility names


# # 1.  Cross-validation for regression
# 
# Load in a nonlinear regression dataset

# load in the data and true function
data = np.asarray(pd.read_csv('datasets/regr_nonlinear_data.csv'))
input_data_2 = data[:,0]
output_data_2 = data[:,1]

func = np.asarray(pd.read_csv('datasets/regr_nonlinear_func.csv'))
true_x_2 = func[:,0]
true_y_2 = func[:,1]

# plot the data and true underlying data-generating function
import regression_plotting_utils as utils  # a set of simple plotting utilities for this notebook
utils.cust_plt_util(input_data_2,output_data_2,true_x_2,true_y_2)


# [Scikit-learn provides a variety of of cross-validation functionality](http://scikit-learn.org/stable/modules/cross_validation.html), in this notebook we will be using [GridSearchCV](http://scikit-learn.org/stable/modules/generated/sklearn.model_selection.GridSearchCV.html), which provides generic cross-validation functionality and is very light on syntax.
# 
# an easy to use cross-validation scikit-learn function
from sklearn.model_selection import GridSearchCV


# ### Kernel-based regressors
# 

# load in KernelRidge - a kernel regressor function from the scikit-learn library
from sklearn.kernel_ridge import KernelRidge

# create a default instance of the regressor
regressor = KernelRidge()

# create a parameter range to test over
parameters = {'kernel':['poly', 'linear'], 'degree':np.arange(1,50)}

# create an instance of the GridSearchCV cross-validator - using our regressor and choice or parameters
cross_validator = GridSearchCV(regressor, parameters)

# run the cross-validation algorithm
cross_validator.fit(input_data_2[:, np.newaxis],output_data_2)      

# get the best parameter choice based on cross-validation and print out
best_param =  cross_validator.best_estimator_.degree     
print 'best parameter according to cross-validation is = ' + str(best_param)


# With the best choice of parameter found via cross-validation, lets look at the resulting model.
# 
# In the next cell we plot the result of a kernel regressor fit to the data - plugging in the best_param found via cross-validation above.

# create an instance of a kernel-based regressor from scikit learn
# load in KernelRidge - a kernel regressor function from the scikit-learn library

regressor = KernelRidge(kernel = 'poly',degree = best_param)

# fit our chosen regressor to the dataset
regressor.fit(input_data_2[:, np.newaxis], output_data_2)                              

# plot pts (in black), true function (in dashed red), and approximation (in blue)
utils.plot_approx(regressor,input_data_2,output_data_2,true_x_2,true_y_2)


parameters = {}

# create an instance of the GridSearchCV cross-validator - using our regressor and choice or parameters
cross_validator = GridSearchCV(regressor, parameters)

# run the cross-validation algorithm
cross_validator.fit(input_data_2[:, np.newaxis],output_data_2)      

# get the best parameter choice based on cross-validation and print out
best_param =  cross_validator.best_estimator_.degree     
print 'best parameter according to cross-validation is = ' + str(best_param)


# With the best choice of parameter found via cross-validation, lets look at the resulting model.
# 
# In the next cell we plot the result of a tree-based regressor fit to the data - plugging in the best_param found via cross-validation above.

# fit our chosen regressor to the dataset
regressor.fit(input_data_2[:, np.newaxis], output_data_2)                              

# plot pts (in black), true function (in dashed red), and approximation (in blue)
utils.plot_approx(regressor,input_data_2,output_data_2,true_x_2,true_y_2)



## load in your tree-based regressor, and create a default instance of it
from sklearn import tree
regressor = tree.DecisionTreeRegressor()

parameters = {'max_depth':np.arange(1, 50)}

# create an instance of the GridSearchCV cross-validator - using our regressor and choice or parameters
cross_validator = GridSearchCV(regressor, parameters)

# run the cross-validation algorithm
cross_validator.fit(input_data_2[:, np.newaxis],output_data_2)

# get the best parameter choice based on cross-validation and print out
best_param =  cross_validator.best_estimator_
print 'best parameter according to cross-validation is = ' + str(best_param)


# ### Neural network based regressors
# 
# Last but not least - neural networks.
# 

# TODO: load in your net-based regressor, and create a default instance of it
from sklearn.neural_network import MLPRegressor

s = [(a,b) for a in np.arange(1, 20) for b in np.arange(1, 20)]
regressor = MLPRegressor(solver = 'lbfgs')
parameters = {'hidden_layer_sizes':s}

# create an instance of the GridSearchCV cross-validator - using our regressor and choice or parameters
cross_validator = GridSearchCV(regressor, parameters)

# run the cross-validation algorithm
cross_validator.fit(input_data_2[:, np.newaxis],output_data_2)      

# get the best parameter choice based on cross-validation and print out
best_param =  cross_validator.best_estimator_.hidden_layer_sizes     
print 'best parameter according to cross-validation is = ' + str(best_param)


# With the best choice of parameter found via cross-validation, lets look at the resulting model.
# 
# In the next cell we plot the result of a neural-net-based regressor fit to the data - plugging in the best_param found via cross-validation above.

# In[23]:

from sklearn.neural_network import MLPRegressor
reg = MLPRegressor(solver = 'lbfgs',alpha = 0,activation = 'tanh',random_state = 1,hidden_layer_sizes = best_param)


# fit our chosen regressor to the dataset
regressor.fit(input_data_2[:, np.newaxis], output_data_2)                              

# plot pts (in black), true function (in dashed red), and approximation (in blue)
utils.plot_approx(regressor,input_data_2,output_data_2,true_x_2,true_y_2)


# an easy to use cross-validation scikit-learn function
from sklearn.model_selection import GridSearchCV


# Lets load up a classification dataset

# load in the data and labels
data = np.asarray(pd.read_csv('datasets/classif_nonlinear_2class_data.csv'))
input_data_2 = data[:,:-1]
labels_2 = data[:,-1]

# load in the true polynomial separator
true_func_2 = np.asarray(pd.read_csv('datasets/classif_nonlinear_2class_func.csv'))

# plot the data and true underlying data-generating function
import classification_plotting_utils as utils  # a set of simple plotting utilities for this notebook
utils.classify_cust_plt_util(input_data_2,labels_2,true_func_2)


# Remember the problem we have: regardless of the nonlinear classification algorithm-type we choose - whether kernel, tree, or neural net - we have parameters that need to be tuned.  Automatically.  This is where cross-validation comes in.

# ### Kernel-based classifiers
# 
# load in SVC - a kernel classifier function from the scikit-learn library
from sklearn.svm import SVC

# create a default instance of the classifier
classifier = SVC()

# create a parameter range to test over
parameters = {'kernel':['rbf'], 'gamma':np.linspace(0,10,100)}

# create an instance of the GridSearchCV cross-validator - using our classifier and choice or parameters
cross_validator = GridSearchCV(classifier, parameters)

# get the best parameter choice based on cross-validation and print out
cross_validator.fit(input_data_2,labels_2)        
best_param =  cross_validator.best_estimator_.gamma     
print 'best parameter according to cross-validation is = ' + str(best_param)


# With the best choice of parameter found via cross-validation, lets look at the resulting model.
# 

# create an instance of a kernel-based regressor from scikit learn
classifier = SVC(kernel = 'rbf',gamma = best_param)

# fit our chosen classifier to the dataset
classifier.fit(input_data_2, labels_2)                              

# plot pts (in red and blue), true separator (in dashed black), and approximation (in solid-black)
utils.plot_approx(classifier,input_data_2,labels_2,true_func_2)

# print out the number of misclassified points
predicted_labels = classifier.predict(input_data_2)
acc = len(labels_2) - accuracy_score(labels_2.ravel(), predicted_labels.ravel(), normalize=False)
print 'Our classifier mislabeled ' + str(acc) + ' of ' + str(len(labels_2)) + ' points'


parameters = {}

# create an instance of the GridSearchCV cross-validator - using our classifier and choice or parameters
cross_validator = GridSearchCV(classifier, parameters)

# get the best parameter choice based on cross-validation and print out
cross_validator.fit(input_data_2,labels_2)        
best_param =  cross_validator.best_estimator_.gamma     
print 'best parameter according to cross-validation is = ' + str(best_param)


# TODO: create an instance of a tree-based classifier from scikit learn


# fit our chosen classifier to the dataset
classifier.fit(input_data_2, labels_2)                              

# plot pts (in red and blue), true separator (in dashed black), and approximation (in solid-black)
utils.plot_approx(classifier,input_data_2,labels_2,true_func_2)

# print out the number of misclassified points
predicted_labels = classifier.predict(input_data_2)
acc = len(labels_2) - accuracy_score(labels_2.ravel(), predicted_labels.ravel(), normalize=False)
print 'Our classifier mislabeled ' + str(acc) + ' of ' + str(len(labels_2)) + ' points'


parameters = {}

# create an instance of the GridSearchCV cross-validator - using our classifier and choice or parameters
cross_validator = GridSearchCV(classifier, parameters)

# get the best parameter choice based on cross-validation and print out
cross_validator.fit(input_data_2,labels_2)        
best_param =  cross_validator.best_estimator_.gamma     
print 'best parameter according to cross-validation is = ' + str(best_param)


# With the best choice of parameter found via cross-validation, lets look at the resulting model.
# 
# In the next cell we plot the result of a neural-net classifier fit to the data - plugging in the best_param found via cross-validation above.

# In[17]:

# TODO: create an instance of a neural-net-based classifier from scikit learn


# fit our chosen classifier to the dataset
classifier.fit(input_data_2, labels_2)                              

# plot pts (in red and blue), true separator (in dashed black), and approximation (in solid-black)
utils.plot_approx(classifier,input_data_2,labels_2,true_func_2)

# print out the number of misclassified points
predicted_labels = classifier.predict(input_data_2)
acc = len(labels_2) - accuracy_score(labels_2.ravel(), predicted_labels.ravel(), normalize=False)
print 'Our classifier mislabeled ' + str(acc) + ' of ' + str(len(labels_2)) + ' points'


