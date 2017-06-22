import pandas as pd
import numpy as np
from sklearn.metrics import accuracy_score  # an accuracy scoring function from scikit learn
from sklearn.model_selection import GridSearchCV
import matplotlib.pylab as plt
import collections
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import RidgeClassifier


def get_stats(x, y):
    # print the number of datapoints in the dataset
    print 'the number of input face datapoints = ' + str(np.shape(x)[0])
    print 'the number of  input features in the dataset = ' + str(np.shape(x)[1] - 1)
    print 'label counter: ' + str(collections.Counter(y))


def fit_data(x, y):
    # create the linear logistic regression classifier and plug it into the previous Python function
    classifier = LogisticRegression(class_weight='balanced')

    # fit our chosen classifier to the dataset
    classifier.fit(x, y)

    # print out the number of misclassified points
    predicted_labels = classifier.predict(x)
    acc = len(y) - accuracy_score(y.ravel(), predicted_labels.ravel(), normalize=False)
    print 'Our classifier mislabeled ' + str(acc) + ' of ' + str(len(y)) + ' points'


def cross_fit(x, y):
    cross_classifier = LogisticRegression()
    # range of parameters to test with cross-validation
    parameters = {'max_iter': [1000000], 'C': [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0],
                  'solver': ['newton-cg', 'lbfgs', 'liblinear', 'sag'],
                  'class_weight': ['balanced']}

    # create an instance of the GridSearchCV cross-validator - using our classifier and choice or parameters
    cross_validator = GridSearchCV(cross_classifier, parameters)

    # get the best parameter choice based on cross-validation and print out
    cross_validator.fit(x, y)
    best_param = cross_validator.best_estimator_
    print 'best parameter according to cross-validation param is = ' + str(best_param)

    classifier = LogisticRegression(class_weight='balanced', max_iter=100000,
                                    solver=best_param.solver, C=best_param.C)

    # fit our chosen classifier to the dataset
    classifier.fit(x, y)

    # print out the number of misclassified points
    predicted_labels = classifier.predict(x)
    acc = len(y) - accuracy_score(y.ravel(), predicted_labels.ravel(), normalize=False)
    print 'Cross validated classifier mislabeled ' + str(acc) + ' of ' + str(len(y)) + ' points'


raw_face_data = np.asarray(pd.read_csv("datasets/raw_face_data.csv", header=None))
feat_face_data = np.asarray(pd.read_csv("datasets/feat_face_data.csv", header=None))

# get x[i] and y
raw_face_input = raw_face_data[:, :-1]
raw_labels = raw_face_data[:, -1]

feat_face_input = feat_face_data[:, :-1]
feat_labels = feat_face_data[:, -1]

get_stats(raw_face_input, raw_labels)

###classify
fit_data(raw_face_input, raw_labels)

###cross vaidation
cross_fit(raw_face_input, raw_labels)