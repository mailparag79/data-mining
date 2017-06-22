from sklearn.metrics import accuracy_score  # an accuracy scoring function from scikit learn
from sklearn.model_selection import GridSearchCV
import matplotlib.pylab as plt
import collections
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import RidgeClassifier


def coded_frame(data, cols):
    columns = list(medical_data.columns.values)
    coded_df = pd.DataFrame(columns=columns)

    data_dict = dict()
    for c in cols:
        codeData = []
        coldata = data[c]
        unique_vals = list(set(data[c]))
        for v in coldata:
            codeData.append(unique_vals.index(v))
        data_dict[c] = codeData

    return data_dict


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


f = coded_frame(medical_data, set(['age', 'readmitted']))

df_gender = pd.get_dummies(medical_data['gender'])
df_new = pd.concat([medical_data, df_gender], axis=1)

del df_new['gender']
del df_new['age']
del df_new['readmitted']

df_coded = pd.DataFrame.from_dict(f)
transformed_medical_data = pd.concat([df_new, df_coded], axis=1)

# get x[i] and y
raw_input = np.asarray(transformed_medical_data[['Male', 'Female', 'age', 'number_emergency']])
raw_labels = np.asarray(transformed_medical_data['readmitted'])

print raw_labels

cross_fit(raw_input, raw_labels)