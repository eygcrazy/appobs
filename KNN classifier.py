#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 12 17:18:32 2019

@author: appobs
"""
## warnings
import warnings
warnings.filterwarnings('ignore')

## 3rd libs for analysis
import pandas as p
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import confusion_matrix, classification_report, precision_score, recall_score, roc_curve, auc

## 3rd for KNN
from sklearn.neighbors import KNeighborsClassifier

## 3rd for DV
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab

## import files
data = p.read_csv('/Users/appobs/Desktop/hw/441/week3/data_wk6/final.csv',
                delimiter= ',',
                na_values= 'nan')

data

for col in data.columns:
    print (col)
    
list(data.columns)

## count of instances and features
rwos, columns = data.shape
print (data.shape)

## Instantiate class, Transforms features by scaling each feature to a given range.
mm = MinMaxScaler()
mm

## scale annual income for instance
data['annual_inc'] = mm.fit_transform(data['annual_inc'].values.reshape(-1, 1))

## Seerate features from target feature, reshape [-1.1] meaning default -1 to 1 column
## since you wanna predict y, so y should be just one dimensional data, and as depdendent var
## x shoud be an array of number, as indeoendent var
x = data['annual_inc'].values.reshape(-1,1)
x
y = data['loan_status'].as_matrix()
y

## Split dataset for validation
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.2, random_state = 1)

## double check the shape
x_train.shape, x_test.shape, y_train.shape, y_test.shape

## create the estimator
knn = KNeighborsClassifier(n_neighbors=3)

## Fit the model
knn.fit(x_train, y_train)

## Check the accuracy score
print (knn.score(x_train, y_train))

## 10 fold cross-validation
cv = cross_val_score(knn, x, y, cv=10)
cv

## Show cross-validation mean & std
print(cv.mean(), cv.std())


## Check graph of ("mean decrease impurity")
knn.kneighbors_graph(x[0:5]).toarray

## Predict y given test set
pret = knn.predict(x_test)

## Take a look at the confusion matrix ([TN,FN],[FP,TP])
confusion_matrix(y_test, pret)

## Accuracy score
print (precision_score(y_test, pret))

## Recall score
print (recall_score(y_test, pret))


## Print classification report
## pretty good result on 0('loan_status = paid'), 
## but bad result on 1('loan_status = unpaid')
print (classification_report(y_test, pret))


## ROC curve
fp, tp, th = roc_curve(y_test, pret)
roc_auc = auc(fp, tp)

## Plot ROC
plt.title('ROC Curve')
# 根据FP, TP 设置auc curve
plt.plot(fp, tp, 'b', 
         label = 'AUC = %0.2f' % roc_auc
         )
## 设置起始点
plt.legend(loc = 'lower right')
plt.plot([0, 1], [0, 1], 'r--')
plt.xlim([0, 1])
plt.ylim([0, 1])
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')












