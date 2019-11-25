#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 20 16:36:11 2019

@author: appobs
"""

# 3rd libs
import sys
import csv
import math
import numpy as np
from operator import itemgetter
import time

from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor
from sklearn.externals import joblib
from sklearn.feature_selection import RFE, VarianceThreshold, SelectFromModel
from sklearn.feature_selection import SelectKBest, mutual_info_regression, mutual_info_classif, chi2
from sklearn import metrics
from sklearn.model_selection import cross_validate, train_test_split
from sklearn.preprocessing import KBinsDiscretizer, scale


#Handle annoying warnings
import warnings, sklearn.exceptions
warnings.filterwarnings("ignore", category=sklearn.exceptions.ConvergenceWarning)

# Set up global vars
target_idx=0                                        #Index of Target variable
cross_val=1                                         #Control Switch for CV
norm_target=0                                       #Normalize target switch
norm_features=0                                     #Normalize target switch
binning=0                                           #Control Switch for bin target
binary_cnt=2                                        #if bins target, this sets number of classes
feat_select=0                                       #Control Switch for feature selection
fs_type=2                                           #Feature selecton type(1=StepWise, Backward, Romoval; 2= Wrapper Selectio; 3=Univarate Selecion)
lv_filter=0                                         #Control Switch for low variance filter on features
feat_start=1                                        #Start Column of Features

# Set up global model parameters
rand_st=1                                           #Set random State var on randomlize splits on runs

# Load files

files1 = csv.reader(open('/Users/appobs/Desktop/hw/540/week2/HW1/Pima_Diabetes.csv'), delimiter = ',', quotechar = '"')

# Read Header line
header = next(files1)

# Read data
data = []
target = []
for rows in files1:
    #load target
    if rows[target_idx]=='':                        #if target is blank, skip it
        continue
    else:
        target.append(float(rows[target_idx]))      #if pre-binned class, change float to int

    #load rows into temparory array, cast columns       
    #Basicly this code loop through values from each feature and if the values is nan, then append as 0
    temp=[]

    for j in range(feat_start, len(header)):
        if rows[j] == '':
            temp.append[float]            
        else:
            temp.append(float(row[j]))
            
            
    #load temp into data array
    data.append(temp)

#Test print
print(header)
print(len(target), len(data))
print('\n')

data_np=np.asarray(data)
target_np=np.asarray(target)

#############################################################################
#
# Preprocess data
#
##########################################


#############################################################################
#
# Feature Selection
#
##########################################


#############################################################################
#
# Train SciKit Models
#
##########################################

print('---ML model output---', '\n')

# Spliting dataset as Training & Testing
data_train, data_test, target_train, target_test = train_test_split(data_np, target_np, test_size = 0.35)

#Classifer
if cross_val==0:
    #Apply DT from SKL
    clf = DecisionTreeClassifier(criterion='entropy', splitter='best', max_depth=None, 
                                 min_samples_split=3, min_samples_leaf=1, max_features=None, random_state=rand_st)
    clf.fit(data_train, target_train)
    
    scores_acc = clf.score(data_test, target_test)
    print('Decision Tree ACC:', scores_acc )
    scores_AUC = metrics.roc_auc_score(target_test, clf.predict_proba(data_test)[:,1])
    print('Decision Tree AUC:', scores_AUC)                     #AUC only works with binary classes, not multiclass            


#Cross-Validation classifer
#In this case, the cross_val more like trigger for different options
if cross_val==1:
    # Setup CrossVal Scores
    scores = {'Accuracy': 'accuracy', 'roc_auc': 'roc_auc'}
    
    #SKL decisionTreeClassifer - Cross Val
    start_ts = time.time()
    clf = DecisionTreeClassifier(criterion='gini', splitter='best', max_depth=None, 
                                 min_samples_split=3, min_samples_leaf=1, max_features=None, random_state=rand_st)
    scores = cross_validate(clf, data_np, target_np, scoring=scores, cv=10)
    
    scores_acc = scores['test_Accuracy']
    print('Decision Tree Acc: %0.2f (+/- %0.2f)' % (scores_acc.mean(), scores_acc.std()*2))
    scores_AUC= scores['test_roc_auc']                                                                      #Only works with binary classes, not multiclass
    print('Decision Tree AUC: %0.2f (+/- %0.2f)' % (scores_AUC.mean(), scores_AUC.std()*2))
    
    
    