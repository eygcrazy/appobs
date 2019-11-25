#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct  3 14:44:26 2019

@author: appobs
"""
import sys
import csv
import math
import numpy as np
import time
import matplotlib.pyplot as plt

from operator import itemgetter
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.externals import joblib
from sklearn.feature_selection import RFE, VarianceThreshold, SelectFromModel
from sklearn.feature_selection import SelectKBest, mutual_info_regression, mutual_info_classif, chi2
from sklearn import metrics
from sklearn.model_selection import cross_validate, train_test_split
from sklearn.preprocessing import KBinsDiscretizer, scale


#Handle annoying warnings
import warnings, sklearn.exceptions
warnings.filterwarnings("ignore", category=sklearn.exceptions.ConvergenceWarning)

#############################################################################
#
# Global parameters
#
#####################

target_idx=0
cross_val=1
norm_target=0
norm_features=0
binning=0
bin_cnt=2
feat_select=1
fs_type=4
lv_filter=0
feat_start=1

#Set global model parameters
rand_st=1


#############################################################################
#
# Load Data
#
#####################

file1=csv.reader(open('/Users/appobs/Desktop/hw/540/week4/HW2/Pima_Diabetes.csv'), delimiter=',', quotechar='"')

#Read Header Line
header = next(file1)

#Read data
data=[]
target=[]

for row in file1:
    #load target
    if row[target_idx]=='':
        continue
    else:
        target.append(float(row[target_idx]))
        
    #Load row into temp array, cast columns  
    temp=[]
    
    for j in range(feat_start, len(header)):
        if row[j] == '':
            temp.append(float())
        else:
            temp.append(float(row[j]))
            
    #Load temp into Data array
    data.append(temp)
    
#Test print
print(header)
print(len(target),len(data))
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

#Low Variance Filter

if lv_filter==1:
    print('--LOW VARIANCE FILTER ON--', '\n')
    
    #LV Threshold
    sel = VarianceThreshold(threshold=0.5)
    fit_mod = sel.fit(data_np)
    fitted = sel.transform(data_np)
    sel_idx=fit_mod.get_support()
    
    #Get lists of selected and non-selected features (names and indexes)
    temp=[]
    temp_idx=[]
    temp_del=[]
    
    for i in range(len(data_np[0])):
        if sel_idx[i]==1:
            temp.append(header[i+feat_start])
            temp_idx.append(i)
        else:
            temp_del.append(i)

    print('Selected:', temp)
    print('Features (total, selected):', len(data_np[0]), len(temp))
    print('\n')
    
    #Filter selected columns from original dataset
    header=header[0:feat_start]
    for field in temp:
        header.append(field)
        
    data_np=np.delete(data_np, temp_del, axis=1)
    

#Feature Selection
if feat_select==1:
    '''Three steps:
       1) Run Feature Selection
       2) Get lists of selected and non-selected features
       3) Filter columns from original dataset
       '''
    print('--FEATURE SELECTION ON--', '\n')
    ##1) Run Feature Selection #######
    #Wrapper Select via model
    if fs_type==2:
        clf = RandomForestClassifier(n_estimators=100, max_depth=None, min_samples_split=3, criterion='entropy', random_state=rand_st)
        sel = SelectFromModel(clf, prefit=False, threshold='mean', max_features=None)
        print('Wrapper Select: ')
        
        fit_mod=sel.fit(data_np, target_np)
        sel_idx=fit_mod.get_support()
        
    if fs_type==4:
        clf = RandomForestClassifier(n_estimators=100, max_depth=None, min_samples_split=3, criterion='entropy', random_state=rand_st)
        fit_mod=clf.fit(data_np, target_np)   
        importances = fit_mod.feature_importances_
        std = np.std([tree.feature_importances_ for tree in fit_mod.estimators_], axis=0)
        indices = np.argsort(importances)[::-1]
        sel_idx = []
        
        
        print('Wrapper Select: ')

        for x in importances:
            if x > np.mean(importances):
                sel_idx.append(1)
            else:
                sel_idx.append(0)



                
##2) Get lists of selected and non-selected features (names and indexes) #######

    temp=[]
    temp_idx=[]
    temp_del=[]
    for i in range(len(data_np[0])):
        if sel_idx[i]==1:                                                           #Selected Features get added to temp header
            temp.append(header[i+feat_start])
            temp_idx.append(i)
        else:                                                                       #Indexes of non-selected features get added to delete array
            temp_del.append(i)
    print('Selected:', temp)
    print('Features (total/selected):', len(data_np[0]), len(temp))
    print('\n')
             
    
    
    ##3) Filter selected columns from original dataset #########
    header=header[0:feat_start]
    for field in temp:
        header.append(field)
        
    data_np = np.delete(data_np, temp_del, axis=1)
    
#############################################################################
#
# Train SciKit Models
#
##########################################
    
print('--ML Model Output--', '\n')

#Test/Train split
data_train, data_test, target_train, target_test = train_test_split(data_np, target_np, test_size=0.35)

####Classifiers####
if cross_val==0:    
    #SciKit Random Forest
    clf = RandomForestClassifier(n_estimators=100, max_depth=None, min_samples_split=3, criterion='entropy', random_state=rand_st)
    fit_mod=clf.fit(data_train, target_train)
    

    scores_ACC = clf.score(data_test, target_test)                                                                                                                          
    print('Random Forest Acc:', scores_ACC)
    scores_AUC = metrics.roc_auc_score(target_test, clf.predict_proba(data_test)[:,1])                                                                                      
    print('Random Forest AUC:', scores_AUC)                                                                     #AUC only works with binary classes, not multiclass            
 
 
###Cross-Val Classifiers####    
if cross_val==1:
    #Setup Crossval classifier scorers
    scorers = {'Accuracy': 'accuracy', 'roc_auc': 'roc_auc'}                                                                                                                
    
    #SciKit Random Forest - Cross Val
    start_ts=time.time()
    clf = RandomForestClassifier(n_estimators=100, max_depth=None, min_samples_split=3, criterion='entropy', random_state=rand_st)
    scores = cross_validate(clf, data_np, target_np, scoring=scorers, cv=5)
    
    scores_Acc = scores['test_Accuracy']                                                                                                                                    
    print("Random Forest Acc: %0.2f (+/- %0.2f)" % (scores_Acc.mean(), scores_Acc.std() * 2))                                                                                                    
    scores_AUC= scores['test_roc_auc']                                                                     #Only works with binary classes, not multiclass                  
    print("Random Forest AUC: %0.2f (+/- %0.2f)" % (scores_AUC.mean(), scores_AUC.std() * 2))                           
    print("CV Runtime:", time.time()-start_ts)






















