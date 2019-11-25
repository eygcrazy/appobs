#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 11 15:02:03 2019

@author: appobs
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.metrics import confusion_matrix, classification_report, roc_auc_score, roc_curve
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor, export_graphviz
from sklearn import neighbors, tree, naive_bayes, model_selection,preprocessing
from pandas.plotting import scatter_matrix
from sklearn.preprocessing import StandardScaler, MinMaxScaler, KBinsDiscretizer, scale, OneHotEncoder
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.externals import joblib
from sklearn.feature_selection import RFE, VarianceThreshold, SelectFromModel,SelectKBest, mutual_info_regression, mutual_info_classif, chi2
from sklearn import metrics
from sklearn.model_selection import cross_validate, train_test_split, GridSearchCV
from sklearn.decomposition import PCA
from IPython.display import Image

#Handle annoying warnings
import warnings, sklearn.exceptions
warnings.filterwarnings("ignore", category=sklearn.exceptions.ConvergenceWarning)


# Data exploration
arbnb=pd.read_csv('/Users/appobs/Desktop/hw/project/new-york-city-airbnb-open-data/AB_NYC_2019.csv', delimiter = ',', sep='\t')
arbnb.head(10)
arbnb.shape
arbnb.columns.values

arbnb.describe(include="all")
corr = arbnb.corr()
corr.style.background_gradient(cmap='coolwarm')
plt.matshow(arbnb.corr())
plt.show()

ax = sns.heatmap(
    corr, 
    vmin=-1, vmax=1, center=0,
    cmap=sns.diverging_palette(20, 220, n=200),
    square=True
)
ax.set_xticklabels(
    ax.get_xticklabels(),
    rotation=45,
    horizontalalignment='right'
);
        
bool_series = pd.isnull(arbnb[['id', 'name', 'host_id', 'host_name', 'neighbourhood_group',
       'neighbourhood', 'latitude', 'longitude', 'room_type', 'price',
       'minimum_nights', 'number_of_reviews', 'last_review',
       'reviews_per_month', 'calculated_host_listings_count',
       'availability_365']])

arbnb['last_review'].fillna(arbnb['last_review'].mode()[0], inplace=True)
arbnb['reviews_per_month'].fillna(arbnb['reviews_per_month'].mode()[0], inplace=True)
arbnb.describe(include="all")


# Data engineer & cleaning
#Drop unhelpful features
arbnb_after_clean = arbnb[['id', 'host_id', 'neighbourhood_group',
       'latitude', 'longitude', 'room_type', 'price',
       'minimum_nights', 'number_of_reviews', 'last_review',
       'reviews_per_month', 'calculated_host_listings_count',
       'availability_365']]

arbnb_after_clean.head(10)
arbnb_after_clean.columns.values
arbnb_after_clean.shape

# Create dummy variable for PCA
arbnb_after_clean_dummy = pd.get_dummies(arbnb_after_clean[['neighbourhood_group', 'room_type']])
arbnb_after_clean_dummy.head(10)

arbnb_after_clean_withDummy = pd.concat([arbnb_after_clean, arbnb_after_clean_dummy], axis=1)
arbnb_after_clean_withDummy.columns.values

arbnb_after_clean_withDummy = arbnb_after_clean_withDummy[['id', 'host_id', 'latitude', 'longitude',
       'price', 'minimum_nights', 'number_of_reviews',
       'reviews_per_month', 'calculated_host_listings_count', 'availability_365',
       'neighbourhood_group_Bronx', 'neighbourhood_group_Brooklyn',
       'neighbourhood_group_Manhattan', 'neighbourhood_group_Queens',
       'neighbourhood_group_Staten Island', 'room_type_Entire home/apt',
       'room_type_Private room', 'room_type_Shared room']]

arbnb_after_clean_withDummy.shape

arbnb_after_clean_withDummy.columns.values

#### Scale the data to be normalized
min_max_scaler = preprocessing.MinMaxScaler()
min_max_scaler.fit_transform(arbnb_after_clean_withDummy)

## Lets's try to extract components via PCA 
pca = PCA(n_components=4)
principalComponents = pca.fit_transform(min_max_scaler.fit_transform(arbnb_after_clean_withDummy))


## Percentage of variance explained by each of the selected components.
print(['%0.2f' % z for z in pca.explained_variance_ratio_]) 

plt.plot((pca.explained_variance_ratio_))
plt.xlabel('number of components')
plt.ylabel('cumulative explained variance')
plt.show()



#### Ready to use supervised learning to make prediction

# Create target 
x = arbnb_after_clean_withDummy.drop('room_type_Private room',1).as_matrix()
y = arbnb_after_clean_withDummy['room_type_Private room'].as_matrix()

#take a look at x
x

#take a look at y
y

#Test/Train split
x_train, x_test, y_train, y_test = train_test_split(x,y,test_size=.3,random_state=1)

## Take a look at the shape
x_train.shape, y_train.shape

#Normalize again 
min_max_scaler.fit(x_train)
x_train_norm = min_max_scaler.transform(x_train)
x_test_norm = min_max_scaler.transform(x_test)


#### Naive Bayes (Gaussian)
nbclf = naive_bayes.GaussianNB()
nbclf = nbclf.fit(x_train_norm, y_train)
cv_scores = model_selection.cross_val_score(nbclf, x_train_norm, y_train, cv=5)
cv_scores


print ("Score on Training: ", nbclf.score(x_train_norm, y_train))

print ("Score on Test: ", nbclf.score(x_test, y_test))

print("Overall Accuracy on X-Val: %0.2f (+/- %0.2f)" % (cv_scores.mean(), cv_scores.std() * 2))

print ("Accuracy on Training: ",  nbclf.score(x_train_norm, y_train))

nbpreds_test = nbclf.predict(x_test)

print(classification_report(y_test, nbpreds_test))
scores_nbAUC = metrics.roc_auc_score(y_test, nbclf.predict_proba(x_test)[:,1])                                                                                      
print('Naive Bayes (Gaussian) AUC:', scores_nbAUC)                                                                     #AUC only works with binary classes, not multiclass            


#### Naive Bayes(MultinomialNB)
nbmclf = naive_bayes.MultinomialNB()
nbmclf = nbmclf.fit(x_train_norm, y_train)
nbm_cv_scores = model_selection.cross_val_score(nbmclf, x_train_norm, y_train, cv=5)
nbm_cv_scores

print ("Score on Training: ", nbmclf.score(x_train_norm, y_train))

print ("Score on Test: ", nbmclf.score(x_test, y_test))

print("Overall Accuracy on X-Val: %0.2f (+/- %0.2f)" % (nbm_cv_scores.mean(), nbm_cv_scores.std() * 2))

print ("Accuracy on Training: ",  nbmclf.score(x_train_norm, y_train))

nbmpreds_test = nbmclf.predict(x_test)

print(classification_report(y_test, nbmpreds_test))
scores_nbmAUC = metrics.roc_auc_score(y_test, nbmclf.predict_proba(x_test)[:,1])                                                                                      
print('Naive Bayes (MultinomialNB) AUC:', scores_nbmAUC)                                        #AUC only works with binary classes, not multiclass            


#### Random Forest Classifier
RFclf = RandomForestClassifier(criterion='entropy')
RFclf =RFclf.fit(x_train_norm, y_train)
RF_cv_scores = model_selection.cross_val_score(RFclf, x_train_norm, y_train, cv=5)
RF_cv_scores

print ("Score on Training: ", RFclf.score(x_train_norm, y_train))

print ("Score on Test: ", RFclf.score(x_test, y_test))

print("Overall Accuracy on X-Val: %0.2f (+/- %0.2f)" % (RF_cv_scores.mean(), RF_cv_scores.std() * 2))

print ("Accuracy on Training: ",  RFclf.score(x_train_norm, y_train))

RFpreds_test = RFclf.predict(x_test)

print(classification_report(y_test, RFpreds_test))
scores_rfAUC = metrics.roc_auc_score(y_test, RFclf.predict_proba(x_test)[:,1])                                                                                      
print('Random Forest AUC:', scores_rfAUC )                                        #AUC only works with binary classes, not multiclass            











