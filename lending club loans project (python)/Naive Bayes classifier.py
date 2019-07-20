#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 11 16:53:05 2019

@author: appobs
"""

## imprt warnings surpass warnings by mat
import warnings
warnings.filterwarnings('ignore')

## import 3rd library
import pandas as pd
from sklearn.preprocessing import OneHotEncoder
from sklearn.naive_bayes import BernoulliNB

## import dataset
data = pd.read_csv('/Users/appobs/Desktop/hw/441/week3/data_wk6/computer.csv',
                  delimiter=',',
                  na_values = 'nan')

data

## start to quantify the string into number
age = {'<=30':0, '31 to 40':1, '>40':2}
income = {'low':0, 'medium':1, 'high':2}
student = {'no':0, 'yes':1}
credit_rating = {'fair':0, 'excellent':1}
buys_computer = {'no':0, 'yes':1}

## then to map into the original dataframe
data['age'] = data['age'].map(age)
data['income'] = data['income'].map(income)
data['student'] = data['student'].map(student)
data['credit_rating'] = data['credit_rating'].map(credit_rating)
data['buys_computer'] = data['buys_computer'].map(buys_computer)

data

## To convert the certain object observations to matrix for calculating the possibiliy
## 转化为矩阵去计算目标对应其他变量出现的次数，这里需要去量化的是‘yes’， 也即是 ‘1’；
x = data.drop('buys_computer', 1).as_matrix()
x
y = data['buys_computer'].as_matrix()
y

## labelEncording 转str到数字，super powerful! 直接把str转化为ordinal number!
## but if you want to make it as dummy var, use oneHotEncoder 
## 

ohe = OneHotEncoder()

## then to merge them into ourdataset;
x = ohe.fit_transform(x).toarray()

x
y

## apply the naive bayes algo;
nb = BernoulliNB(alpha=1.0, fit_prior=True)

## to get the naive bayes result from x and y
nb.fit(x, y)

## ask 10 num but only have 4, so need to transfer them
x1 = ohe.transform([[0,2,0,1]]).toarray()

## to predict
nb.predict(x1)


























