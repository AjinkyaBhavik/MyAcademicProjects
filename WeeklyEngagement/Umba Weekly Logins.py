#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 31 12:03:20 2020

@author: ajinkyabhavik
"""

#importing appropriate libraries
import pandas as pd
import numpy as np 
import seaborn as sns
import plotly as plot
import matplotlib.pyplot as plt

#importing (collecting) dataset
data = pd.read_csv("/Users/ajinkyabhavik/Downloads/weekly_logins.csv")

#understanding the data first

#to get a general sense of the data values
print (data.describe())

#understand the columns
print (data.columns)


#analyze unique values in each column
df = data.nunique()


data['Industry'].unique()

#Looking at the dataset we realise that influence on Weeky logins 
#can be influenced by Industry type category

Industry = data[['Industry', 'Weekly Logins']]


 sns.set(style="ticks", color_codes=True)
 plt.figure(figsize=(20,5))
    #figsize = (20,10)
    #fig, ax = plt.catplot(figsize=fig_dims)
sns.catplot(x="Industry", y="Weekly Logins", data=Industry, height=5,
    aspect=2.5);
    
 #Here we notice that there are different population of weekly logins for different segments
 #so we check if othr parameters like slack, gmail and zendesk integration have an effect on the weekly logins
 
 Integpara = data[['Slack Integration','Zendesk Integration','Salesforce Integration','Gmail Integration','Weekly Logins']]

#importing encoders
from sklearn.preprocessing import LabelEncoder, OneHotEncoder

#Checking columns in the new dataframe

Integpara.columns

#Encoding Categorial Data
#Encoding the Independent Variable
for column in Integpara.columns:
    Encoder_Integpara = LabelEncoder()
    Integpara[column] = Encoder_Integpara.fit_transform(data[column])


#plotting the correlation matrix
plt.figure(figsize = (10,6))
palette = sns.diverging_palette(20, 220, n=256)
corrMatrix = Integpara.corr()
akws = {"ha": 'center',"va": 'bottom'}
sns.heatmap(corrMatrix, annot=True, annot_kws=akws)
plt.show()

