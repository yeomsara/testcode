from imblearn.over_sampling import RandomOverSampler,SMOTE
from imblearn.under_sampling import RandomUnderSampler
from imblearn.combine import SMOTEENN,SMOTETomek
import pandas as pd
import numpy as np
from sklearn.datasets import load_iris,load_breast_cancer
from collections import Counter
import collections
import tensorflow as tf
from sklearn import model_selection

def split_train_test_data(df,test_ratio):    
    array = df.values
    X = array[:,0:len(array[0])-1]
    Y = array[:,len(array[0])-1]
    seed = 7
    X_train, X_test, Y_train, Y_test = model_selection.train_test_split(X, Y, test_size=test_ratio, random_state=seed)
    return X_train, X_test, Y_train, Y_test

def load_with_header(df,target_dtype,features_dtype,target_column=-1):
    n_samples = len(df)
    n_features = len(df.columns)
    data = np.zeros((n_samples,n_features), dtype=features_dtype)
    target = np.zeros((n_samples,),dtype=target_dtype)
    target - np.asarray(df[df.columns[-1]],dtype=target_dtype)
    data = np.asarray(df.drop(df.columns[[target_column]],axis=1), dtype=features_dtype)
    return Dataset(data=data, target= target)

### Training set OverSampling ###
# ros = RandomOverSampler
# rus = RandomUnderSampler
# smo = SMOTE - borderline1
# smob1 = SMOTE - borderline1
# smob2 = SMOTE - borderline2
# sme = SMOTEENN
# smt = SMOTETomek 
global sampling
def sampling_factory(X,Y,ratio,cat):
    if cat == 'ros':
        sampling = 'random_over_sampling'
        data = RandomOverSampler(ratio=ratio)
    elif cat == 'rus':
        sampling = 'random_under_sampling'
        data = RandomUnderSampler(ratio=ratio)
    elif cat == 'smo':
        sampling = 'SMOTE_over_sampling'
        data = SMOTE(ratio=ratio)
    elif cat == 'smob1':
        sampling = 'borderline_SMOTE1_over_sampling'
        data = SMOTE(ratio=ratio,kind='borderline1')
    elif cat == 'smob2':
        sampling = 'borderline_SMOTE2_over_sampling'
        data = SMOTE(ratio=ratio,kind='borderline2')
    elif cat == 'sme':
        sampling = 'SMOTEENN_combine_sampling'
        data = SMOTEENN(ratio=ratio,random_state=42)
    else :
        sampling = 'SMOTETomek_combine_sampling'
        data = SMOTETomek(random_state=42)
    X_resampled,y_resampled = data.fit_sample(X,Y)
    X2 = pd.DataFrame(X_resampled)
    # columns rename
    X2.columns = X.columns.values
    return X2


### load_breast_cancer data classes = 2
### class 0 = malignant , count =  212
### class 1 = benign , count = 357
cancer = load_breast_cancer()
df_cancer = pd.DataFrame(data=np.c_[cancer['data'],cancer['target']],columns=np.append(cancer['feature_names'],'target'))
print(cancer['target_names'])
print(Counter(df_cancer['target']))

Dateset = collections.namedtuple('Dataset',['data','target'])
Dateset2 = collections.namedtuple('Dataset',['train','test'])
test_split= 0.20
X_train, X_test, Y_train, Y_test = split_train_test_data(df_cancer,test_split)
