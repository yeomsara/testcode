import pandas as pd
pd.options.display.max_rows = 300
pd.options.display.max_columns = 60
import jaydebeapi
from pandas.io.sql import read_sql
import configparser as cfp
global JCLASS,URL,DB_ID_PW,JAR_DIR,DBNAME,HOST,PORT,RS_ID,RS_PWmer,SCHEMA
import pandas as pd
import pandas_redshift as pr
import numpy as np
import time
import statsmodels.api as sm

## init config file
cfg = cfp.ConfigParser()
cfg.read('config.ini')
JCLASS = cfg['common']['JCLASS']
URL = cfg['common']['URL']
DB_ID = cfg['common']['ID']
DB_PW = cfg['common']['PW']
DB_ID_PW = [DB_ID,DB_PW]
JAR_DIR = cfg['common']['JAR_DIR']
DBNAME = cfg['redshift_lib']['dbname']
HOST = cfg['redshift_lib']['host']
PORT = cfg['redshift_lib']['port']
RS_ID = cfg['redshift_lib']['user']
RS_PW = cfg['redshift_lib']['password']
SCHEMA =  cfg['redshift_lib']['schema']

##데이터로드 
def rs_data_select(query):
    pr.connect_to_redshift(dbname=DBNAME,
                           host=HOST,
                           port=PORT,
                           user=RS_ID,
                           password=RS_PW)

    df = pr.redshift_to_pandas(query)
    pr.close_up_shop()
    df = df.round(2)
    df = df.fillna(0)
    df[int_cols] = df[int_cols].astype(int)
    return df


def save_model(model,model_dir):
    import pickle
    model.save(model_dir)
    print('%s 로 모델이 저장되었습니다.'% model_dir)
    
## 예측모델 로드 ##
def load_sm_model(model_dir):
    import statsmodels.api as sm
    from statsmodels.iolib.smpickle import load_pickle
    new_results = sm.load(model_dir)
    return new_results


model_df = rs_data_select(model_query)
print('학습 데이터 셋 길이 : %s'%len(model_df))

## 데이터 전처리
model_df = data_clean(model_df)

## make model input dataset ###
dataset = make_model_dataset(model_df)


## make_logit_training_set
train_X = dataset[cols].drop(columns=['a'])
train_X = train_X.drop_duplicates(subset=train_X.columns.values.tolist(),keep='first')
train_y = train_X['target_label']
print(len(train_y[train_y ==1]))
print(len(train_y[train_y ==0]))
train_X = train_X.drop(columns=['target_label'])
train_cols = train_X.columns.values.tolist()
logit_model = sm.Logit(train_y.astype(int),train_X.astype(int))
result = logit_model.fit()
## model save code
# model_dir = 'model_name.pickle'
# save_model(result,model_dir)
