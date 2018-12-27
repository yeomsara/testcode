import pandas as pd
import configparser as cfp
import psycopg2

global JCLASS,URL,DB_ID_PW,JAR_DIR,DBNAME,HOST,PORT,RS_ID,RS_PW,SCHEMA
cfg      = cfp.ConfigParser()
conf_dir = ''
cfg.read(conf_dir)
DBNAME   = cfg['lib']['dbname']
HOST     = cfg['lib']['host']
PORT     = cfg['lib']['port']
RS_ID    = cfg['lib']['user']
RS_PW    = cfg['lib']['password']

def select_df(query):
    try:
        conn = psycopg2.connect(dbname=DBNAME,
                                user=RS_ID,
                                password=RS_PW,
                                host=HOST,
                                port=PORT)
        curs = conn.cursor()
        df = pd.read_sql(query,conn)
        return df
    except Exception as e:
        print(e)
    finally:
        curs.close()
        conn.close()
    

def update_delete(query):
    try:
        conn = psycopg2.connect(dbname=DBNAME,
                                user=RS_ID,
                                password=RS_PW,
                                host=HOST,
                                port=PORT)
        curs = conn.cursor()
        curs.execute(query)
        conn.commit()
    except Exception as e:
        print(e)
    finally:
        curs.close()
        conn.close()
        
    
def insert(query,df):
    try:
        conn = psycopg2.connect(dbname=DBNAME,
                                user=RS_ID,
                                password=RS_PW,
                                host=HOST,
                                port=PORT)
        curs = conn.cursor()
        cols = ['a','b','c']
        df = df.reindex(columns=cols)
        args_bytes = b','.join(curs.mogrify("(%s,%s,%s)",x) for x in df.values)
        curs.execute(query+args_bytes)
        conn.commit()
    except Exception as e:
        print(e)
    finally:
        curs.close()
        conn.close()
