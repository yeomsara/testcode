import pandas_redshift as pr
import configparser as cfp
global JCLASS,URL,DB_ID_PW,JAR_DIR,DBNAME,HOST,PORT,RS_ID,RS_PW,SCHEMA
cfg = cfp.ConfigParser()
cfg.read(conf_dir)
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

def rs_data_select(query):
    pr.connect_to_redshift(dbname=DBNAME,
                           host=HOST,
                           port=PORT,
                           user=RS_ID,
                           password=RS_PW)
    df = pr.redshift_to_pandas(query)
    pr.close_up_shop()
    df = df.round(2)
    return df

def rs_data_update_delete(query):
    conn = psycopg2.connect(dbname=DBNAME,
                            user=RS_ID,
                            password=RS_PW,
                            host=HOST,
                            port=PORT)
    curs = conn.cursor()
    curs.execute(query)
    conn.commit()
    curs.close()
    conn.close()
    
def rs_data_insert(query,df):
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
    curs.close()
    conn.close()

### oracle (cx_Oracle)

def Oracle_select(query):
    import cx_Oracle
    import pandas.io.sql as psql
    conn = cx_Oracle.connect(DBID,DBPW,DBSID)
    query= query
    df = psql.read_sql(query,conn)
    conn.commit()
    cur.close()
    conn.close()
    return df

