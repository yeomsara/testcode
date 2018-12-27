import requests
from bs4 import BeautifulSoup as bs
import pymysql
import pandas as pd
 
import configparser as cfp

global DBNAME,HOST,ID,PW
cfg      = cfp.ConfigParser()
conf_dir = 'C:\\Users\\Sarah Yeom\\config.ini'
cfg.read(conf_dir)

HOST     = str(cfg['lib']['host'])
ID       = str(cfg['lib']['user'])
PW       = str(cfg['lib']['password'])
DBNAME   = str(cfg['lib']['dbname'])
    
# MySQL Connection 연결
def select_df(query):
    conn = pymysql.connect(host=HOST, user=ID, password=PW,
                       db=DBNAME, charset='utf8')
 
    # Connection 으로부터 Cursor 생성
    curs = conn.cursor()
    # SQL문 실행
    df = pd.read_sql(query,conn)
    conn.close()
    curs.close()
    return df

df  = select_df('''
            SELECT *
            FROM news_keyword
            ''')

keyword1 = df['keyword'].values
title = []
url = []
keyword = []

for i in keyword1:
    req = requests.get('https://search.naver.com/search.naver?query=%s&where=news&ie=utf8&sm=nws_hty'%i)
    html = req.text
    soup = bs(html,'html.parser')


    my_titles = soup.select(
    'li > dl > dt > a '
    )

    for j in my_titles:
        keyword.append(i)
        url.append(j.get('href'))
        title.append(j.get('title'))

# soup.find('a',{'class':'_sp_each_url _sp_each_title'})['title']
