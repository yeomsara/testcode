import requests
from bs4 import BeautifulSoup as bs

req = requests.get('https://search.naver.com/search.naver?query=수주&where=news&ie=utf8&sm=nws_hty')
html = req.text

soup = bs(html,'html.parser')


my_titles = soup.select(
'li > dl > dt > a '
)

title = []
url = []

for i in my_titles:
    url.append(i.get('href'))
    title.append(i.get('title'))

# soup.find('a',{'class':'_sp_each_url _sp_each_title'})['title']
