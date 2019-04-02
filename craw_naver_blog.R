rm(list=ls())
gc()
library(xml2)
library(bitops)
library(RCurl)
library(XML)
library(httr)
library(rvest)
library(jsonlite)
library(plyr)
library(stringi)
library(stringr)
library(dplyr)
library(urltools)
require(RMySQL)


## ============================== 2017-12-07  사라 수정 =====================================

#keyword_set = ["신입채용예정","신축예정", "착공예정"]
#s_keyworkd for in keyword_set
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
data <- dbGetQuery(con, "select * from blog_keyword")
dbDisconnect(con)
data$keyword <- repair_encoding(data$keyword)
keyword.t <- data$keyword
print(keyword.t)
## Download Cmplete
start_date <- Sys.Date()-7
end_date <- Sys.Date()
end_date <- gsub('-','',end_date)

for (k in 1:length(keyword.t)){
  tryCatch({
    ## keyword 가져오기
    keyword <- data$keyword[k]
    type <- as.character(data$type[k])
    keyword1 <- curlEscape(stri_conv(keyword,to="EUC-KR"))
    for(j in 1:5){
      tryCatch({
        ## 페이지 수 확인
        url <- paste0("https://search.naver.com/search.naver?date_to=",end_date,"&dup_remove=1&nso=p%3A1d&post_blogurl=&post_blogurl_without=&query=",keyword,"&sm=tab_pge&srchby=all&st=sim&where=post&start=",j)
        #   url <- paste("https://search.naver.com/search.naver?sm=sta_hty.blog&ie=utf8&query=",keyword, sep="")
        #   url <- enc2utf8(url)
        #   url <- GET(url, timeout(200))
        html <- read_html(as.character(url), encoding="UTF-8")
        #   PARSED <- htmlParse(html)
        
        header <- html %>% html_nodes(xpath="//div[@class='blog section _blogBase']") %>% html_nodes(xpath="//ul[@id='elThumbnailResultArea']") %>% html_nodes(xpath="//a[@class='sh_blog_title _sp_each_url _sp_each_title']") %>% html_text
        link <- html %>% html_nodes(xpath="//div[@class='blog section _blogBase']") %>% html_nodes(xpath="//ul[@id='elThumbnailResultArea']") %>% html_nodes(xpath="//a[@class='sh_blog_title _sp_each_url _sp_each_title']") %>% html_attr('href')
        link <- as.data.frame(unique(link))
        names(link) <- "url"
        link <- as.data.frame(cbind(keyword,link, type))
        urls <- link  
        if (j == 1) urls1 <- urls else urls1 <- rbind(urls1, urls)
      }, error=function(e){cat(j," ERROR :",conditionMessage(e), "\n")})
    }
    if (k == 1) urls2 <- urls1 else urls2 <- rbind(urls2, urls1)
  }, error=function(e){cat(k," ERROR :",conditionMessage(e), "\n")})
}

t.urls <- urls2
t.urls <- unique(t.urls) # 중복제거


rm(list= ls()[!(ls() %in% c('t.urls'))])
# gc()
t.urls$url <- as.character(t.urls$url)
urls <- unique(t.urls$url)

for (i in 1:length(urls)){
  url <- urls[i]
  data <- t.urls[t.urls$url == url,]
  
  if (length(table(as.character(data$keyword))) != 1){
    keyword <- paste(data$keyword, collapse=",")
  } else keyword <- as.character(data$keyword[1])
  
  type <- paste(data$type)
  data <- as.data.frame(cbind(keyword, as.character(data$url[1]), type))
  names(data) <- c("keyword","url","type")
  if(i==1) t.urls2 <- data else t.urls2 <- rbind(t.urls2, data)
}

t.urls2 <- unique(t.urls2)
t.urls2 <- t.urls2[substr(t.urls2$url,1,22) %in% c('http://blog.naver.com/','https://blog.naver.com') , ]


## t.urls 제외하고 오브젝트 모두 삭제
rm(list= ls()[!(ls() %in% c('t.urls2'))])
#gc()

t.urls2$url <- gsub("http://","",t.urls2$url)
t.urls2$url <- gsub("https://","",t.urls2$url)
t.urls2$url <- gsub("blog.naver.com","",t.urls2$url)
t.urls2$url <- gsub("Redirect=Log&logNo=","",t.urls2$url)
t.urls2$url <- gsub("&from=section","",t.urls2$url)


urls <- unlist(strsplit(t.urls2$url,'[?]'))
url_id <- urls[substr(urls,1,1) == '/']
url_id <- gsub('/','',url_id)
url_no <- urls[substr(urls,1,1) != '/']

t.urls2$url <- paste0('http://blog.naver.com/PostView.nhn?blogId=',url_id,'&logNo=',url_no)


require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
db_urls <- dbGetQuery(con, paste0("select url, type from blog_raw where date = '",as.character(Sys.Date()),"'")) 
dbDisconnect(con)

if (length(db_urls) != 0){
  db_urls$eli <- "O"
  t.urls <- join(t.urls2, db_urls, by=c("url", "type"),type="left", match="all")
  t.urls <- t.urls[is.na(t.urls$eli),]
  t.urls3 <- as.data.frame(table(t.urls$url))
  ## t1이 db에서 걸러져서 0이 db에 이미 들어가 있는애들 
  ## t2가 db에 있지않은 새로운 url
  t1 <- t.urls3[t.urls3$Freq == 0,]
  t2 <- t.urls3[t.urls3$Freq == 1,]
  ## t3에는 새롭게 들어오는 url중에 이미 디비에 저장되어있는  url이면서 타입이 all인 애들만 url만 필터링
  t3 <- db_urls[db_urls$url %in% t1 & db_urls$type == 'all' ,] 
  ## t4에서 t3로 걸른 url이아닌애들을 또한번 잡아주고  
  t4 <- t.urls[!(t.urls$url %in% t3$url),]
  ## t2는 신규  url임으로 db에 넣는 조건으로또다른 객체로서 만들어
  t5 <- t.urls[t.urls$url %in% t2$url,] 
  ## 두가지의 테이블을붙여주면 중복된 url 제거와함꼐 신규 url입력 완료
  t.urls2 <- rbind(t4,t5)
  #   b <- as.data.frame(table(t.urls2$url))
}
# &redirect=Dlog&widgetTypeCall=true&directAccess=false
t.urls2 <- t.urls2[, !(colnames(t.urls2) %in% c("eli"))]

## Eliminate Complete
if (length(t.urls2[,2]) > 0){
  testObject <- function(object)
  {
    exists(as.character(substitute(object)))
  }
  ## 네이버블로그 개별페이지 스크래핑(biz)
  for (j in 1:length(t.urls2[,2])){
    tryCatch({
      url <- t.urls2[j,2]
      type <- as.character(t.urls2[j,3])
      html <- read_html(as.character(url), encoding="utf-8")
      PARSED <- htmlParse(html)
      header <- html %>% html_nodes(xpath="//*[@class='pcol1']") %>% html_text
      if(length(header) == 0){
        header <- html %>% html_nodes(xpath="//span[@class='se-fs-fs32 se-ff-nanumgothic']") %>% html_text
      }else{
        header <- header
      }
      
      header <- stri_conv(header, from="UTF-8")[1]
      header <- gsub("\r","",header)
      header <- gsub("\n","",header)
      header <- gsub("\t","",header)
      header <- gsub("  ","",header)
      header <- gsub('\\"',"",header)
      #       header <- gsub("[:punct:]","",header)
      header <- noquote(header)
      text <- html %>%  html_nodes(xpath="//*[@class='se-main-container']") %>% html_text()
      if(length(text) == 0){
        text <- html %>% html_nodes(xpath="//*[@id='printPost1']") %>% html_text
      }else{
        text <- text
      }
      text <- stri_conv(text, from="UTF-8")
      text <- gsub("\r","",text)
      text <- gsub("\n","",text)
      text <- gsub("\t","",text)
      text <- gsub("  ","",text)
      text <- gsub("다 ","다. ",text)
      text <- gsub("요 ","요. ",text)
      text <- gsub("\\ㅋ","",text)
      text <- gsub("\\ㅎ","",text)
      # text <- gsub(pattern="[[:punct:]]","",text)
      date <- as.character(Sys.Date())
      
      keyword <- as.character(t.urls2[j,1])
      
      data1 <- as.data.frame(cbind(date,url, header,text, keyword, type))
      # print(date)
      if (testObject(data11) == F) data11 <- data1 else data11 <- rbind(data11, data1)
    }, error=function(e){cat(j," ERROR :",conditionMessage(e), "\n")})
  }
  
  rm(list= ls()[!(ls() %in% c('data11','t.urls2'))])
  data <- data11
  data <- data[complete.cases(data), ]
  data$id <- ""
  data$source <- "naver"
  data$like_count <- 0
  data$complete <- 0
  data$regdate <- as.character(Sys.time())
  head(data)
  data <- data[nchar(as.character(data$text)) > 10 & str_detect(data$text, "CDATA") == FALSE ,]
  
  ## url당 all 포함하고 있는 갯수
  remvdup <- data %>% group_by(url) %>% dplyr::summarise( allcheck = sum(type == "all"))
  remvdup <- remvdup[remvdup$allcheck >= 1, ]
  remvdup <- dplyr::inner_join(remvdup, data, by="url")
  remvdup <- unique(remvdup[remvdup$type == "all",])
  
  ## keep only observations in data that match in remvdup
  data_1 <- semi_join(data, remvdup, by= c("url", "type"))
  
  ## drops all observations in data  that match in remvdup
  data_2 <- data[!(data$url %in% data_1$url),]
  
  ## rbind
  data <- unique(rbind(data_1, data_2))
  
  ## remove blog which contains over 100 commas 
  data$valid <- sapply(data$text, function(x) stringr::str_count(x, pattern=","))  
  data <- data[data$valid < 100, ]
  data$valid <- NULL
  today <- as.character(Sys.Date())
  data <- data[data$date == today,]
  
  ## DB Upload
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
  dbGetQuery(con,"set names utf8")
  dbWriteTable(con, "blog_raw", data, append=T, row.names=F)
  dbDisconnect(con)
  
  
  
}