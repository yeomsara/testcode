rm(list=ls())
gc()
library(RCurl)
library(XML)
library(httr)
library(rvest)
library(jsonlite)
library(plyr)
library(stringi)
library(dplyr)

## Keyword Download
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
data <- dbGetQuery(con, "select * from news_keyword")
dbDisconnect(con)
data$keyword <- repair_encoding(data$keyword)
keyword.t <- data$keyword
## Download Cmplete

## Keyword Search
for (k in 1:length(keyword.t)){
  tryCatch({
    ## keyword 가져오기
    keyword <- data$keyword[k]
    type <- data$type[k]
    
    ## 총 페이지 가져오기
    #for(j in times)
    for(j in 1:3){
      tryCatch({
        url <- paste("http://search.etnews.com/etnews/search.php?category=CATEGORY1&kwd=",keyword,"&pageNum=",j,"&pageSize=3&reSrchFlag=false&sort=1&startDate=&endDate=&sitegubun=&jisikgubun=&preKwd%5B0%5D=",keyword,sep="")
        url <- GET(url, timeout(200), encoding="UTF-8")
        html <- read_html(as.character(url), encoding="UTF-8")
        link <- html %>%  html_nodes(xpath="//ul[@class='list_news']") %>% html_nodes("a") %>% html_attr("href")
        link <- as.data.frame(link)
        link <- unique(link)
        link <- as.data.frame(cbind(keyword,link,type))
        urls <- link
        
        if (j == 1) urls1 <- urls else urls1 <- rbind(urls1, urls)
      }, error=function(e){cat(j," ERROR :",conditionMessage(e), "\n")})
    }
    if (k == 1) urls2 <- urls1 else urls2 <- rbind(urls2, urls1)
  }, error=function(e){cat(k," ERROR :",conditionMessage(e), "\n")})
}

t.urls <- urls2
rm(list= ls()[!(ls() %in% c('t.urls'))])
head(t.urls)
names(t.urls) <- c("keyword","url","type")
t.urls <- unique(t.urls)
## Search Complete

## Eliminate duplicate row
t.urls$url <- as.character(t.urls$url)
urls <- unique(t.urls$url)
for (i in 1:length(urls)){
  url <- urls[i]
  data <- t.urls[t.urls$url == url,]
  
  if (length(table(as.character(data$keyword))) != 1){
    keyword <- paste(data$keyword, collapse=", ")
  } else keyword <- as.character(data$keyword[1])
  
  type <- paste(data$type)
  data <- as.data.frame(cbind(keyword, as.character(data$url[1]), type))
  names(data) <- c("keyword","url","type")
  if(i==1) t.urls2 <- data else t.urls2 <- rbind(t.urls2, data)
}
t.urls2 <- unique(t.urls2)
## t.urls 제외하고 오브젝트 모두 삭제
rm(list= ls()[!(ls() %in% c('t.urls2'))])
## Eliminate Complete

## Check DB
## Eliminate Duplicate News
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
urls <- dbGetQuery(con, "select url, type from news_raw where source='전자신문'")
dbDisconnect(con)

if (length(urls) != 0){
  urls$eli <- "O"
  t.urls <- join(t.urls2, urls, by=c("url","type"),type="left", match="all")
  t.urls <- t.urls[is.na(t.urls$eli),]
  t.urls3 <- as.data.frame(table(t.urls$url))
  ## t1이 db에서 걸러져서 0이 db에 이미 들어가 있는애들 
  ## t2가 db에 있지않은 새로운 url
  t1 <- t.urls3[t.urls3$Freq == 0,]
  t2 <- t.urls3[t.urls3$Freq == 1,]
  ## t3에는 새롭게 들어오는 url중에 이미 디비에 저장되어있는  url이면서 타입이 all인 애들만 url만 필터링
  t3 <- urls[urls$url %in% t1 & urls$type == 'all' ,] 
  ## t4에서 t3로 걸른 url이아닌애들을 또한번 잡아주고  
  t4 <- t.urls[!(t.urls$url %in% t3$url),]
  ## t2는 신규  url임으로 db에 넣는 조건으로또다른 객체로서 만들어
  t5 <- t.urls[t.urls$url %in% t2$url,] 
  ## 두가지의 테이블을붙여주면 중복된 url 제거와함꼐 신규 url입력 완료
  t.urls2 <- rbind(t4,t5)
} 
rm(list= ls()[!(ls() %in% c('t.urls2'))])
t.urls2 <- unique(t.urls2)

## Eliminate Complete
if (length(t.urls2[,1]) > 0){
  
  ## News Download
  ## 전자신문 개별페이지 스크래핑([@])
  testObject <- function(object)
  {
    exists(as.character(substitute(object)))
  }
  if(length(t.urls2[,1]) > 0){
    
    for (j in 1:length(t.urls2[,1])){
      tryCatch({
        url <- as.character(t.urls2[j,2])
        url11 <- GET(url, timeout(200), encoding="UTF-8")
        html <- read_html(as.character(url11), encoding="UTF-8")
        header <- html %>%  html_nodes("h2") %>% html_text()
        header <- paste(header, collapse = "")
        header <- gsub("\n"," ",header)
        header <- gsub("\r"," ",header)
        header <- gsub("\t"," ",header)
        header <- gsub("  "," ",header)
        
        text <- html %>%  html_nodes(xpath="//div[@class='tableWrap']") %>% html_text()
        url
        if (length(text) == 0){
          text <- html %>%  html_nodes(xpath="//section[@class='article_body']") %>% html_text()
        }
        text <- stri_conv(text, from="UTF-8")
        text <- as.data.frame(text)
        names(text) <- c("text")
        text$text <- as.character(text$text)
        text$text <- gsub("\n","",text$text)
        text$text <- gsub("\t"," ",text$text)
        text$text <- gsub("\r"," ",text$text)
        text$text <- gsub("  "," ",text$text)
        text$text <- gsub("\\{\\}","",text$text)
        text$text <- gsub("\\(\\)","",text$text)
        text$text <- gsub("\\( \\= \\. \\|\\| \\[\\]\\)\\.\\;","",text$text)
        
        ## 발행일 정보
        date <- html %>%  html_nodes(xpath="//time[@class='date']")  %>% html_text()
        date <- gsub("\\D","",date)
        date <- paste(substr(date,1,4),"-",substr(date,5,6),"-",substr(date,7,8),sep="")
        
        keyword <- as.character(t.urls2[j,1])
        type <- as.character(t.urls2$type[j])
        data1 <- as.data.frame(cbind(date,url, header,text, keyword, type))
        
        if (testObject(data11) == F) data11 <- data1 else data11 <- rbind(data11, data1)
      }, error=function(e){cat(j," ERROR :",conditionMessage(e), "\n")})
    }
    
    rm(list= ls()[!(ls() %in% c('url1','url2','data11','table','t.urls2'))])
  }
  data <- data11
  data <- data[data$date != "",]
  data <- data[data$header != "",]
  data <- data[data$text != "",]
  data <- data[data$keyword != "",]
  data <- data[data$url != "",]
  
  data$text <- gsub("co","",data$text)
  data$text <- gsub("kr","",data$text)
  data$text <- gsub("etnews","",data$text)
  data$text <- gsub("adsbygoogle","",data$text)
  data$text <- gsub("windowadsbygoogle","",data$text)
  data$text <- gsub("pushadsbygoogle","",data$text)
  data$text <- gsub("push","",data$text)
  data$text <- gsub("window","",data$text)
  
  data$date <- substr(data$date,1,10)
  data$date <- gsub("\\.","-",data$date)
  
  data$id <- ""
  data$source <- "전자신문"
  data$like_count <- 0
  data$regdate <- as.character(Sys.time())
  
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
  
  today <- as.character(Sys.Date())
  up <- data[data$date == today,]

    
  ## DB Upload
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
  dbGetQuery(con,"set names utf8")
  dbWriteTable(con, "news_raw", up, append=T, row.names=F)
  
  dbSendQuery(con,paste0("update news_raw
              set text = REPLACE(text,'Criteo.DisplayAd({  \"zoneid\": 345608,  \"async\": false});', '' )
              where text like concat('%','Criteo.DisplayAd({  \"zoneid\": 345608,  \"async\": false});','%')"))
  dbDisconnect(con)
}