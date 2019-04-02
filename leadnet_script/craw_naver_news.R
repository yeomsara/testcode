rm(list=ls())
gc()
library(RCurl)
library(XML)
library(httr)
library(rvest)
library(jsonlite)
library(plyr)
library(stringi)
library(RMySQL)
library(dplyr)

# Keyword Download
# require(RMySQL)
# con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
# dbGetQuery(con,"set names utf8")
# data <- dbGetQuery(con, "select keyword,type from news_keyword where type not in ('business1','business2')")
# data2 <- dbGetQuery(con, "select keyword,type from news_keyword where type in ('all','business1','business2')")
# company <- dbGetQuery(con, "select name,type from business_company")
# dbDisconnect(con)
# 
# paste_key <- c(outer(company[, "name"],data2[, "keyword"], FUN=paste, sep=" "))
# paste_key2 <- c(outer(company[, "name"],data2[, "keyword"],company[,"type"], FUN=paste, sep=" "))
# a <- data.frame(do.call('rbind', strsplit(as.character(paste_key2),' ',fixed=TRUE)))
# b <- as.data.frame(cbind(paste_key,as.character(a$X3)))
# names(b) <- c('keyword','type')
# data <- rbind(data,b)
# keyword.t <- data$keyword

require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
data <- dbGetQuery(con, "select * from news_keyword")
dbDisconnect(con)
data$keyword <- repair_encoding(data$keyword)
keyword.t <- data$keyword

########### 17-09-17 수정완료 ##########
req_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.78 Safari/537.36"

for (k in 1:length(keyword.t)){
  tryCatch({
    keyword <- data$keyword[k]
    type <- data$type[k]
    keyword1 <- curlEscape(stri_conv(keyword,to="EUC-KR"))
    for(j in seq(1, 31, 10)){
      tryCatch({
        url <- paste("http://search.naver.com/search.naver?ie=utf8&where=news&query=",keyword,"&sm=tab_pge&sort=0&start=",j, sep="")
        #url <- paste("http://utils.wish.today/image-proxy/?u=http%3A%2F%2Fsearch.naver.com%2Fsearch.naver%3Fie%3Dutf8%26where%3Dnews%26query%3D",keyword,"&sm=tab_pge&sort=0&start=",j, sep="")
        url <- enc2utf8(url)
        #         url <- GET(url, timeout(200))
        #html <- read_html(as.character(url), encoding="UTF-8")
        res <- httr::GET(as.character(url), httr::add_headers(UserAgent = req_agent), encoding="UTF-8")
        html <- httr::content(res)
        
        link <- html %>%  html_nodes("div") %>% html_nodes(xpath="//dd[@class='txt_inline']")%>% html_nodes(xpath="//*[@class='_sp_each_url']")  %>% html_attr("href")
        link <- as.data.frame(unique(link))
        names(link) <- "url"
        link <- as.data.frame(cbind(keyword,link, type))
        urls <- link   
        urls
        if (j == 1) urls1 <- urls else urls1 <- rbind(urls1, urls)
      }, error=function(e){cat("k=",k,"j=",j," ERROR :",conditionMessage(e), "\n")})
    }    
    if (k == 1) urls2 <- urls1 else urls2 <- rbind(urls2, urls1)
  }, error=function(e){cat("k=",k," ERROR :",conditionMessage(e), "\n")})
}

t.urls <- urls2
t.urls <- unique(t.urls)
t.urls <- t.urls[substr(t.urls$url,1,22) %in% c("http://news.naver.com/","https://news.naver.com"),]

rm(list= ls()[!(ls() %in% c('t.urls'))])

t.urls$url <- as.character(t.urls$url)
urls <- unique(t.urls$url)
for (i in 1:length(urls)){
  url <- urls[i]
  data <- t.urls[t.urls$url == url,]
  
  if (length(table(as.character(data$keyword))) != 1){
    keyword <- paste(data$keyword, collapse=",")
    keyword <- unique(keyword)
  } else keyword <- as.character(data$keyword[1])
  
  type <- paste(data$type)
  
  data <- as.data.frame(cbind(keyword, as.character(data$url[1]), type))
  names(data) <- c("keyword","url","type")
  if(i==1) t.urls2 <- data else t.urls2 <- rbind(t.urls2, data)
}
t.urls2 <- unique(t.urls2)

rm(list= ls()[!(ls() %in% c('t.urls2'))])


require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
urls <- dbGetQuery(con, "select url, type from news_raw") ## 시간정보 줄 것
dbDisconnect(con)

if (length(urls) != 0){
  urls$eli <- "O"
  t.urls <- join(t.urls2, urls, by=c("url", "type"),type="left", match="all")
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
  #   b <- as.data.frame(table(t.urls2$url))
}
rm(list= ls()[!(ls() %in% c('t.urls2'))])


#if (length(t.urls2[,1]) > 0){

testObject <- function(object)
{
  exists(as.character(substitute(object)))
}

## 네이버 뉴스 개별페이지 스크래핑

req_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.78 Safari/537.36"

for (j in 1:length(t.urls2[,1])){
  tryCatch({
    url <- as.character(t.urls2[j,2])
    #url <- paste("http://utils.wish.today/image-proxy/?u=", URLencode(url, reserved = TRUE), sep="")
    html <- read_html(url)
    #res <- httr::GET(url, httr::add_headers(UserAgent = req_agent), encoding="UTF-8")
    #html <- httr::content(res)
    
    header <- html %>%  html_nodes(xpath="//h3[@id='articleTitle']") %>% html_text()
    if (length(header) == 0) header <- html %>%  html_nodes(xpath="//p[@class='end_tit']") %>% html_text() #연예
    
    source <- html %>%  html_nodes(xpath="//meta[@property='og:article:author']") %>% html_attr("content")
    source <- strsplit(source,"|",fixed=T)[[1]][1]
    source <- gsub(" ","",source)
    
    text <- html %>%  html_nodes(xpath ="//div[@id='articleBodyContents']") %>% html_text()
    if (length(text) == 0) text <- html %>%  html_nodes(xpath ="//div[@id='articeBody']") %>% html_text()
    # 기본 전처리
    text <- gsub("\r","",text)
    text <- gsub("\t","",text)
    text <- gsub("\n","",text)
    text <- gsub("\\{\\}","",text)
    text <- gsub("\\(\\)","",text)
    #text <- gsub("\\("," (",text)
    #text <- gsub("다+\\.","다. ",text)
    #text <- gsub("뉴시스 뉴스, 네이버 뉴스 스탠드에서도 만나세요뉴시스 SNS","",text)
    text <- gsub("뉴스1 © News1","",text)
    text <- gsub("무단 전재 및 재배포 금지","",text)
    text <- gsub("무단전재 및 재배포 금지","",text)
    text <- gsub("ⓒ","",text)
    text <- gsub("©","",text)
    text <- gsub("한국경제 & hankyung.com","",text)
    text <- gsub("매일 업데이트 최신 만화 100% 무료","",text)
    text <- gsub("// flash 오류를 우회하기 위한 함수 추가function _flash_removeCallback", "", text) #E-mail 제거
    #text <- gsub("[가-힣a-zA-Z]+[[:space:]]+(기자|기자 =)"," ",text) #기자 제거
    ##뉴스정보 제거
    #text <- gsub("\\(\\S+(뉴스|신문)+\\)","",text) 
    #text <- gsub("\\(+[가-힣a-zA-Z]+=+\\)","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+=+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+=+[[:space:]]+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+[[:space:]]\\S+=+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+[[:space:]]+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+\\(+[가-힣a-zA-Z]+\\)+=+[가-힣a-zA-Z]+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+[[:space:]]+\\(+[가-힣a-zA-Z]+\\)+=+[가-힣a-zA-Z]+[[:space:]]+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+\\(+[가-힣a-zA-Z]+\\)+=+[가-힣a-zA-Z]+[[:space:]]+\\]","",text)
    #text <- gsub("\\[+[가-힣a-zA-Z]+[[:space:]]+[가-힣a-zA-Z]+\\]","",text)
    #text <- gsub("\\【+[가-힣a-zA-Z]+=+\\】","",text)
    #text <- gsub("\\【+[가-힣a-zA-Z]+\\(+[가-힣a-zA-Z]+\\)+=+\\】","",text)
    #text <- gsub("\\【+[가-힣a-zA-Z]+\\(+[가-힣a-zA-Z]+\\)+=+[가-힣a-zA-Z]+\\】","",text)
    #text <- gsub("\\【+[가-힣a-zA-Z]+[[:space:]]+\\(+[가-힣a-zA-Z]+\\)+=+[가-힣a-zA-Z]+\\】","",text)
    #text <- gsub("\\【+[가-힣a-zA-Z]+=+[가-힣a-zA-Z]+\\】","",text)
    #text <- gsub("\\【+[가-힣a-zA-Z]+=+[가-힣a-zA-Z]+\\/+[가-힣a-zA-Z]+\\】","",text)
    #text <- gsub("\\［+[가-힣a-zA-Z]+=\\］","",text)
    #text <- gsub("\\［+[가-힣a-zA-Z]+[[:space:]]+=\\］","",text)
    #text <- gsub("\\［+[가-힣a-zA-Z]+=+[[:space:]]+\\］","",text)
    #text <- gsub("\\［+[가-힣a-zA-Z]+[[:space:]]+=+[[:space:]]+\\］","",text)
    #text <- gsub("\\[+\\]+[[:space:]]","",text)
    #text <- gsub("\\[+[[:space:]]\\S+=+\\]","",text)
    #text <- gsub("   ","  ",text)
    #text <- gsub("  ","",text)
    #text <- gsub("\\[+\\]","",text)
    #text <- gsub("\\[+[[:space:]]+\\]","",text)
    #text
    if (substr(text,1,1) == " ") text <- substr(text,2,nchar(text))
    # 발행일 정보
    date <- html %>%  html_nodes(xpath ="//span[@class='t11']") %>% html_text()
    if (length(date) == 2) date <- date[1]
    if (length(date) == 0){
      date <- html %>%  html_nodes(xpath ="//span[@class='author']") %>% html_nodes("em") %>% html_text()
      if (length(date) == 2) date <- date[1]
      date <- gsub("오전 ","",date)
      if (nchar(date) == 18) date <- paste0(substr(date,1,10)," ",(as.numeric(gsub("\\D","",substr(date,15,16)))+12),":",substr(date,nchar(date)-1,nchar(date)))
    }
    
    keyword <- as.character(t.urls2[j,1])
    head(t.urls2)
    type <- as.character(t.urls2$type[j])
    data1 <- as.data.frame(cbind(date,url, header,text, keyword,source,type))
    if (testObject(data11) == F) data11 <- data1 else data11 <- rbind(data11, data1)
  }, error=function(e){cat(j," ERROR :",conditionMessage(e), "\n")})
}

data  <- data11
data  <- data[data$date != "",]
data  <- data[data$header != "",]
data  <- data[data$text != "",]
data  <- data[data$keyword != "",]
data  <- data[data$url != "",]

data$date <- data$date

data$id <- ""
data$like_count <- 0
data$regdate <- as.character(Sys.time())
data$complete <- 0
data$view <- 0

data$text <- gsub("// flash 오류를 우회하기 위한 함수 추가function _flash_removeCallback",
                  "", data$text)

data$header <- gsub("\r","",data$header)
data$header <- gsub("\t","",data$header)
data$header <- gsub("\n","",data$header)

data$text <- gsub("co","",data$text)
data$text <- gsub("kr","",data$text)
data$date <- substr(data$date,1,10)
data$date <- gsub("\\.","-",data$date)
data <- data[!is.na(data$text),]

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
data <- data[data$date == today,]

## DB Upload
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
dbWriteTable(con, "news_raw", data, append=T, row.names=F)
dbDisconnect(con)

#}

