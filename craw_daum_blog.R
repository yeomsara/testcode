rm(list=ls())
gc()
library(RCurl)
library(XML)
library(httr)
library(rvest)
library(jsonlite)
library(plyr)
library(stringi)
library(stringr)
library(dplyr)

## Keyword Download
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
data <- dbGetQuery(con, "select * from blog_keyword")
dbDisconnect(con)
data$keyword <- repair_encoding(data$keyword)
keyword.t <- data$keyword

for (k in 1:length(keyword.t)){
  keyword <- data$keyword[k]
  type <- as.character(data$type[k])
  ## 페이지 수 확인
  url <- paste("http://search.daum.net/search?q=",keyword,"&w=blog&m=board&f=section&SA=daumsec&lpp=10&nil_src=blog&page=1&DA=PGD", sep="")
  url <- enc2utf8(url)
  html <- read_html(as.character(url), encoding="UTF-8")
  PARSED <- htmlParse(url)
  times <- html_nodes(html, xpath="//span[@id='resultCntArea']")%>% html_text
  times <- stri_conv(times, from="UTF-8")
  times <- substr(times,5,16)
  times <- gsub("\\D","",times)
  times <- round(as.numeric(as.character(times))/10,0)
  times
  
  ## 총 페이지수만큼 가져오기, 일단 속도를 위해 10페이지까지만 진해
  #for(j in 1:times){
  for(j in 1:5){
    url <-paste0("http://search.daum.net/search?q=",keyword,"&w=blog&m=board&f=section&SA=daumsec&lpp=10&nil_src=blog&page=",j,"&sort=recency&DA=STC")
    #     url <- paste("http://search.daum.net/search?q=",keyword,"&w=blog&m=board&f=section&SA=daumsec&lpp=10&nil_src=blog&page=",j,"&DA=PGD", sep="")
    url <- enc2utf8(url)
    url <- GET(url, timeout(200))
    html <- read_html(as.character(url), encoding="UTF-8")
    ########## 2017-03-31변경 sara################
#     link <- html %>%  html_nodes(xpath="//*[@id='blogResultUl']") %>% html_nodes("a") %>% html_attr("href")
    link <- html %>%  html_nodes(xpath="//a[@class='f_link_b']") %>% html_attr("href")
    link <- as.data.frame(link)
    link <- unique(link)
#     link <- as.data.frame(as.character(link[substr(link[,1],1,1) == "h",]))
    names(link) <- "url"
    link <- as.data.frame(cbind(keyword,link, type))
    urls <- link
    if (j == 1) urls1 <- urls else urls1 <- rbind(urls1, urls)
  }
  if (k == 1) urls2 <- urls1 else urls2 <- rbind(urls2, urls1)
}

t.urls <- urls2
t.urls <- unique(t.urls) # 중복제거

rm(list= ls()[!(ls() %in% c('t.urls'))])

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
  names(data) <- c("keyword","url", "type")
  if(i==1) t.urls2 <- data else t.urls2 <- rbind(t.urls2, data)
}

t.urls2 <- unique(t.urls2)

t.urls2 <- t.urls2[substr(as.character(t.urls2$url),1,20) == 'http://blog.daum.net',]
## t.urls 제외하고 오브젝트 모두 삭제
rm(list= ls()[!(ls() %in% c('t.urls2'))])
head(t.urls2)
## Check DB
## Eliminate Duplicate blogs
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
urls <- dbGetQuery(con, "select url from blog_raw where source='daum'")
dbDisconnect(con)

if (length(urls[,1]) > 0){
  urls$eli <- "O"
  t.urls <- join(t.urls2, urls, by="url",type="left", match="all")
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

## Eliminate Complete
if (length(t.urls2[,1]) > 0){
  testObject <- function(object)
  {
    exists(as.character(substitute(object)))
  }
  ## 다음 블로그 개별페이지 스크래핑(biz)
  for (j in 1:length(t.urls2[,1])){
    tryCatch({
      url <- as.character(t.urls2[j,2])
      url1 <- GET(url, timeout(200))
      html <- read_html(as.character(url1))
      url
      id <- html %>%  html_nodes("frame") %>% html_attr("src")
      id <- id[1]
      id <- gsub("blog/BlogTypeView.do","",id)
      id <- gsub("_?blogid=","",id)
      id <- strsplit(id,"&", fixed=T)
      id <- unlist(id)
      id[1] <- substr(id[1],4,nchar(id[1]))
      id[2] <- gsub("articleno=","",id[2])
      blog <- id[2]
      id <- id[1]
      
      url11 <- paste("http://blog.daum.net/_blog/hdn/ArticleContentsView.do?blogid=",id,"&articleno=",blog,"&looping=0&longOpen=",sep="")
      url1 <- GET(url11, timeout(200))
      html <- read_html(as.character(url1))
      
      header <- html %>%  html_nodes(xpath ="//html/head/title") %>% html_text()
      header <- stri_conv(header, from="UTF-8")[1]
      header <- gsub("\r","",header)
      header <- gsub("\n","",header)
      header <- gsub("\t","",header)
      header <- gsub("\\ㅋ","",header)
      header <- gsub("\\ㅎ","",header)
      header <- gsub("\\ㅎ","",header)
      header <- gsub("  ","",header)
      header <- gsub(pattern="[[:punct:]]","",header)
      
      text <- html %>%  html_nodes(xpath ="//p") %>% html_text()
      text <- stri_conv(text, from="UTF-8")
      text <- gsub("\r","",text)
      text <- gsub("\n","",text)
      text <- gsub("\t","",text)
      text <- gsub("  ","",text)
      text <- gsub("다 ","다. ",text)
      text <- gsub("요 ","요. ",text)
      text <- paste(text, collapse=" ")
      text <- gsub("  ","",text)
      
      url2 <- paste("http://blog.daum.net/_blog/BlogTypeView.do?blogid=",id,"&articleno=",blog,"&admin=",sep="")
      url2 <- GET(url2, timeout(200))
      html1 <- read_html(as.character(url2))
      date <- html1 %>% html_nodes("span") %>% html_nodes(xpath="//*[@id='cContentFunc']/div[1]/div/span[2]") %>% html_text()
      date <- stri_conv(date, from="UTF-8")
      date <- substr(date, 1, 10)
      date <- gsub("\\D","",date)
      date <- paste(substr(date,1,4),"-",substr(date,5,6),"-",substr(date,7,8),sep="")
      keyword <- as.character(t.urls2[j,1])
      type <- as.character(t.urls2$type[j])
      
      data1 <- as.data.frame(cbind(date,url, header,text, keyword, type))
      head(data1)
      if (testObject(data11) == F) data11 <- data1 else data11 <- rbind(data11, data1)
    }, error=function(e){cat(j," ERROR :",conditionMessage(e), "\n")})
  }
  rm(list= ls()[!(ls() %in% c('data11','t.urls2'))])
  
  data <- data11
  data$id <- ""
  data$source <- "daum"
  data$like_count <- 0
  data$regdate <- as.character(Sys.time())
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
  head(data) 
  ## DB Upload
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
  dbGetQuery(con,"set names utf8")
  dbWriteTable(con, "blog_raw", data, append=T, row.names=F)
  dbDisconnect(con)
  
}

