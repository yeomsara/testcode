## kiscon G2B
rm(list=ls())

library(RCurl)
library(XML)
library(httr)
library(rvest)
library(jsonlite)
library(plyr)
library(stringi)
library(stringr)

today <- as.character(Sys.Date())
fromday <- as.character(Sys.Date()-30)
fromBidDt <- paste0(substr(fromday,1,4),"%2F",substr(fromday,6,7),"%2F",substr(fromday,9,10))
toBidDt <- paste0(substr(today,1,4),"%2F",substr(today,6,7),"%2F",substr(today,9,10))

for (i in 1:300){
  tryCatch({
    #url <- paste0("http://www.g2b.go.kr:8101/ep/tbid/tbidList.do?area=&areaNm=&bidNm=&bidSearchType=1&budget=&budgetCompare=UP&currentPageNo=",i,"&detailPrdnm=&detailPrdnmNo=&fromBidDt=2017%2F01%2F01&fromOpenBidDt=&industry=&industryCd=&instNm=&instSearchRangeType=&intbidYn=&maxPageViewNoByWshan=12&procmntReqNo=&radOrgan=1&recordCountPerPage=30&refNo=&regYn=Y&searchDtType=1&searchType=1&taskClCds=&toBidDt=2017%2F01%2F31&toOpenBidDt=&")
    url <- paste0("http://www.g2b.go.kr:8101/ep/tbid/tbidList.do?searchType=1&bidSearchType=1&taskClCds=&bidNm=&searchDtType=1&fromBidDt=",fromBidDt,"&toBidDt=",toBidDt,"&fromOpenBidDt=&toOpenBidDt=&radOrgan=1&instNm=&instSearchRangeType=&refNo=&area=&areaNm=&industry=&industryCd=&budget=&budgetCompare=UP&detailPrdnmNo=&detailPrdnm=&procmntReqNo=&intbidYn=&regYn=Y&recordCountPerPage=30&&currentPageNo=",i)
    url <- enc2utf8(url)
    url <- GET(url, timeout(200))
    html <- read_html(as.character(url), encoding="UTF-8")
    link <- html %>%  html_nodes("a") %>% html_attr("href")
    link <- unique(link[substr(link,1,4) == "http"])
    cat <- html %>% html_nodes("tbody") %>% html_nodes("tr") %>% html_nodes("td") %>% html_nodes("div") %>%  html_text()
    table <- as.data.frame(matrix(cat, nrow=30, ncol=10, byrow=T))
    names(table) <- c("work","number","cat","header","facility1","facility2","waytocon","input_date","co_supply","tent")
    table$url <- link
    names(table)
    if (i == 1) data <- table else data <- rbind(data,table)
  }, error=function(e){cat(i," ERROR :",conditionMessage(e), "\n")})
}

data <- unique(data)
date <-  strsplit(as.character(data$input_date),"(",fixed=T)
for (j in 1:length(date)){
  input_date <- date[[j]][1]
  end_date <- gsub(")","",date[[j]][2])
  date_t <- as.data.frame(cbind(input_date,end_date))
  if( j == 1) date_table <- date_t else date_table <- rbind(date_table, date_t)
}
data <- data[,-8]
data <- cbind(data, date_table)
data$id <- ""


## 수정할 것
## Check DB
## Eliminate Duplicate News
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
urls <- dbGetQuery(con, "select number from kiscon_raw") ## 시간정보 줄 것
dbDisconnect(con)

if (length(urls) != 0){
  urls$eli <- "O"
  t.urls <- join(data, urls, by="number",type="left", match="all")
  t.urls <- t.urls[is.na(t.urls$eli),]
  data1 <- t.urls
  data1 <- data1[,-14]
}
head(data1)
data1$regdate <- as.character(Sys.time())

require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
# dbGetQuery(con,"set names ")
dbWriteTable(con, "kiscon_raw", data1, append=T, row.names=F)
dbDisconnect(con)