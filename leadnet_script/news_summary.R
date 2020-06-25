#### News Summary
rm(list=ls()) ## Clear Environment

library(RMeCab) ## 은전한닢 형태소분석기
library(RMySQL) ## MySQL
library(stringi) ## Text Handling
library(LSAfun) ## LSA Function(summary function)
library(tm) ## text mining
library(parallel) ## parallel
library(doParallel) ## parallel
library(ff) ## virtual Memory
library(plyr) ## data Handling
library(RMySQL)

to <- Sys.time()-60 ## 1분전
from <- Sys.time()-660 ## 11분 전, 총 10분간 데이터 수집, 크롤러는 1, 10, 21, 31, 41, 51분에 실행
to <- paste(substr(to,1,17),"00",sep="")
from <- paste(substr(from,1,17),"01",sep="")

query <- paste("select id, text from news_raw where regdate > '",from,"' and regdate < '",to,"'", sep="") ## 쿼리작성
#query <- paste("select id, text from news_raw", sep="") ## 쿼리작성
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
data <- dbGetQuery(con, query) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제

if (length(data) != 0){ ## 데이터 존재 시
  ## 전처리
  text <- data[,c("id","text")]
  text$text <- gsub("  ","", text$text)
  text <- text[text$text != "",]
  text$text <- gsub("본문 이미지 영역","",text$text)
  
  for (i in 1:length(text[,1])){
    temp <- text[i,2]
    id <- text[i,1]
    temp <- strsplit(temp,".",fixed=T)
    temp <- temp[[1]][nchar(temp[[1]]) > 5]
    temp <- temp[1:(length(temp)-1)]
    temp <- gsub("[가-힣a-zA-Z]+@+[가-힣a-zA-Z]","",temp)
    temp <- temp[nchar(temp) > 5]
    temp <- paste(temp, collapse=". ")
    table <- as.data.frame(cbind(id,temp))
    
    if (i == 1) total <- table else total <- rbind(total, table)
  }
  
  ## parallel
  (numCores <- detectCores() - 5) # CPU를 모두 사용할 경우 R 이외의 작업할 때 컴퓨터가 버벅거리기도 해서 1개 뺌 
  (cl <- makeCluster( numCores ))
  registerDoParallel(cl)
  
  ## Summary
  summary <- foreach(i=1:length(text[,1]), .packages=c("ff","LSAfun"), .combine=rbind) %dopar%{
    tryCatch({
      a <- unique(genericSummary(as.character(text$text[i]), k=3))
      as.data.frame(cbind(a[1],a[2],a[3],text$id[i]))
    }, error=function(e){cat(i," ERROR :",conditionMessage(e), "\n")})
  }
  
  stopImplicitCluster() ## parallel 해제
  
  names(summary) <- c("summary1","summary2","summary3","id")
  
  ## 조인걸기위한 전처리
  summary$regdate <- as.character(Sys.time())
  head(summary)
  ## DB Upload
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
  dbGetQuery(con,"set names utf8")
  dbWriteTable(con, "news_summary", summary, append=T, row.names=F)
  dbDisconnect(con)
}