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

to <- as.character(Sys.Date())
from <-  as.character(Sys.Date()-2)

query <- paste0("SELECT A.id,A.header,A.text                                        
                FROM blog_raw A                                                
                LEFT OUTER JOIN blog_summary B on A.id = B.id                      
                WHERE A.date = '",to,"' and B.id is null") ## 쿼리작성
# query <- paste0("SELECT id,header,text from blog_raw WHERE date = '2017-03-09'")
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="**",dbname="**",host="**") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
data <- dbGetQuery(con, query) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제

if (length(data) != 0){ ## 데이터 존재 시
  ## 전처리
  text <- data[,c("id","text")]
  text$text <- gsub("  ","", text$text)
  text <- text[text$text != "",]
  
  ## parallel
  (numCores <- detectCores() - 2) # CPU를 모두 사용할 경우 R 이외의 작업할 때 컴퓨터가 버벅거리기도 해서 1개 뺌 
  registerDoParallel(numCores)
  
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
  ## DB Upload
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="**",dbname="**",host="**") ## DB 접속
  dbGetQuery(con,"set names utf8")
  dbWriteTable(con, "blog_summary", summary, append=T, row.names=F)
  dbSendQuery(con,paste0("UPDATE blog_summary SET summary1 = REPLACE(summary1,'&quot;','') WHERE summary1 like '%&quot;%' "))
  dbDisconnect(con)
}
