#### Dashboard
rm(list=ls()) ## Clear Environment
library(RMeCab) ## 은전한닢 형태소분석기
library(RMySQL) ## MySQL
library(stringi) ## Text Handling
library(plyr) ## data Handling
library(jsonlite)
library(parallel)
library(doParallel)
library(ff)
library(tm)
library(wordcloud)
library(extrafont)
library(RCurl)

to <- Sys.Date() ## 1분전
from <- Sys.Date()- 7

query1 <- paste0("SELECT L1.id,L1.date,L1.header,L1.text,L1.keyword,L2.yn FROM  news_raw AS L1 LEFT JOIN news_recom AS L2 ON L1.id=L2.id where L1.date >= '",from,"' and L1.date <= '",to,"'")
query1
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
data <- dbGetQuery(con, query1) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제
names(data)
data$text <- paste(data$header, data$text, sep=". ")
data <- data[,c("id","date","text","keyword","yn")]

trword        <- list()
word          <- c()
extract_nouns     <- function(i, data, j){
  trword    <- RMeCabC(data[i,j])
  trword2   <- unlist(trword)
  trword    <- trword2[names(trword2)=="NNG"|names(trword2)=="NNP"|
                         names(trword2)=="SL"|names(trword2)=="SH"]
  word      <- paste(trword, collapse=" ")
  return(word)
}

## term Freq
words <- foreach(i=1:length(data[,1]), .combine=rbind, .packages=c("ff","RMeCab")) %do% {extract_nouns(i, data, 3)}
myCorpus <- Corpus(VectorSource(words))
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- c(stopwords("english"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths = c(1,Inf)))
termFreq <- rowSums(as.matrix(tdm))
termFreq <- termFreq[order(termFreq, decreasing=T)]
termFreq <- as.data.frame(termFreq)
termFreq$term <- row.names(termFreq)
termFreq$nchar <- nchar(termFreq$term)
termFreq <- termFreq[termFreq$nchar > 1,]
termFreq <- termFreq[,c("term","termFreq")]
names(termFreq) <- c("term","freq")
termFreq$regdate <- as.character(Sys.time())
c <-  termFreq[1:200,]
termFreq <- termFreq[1:20,]

## 수집현황
trend <- data[,c("date","yn")]
trend1 <- as.data.frame(table(trend$date))
trend2 <- table(trend$date,trend$yn)
trend <- cbind(trend1, trend2[,2])
names(trend) <- c("date","crawled","recomm")
trend$recomm_per <- round((trend$recom/trend$crawled)*100,2)
trend$crawled_diff <- c(0,diff(trend$crawled))
trend$recomm_diff <- c(0,diff(trend$recomm))
trend$recomm_per_diff <- c(0,diff(trend$recomm_per))
trend$regdate <- as.character(Sys.time())

## pie chart
pie <- as.data.frame(table(data$keyword))
pie <- pie[order(pie$Freq, decreasing=T),]
pie1 <- pie[1:9,]
pie2 <- pie[10:length(pie[,1]),]
pie2 <- as.data.frame(cbind("기타",sum(pie2$Freq)))
names(pie2) <- names(pie1)
pie <- rbind(pie1,pie2)
names(pie) <- c("term","freq")
pie$regdate <- as.character(Sys.time())


## DB Upload
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
dbSendQuery(con,"delete from dash_term_freq")
dbSendQuery(con,"delete from dash_trend")
dbSendQuery(con,"delete from dash_pie")
dbWriteTable(con, "dash_term_freq",termFreq, append=T, row.names=F)
dbWriteTable(con, "dash_trend",trend, append=T, row.names=F)
dbWriteTable(con, "dash_pie",pie, append=T, row.names=F)
dbDisconnect(con)

c <- na.omit(c)
pal <- brewer.pal(5,"Accent")
png("R/png/wordcloud.png", width=400,height=400,bg = "transparent")
wordcloud(c$term, c$freq, min.freq=1, max.freq=Inf,scale=c(8,.2), random.order=T, colors=pal, rot.per=.15, family="BM DoHyeon")
dev.off()

ftpUpload(what = "R/png/wordcloud.png",
          to = "ftp://192.168.0.21//home/ftp/var/etc/wordcloud.png",
          verbose = TRUE,
          userpwd = "ztc-server:ztcserver@@", prequote="CWD /home/ftp/var/etc")
