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
from <- Sys.Date()- 3

query1 <- paste0("SELECT L1.id,L1.date,L1.header,L1.text,L1.type
                 FROM  all_news_raw AS L1 
                 where L1.date >= '",from,"' and L1.date <= '",to,"'")

# query1 <- paste0("SELECT L1.id,L1.date,L1.header,L1.text,L1.type, L2.idx
#                  FROM  all_news_raw AS L1 
#                  LEFT JOIN news_like AS L2
#                  ON L1.id=L2.id
#                  where L1.date >= '",from,"' and L1.date <= '",to,"'")
# 

require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
data <- dbGetQuery(con, query1) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제
# data <- data[!is.na(data$idx),]
data$text <- paste(data$header, data$text, sep=". ")
names(data)
data <- data[,c("id","date","text","type")]

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
words <- as.data.frame(words)
names(words) <- "words"
data <- cbind(data, words)
for(i in 1:length(table(data$type))){
  table <- data[data$type==as.character(as.data.frame(table(data$type))[i,1]),]
  wordsi <- as.character(table$words)
  myCorpus <- Corpus(VectorSource(wordsi))
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
  termFreq <- termFreq[1:10,]
  termFreq$type <- as.character(as.data.frame(table(data$type))[i,1])
  if (i == 1) total <- termFreq else total <- rbind(total, termFreq)
}

total$cat <- "news"
total$regdate <- as.character(Sys.time())
head(total)


require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
dbSendQuery(con, "delete from term_freq_all where cat='news'")
dbWriteTable(con, "term_freq_all",total, append=T, row.names=F)
dbDisconnect(con)

