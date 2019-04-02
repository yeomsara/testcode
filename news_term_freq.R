#### TERM_FREQ
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
library(RCurl)
library(wordcloud)
library(RColorBrewer)

to <- Sys.time()-60 ## 1분전
from <- Sys.time()-660 ## 11분 전, 총 10분간 데이터 수집, 크롤러는 1, 10, 21, 31, 41, 51분에 실행
to <- paste(substr(to,1,17),"00",sep="")
from <- paste(substr(from,1,17),"01",sep="")

query <- paste("select id, header, text from news_raw where regdate > '",from,"' and regdate < '",to,"'", sep="") ## 쿼리작성
# query <- paste0("select id, header, text from news_raw where id = 274043")
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
data <- dbGetQuery(con, query) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제
data$text <- paste(data$header, data$text, sep=". ")
data <- data[,c("id","text")]

## parallel
registerDoParallel(cores = 5)

if (length(data) != 0){ ## 데이터 존재 시
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
  
  words <- foreach(i=1:length(data[,1]), .combine=rbind, .packages=c("ff","RMeCab")) %dopar% {extract_nouns(i, data, 2)}
  par(mar=c(0,0,0,0))
  for (i in 1:length(data[,1])){
    myCorpus <- Corpus(VectorSource(words[i]))
    myCorpus <- tm_map(myCorpus, removeNumbers)
    myStopwords <- c(stopwords("english"))
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths = c(1,Inf)))
    termFreq <- rowSums(as.matrix(tdm))
    termFreq <- termFreq[order(termFreq, decreasing=T)]
    termFreq <- as.data.frame(termFreq)
    termFreq$term <- row.names(termFreq)
    termFreq <- termFreq[,c("term","termFreq")]
    termFreq <- termFreq[nchar(termFreq$term)>1,]
    a <- termFreq
    names(a) <- c("term","freq")
    a <- a[1:20,]
    a <- na.omit(a)
    term <- as.character(paste(a$term, collapse=","))
    freq <- as.character(paste(a$freq, collapse=","))
    id <- as.numeric(data$id[i])
    up <- as.data.frame(cbind(id, term, freq))
    if (i == 1) upload <- up else upload <- rbind(upload, up)
    
    c <-  termFreq[1:200,]
    c <- na.omit(c)
    pal <- brewer.pal(5,"Accent")
    png(paste0("R/png/wordcloud_",as.character(data$id[i]),".png"), width=500,height=420,bg="transparent")
    wordcloud(c$term, c$termFreq, min.freq=1, scale=c(9,.1), random.order=T, colors=pal, rot.per=.15, family="BM DoHyeon")
    dev.off()
    
    ftpUpload(what = paste0("R/png/wordcloud_",as.character(data$id[i]),".png"),
              to = paste0("ftp://192.168.0.21//home/ftp/var/etc/wordcloud_",as.character(data$id[i]),".png"),
              verbose = TRUE,
              #userpwd = "ztcit:ztc20110104", prequote="CWD /home/ftp/var/etc")
              userpwd = "ztc-server:ztcserver@@", prequote="CWD /home/ftp/var/etc")
    
  }
  
  upload$regdate <-as.character(Sys.time())
  
  names(upload)
  
  min(as.numeric(as.character(upload$id)))
  max(as.numeric(as.character(upload$id)))
  
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
  dbGetQuery(con,"set names utf8")
  dbWriteTable(con, "news_term_freq",upload, append=T, row.names=F)
  dbDisconnect(con)
}