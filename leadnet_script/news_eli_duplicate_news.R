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
library(stringdist)
to <- Sys.Date() ## 1분전
from <- Sys.Date()-2

### exist data Download
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
query <- paste("select * from all_news_raw where date > '",from,"' and date <= '",to,"'", sep="") ## 쿼리작성
dbGetQuery(con, "set names utf8")
data <- dbGetQuery(con, query)
dbDisconnect(con)
data <- data[,c("id","header","type","yn")]
data <- data[data$yn ==1,]

testObject <- function(object)
{
  exists(as.character(substitute(object)))
}

for (l in 1:length(table(data$type))){
  data1 <- data[data$type == as.data.frame(table(data$type))[l,1],]
  data2 <- vector("list", length(data1[,1]))
  
  for (i in 1:length(data1[,1])){
    trword        <- list()
    word          <- c()
    trword    <- RMeCabC(data1[i,2])
    trword2   <- unlist(trword)
    trword    <- trword2[names(trword2)=="NNG"|names(trword2)=="NNP"|
                           names(trword2)=="SL"|names(trword2)=="SH"]
    word <- as.vector(trword)
    data2[[i]] <- word
  }
  
  for (j in 1:length(data2)){
    for (k in 1:length(data2)){
      mat <- length(intersect(data2[[j]],data2[[k]]))
      id1 <- data1[j,1]
      id2 <- data1[k,1]
      table <- as.data.frame(cbind(id1, id2, mat))  
      if (k == 1) table1 <- table else table1 <- rbind(table1, table)
    }
    if(j == 1) table2 <- table1 else table2 <- rbind(table2, table1)
  }
  
  table2$check <- ifelse(table2$id1 == table2$id2,1, 0)
  table2 <- table2[table2$check == 0,]
  table2 <- table2[table2$mat > 3,]
  
  if (length(table2[,1]) > 0){
    for( i in 1:length(table(table2$id1))){
      a <- table2[table2$id1 == as.data.frame(table(table2$id1))[i,1],]
      b <- unique(c(a$id1,a$id2))
      b <- b[order(b)]
      b <- paste(b, collapse=", ")
      if (i == 1) c <- b else c <- rbind(c,b)
    }
    c <- unique(c)
    c <- strsplit(c, ",", fixed=T)
    
    
    for ( i in 1:length(c)){
      a <- as.data.frame(c[[i]])
      a$yn <- 0
      a$yn[1] <- 1
      if (i == 1) b <- a else b <- rbind(b,a)
    }
    if (testObject(d) == F) d <- b else d <- rbind(d,b)
  }
}
d <- unique(d)
head(d)

require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
for (i in 1:length(d[,1])){
  id <- as.character(d[i,1])
  yn <- as.numeric(as.character(d[i,2]))
  dbGetQuery(con,paste0("update all_news_raw set yn=",yn," where id=",id))
  dbGetQuery(con,paste0("update news_recom set yn=",yn," where id=",id))
}
dbDisconnect(con)
