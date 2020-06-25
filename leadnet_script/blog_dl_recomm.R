rm(list=ls())
gc()
options(warn=-1)

library(tm)
library(e1071)
library(RWeka)
library(RMeCab)
library(parallel)
library(doParallel)
library(ff)
library(slam)
library(lsa)
library(caret)
library(Matrix)
library(SparseM)
library(rJava)
library(RMySQL)
library(h2o)

curdate <- as.character(Sys.Date())

args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0) {
  query <- paste0("select id, text from blog_raw where date = '",args[1],"' and complete=0 ") ## 쿼리작성
} else {
  query <- paste0("select id, text from blog_raw where date = '",curdate,"' and complete=0 ") ## 쿼리작성
}
#query <- paste0("select id, text from blog_raw where date >= '2011-10-20' and complete=0 ") ## 쿼리작성
#query <- paste0("select id, text from blog_raw where complete=0 ") ## 쿼리작성
#query <- "select id, text from news_raw"
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
data <- dbGetQuery(con, query) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제

data <- data[!is.na(data$text),]

for (i in 1:length(data[,1])){
  tryCatch({
    text <- data$text[i]
    text <- strsplit(text,".",fixed=T)
    text <- text[[1]][nchar(text[[1]]) > 5]
    text <- text[1:(length(text)-1)]
    text <- gsub("[가-힣a-zA-Z]+@+[가-힣a-zA-Z]","",text)
    text <- text[nchar(text) > 5]
    if (length(text) > 5){
      text <- paste(text, collapse=". ")
    } else text <- NA
    data$text[i] <- text
  }, error=function(e){cat(i," ERROR :",conditionMessage(e), "\n")})
}


if (length(data[,1]) > 0){
  rm(list= ls()[!(ls() %in% c('data'))])
  load("/home/ztc-linux/R/model/bdl2.RData")
  
  
  vsm                <- function(message, highlyrepeatedwords){                                         
    tokenizedmessage <- unlist(RMeCabC(message))                                                         ## RMeCabC : 형태소 분석
    ## unlist : a list into a single vector
    tmpmessage       <- tokenizedmessage[names(tokenizedmessage)=="NNG"|names(tokenizedmessage)=="NNP"|  ## NNG, NNP, SL, SH(명사)만 추출
                                           names(tokenizedmessage)=="SL"|names(tokenizedmessage)=="SH"]
    
    #making vector
    v                <- rep(0, length(highlyrepeatedwords))                                              ## replication of zero
    for(i in 1:length(highlyrepeatedwords)){
      for(j in 1:length(tmpmessage)){
        if(highlyrepeatedwords[i]==tmpmessage[j]){                                                       ## highlyrepeatedwords가 기사 안에 존재하면
          v[i] <- v[i]+1                                                                                 ## 빈도수 1 증가
        }
      }
    }
    return(v)                                                                                            ## v 반환
  }
  
  #vectorized new data set
  vnedata=NULL
  
  for(i in 1:length(data[,2])){
    tryCatch({
      vnedata <- rbind(vnedata, c(data$id[i],vsm(data[i,2], highlyrepeatedwords)))
      
    }, error=function(e){cat(i," ERROR :",conditionMessage(e), "\n")})
  }
  
  h2o.init()
  vnedata   <- as.data.frame(vnedata)
  colnames(vnedata) <- c("id",highlyrepeatedwords)
  ############## id가 중복되는 경우가 생길수있음 ################## 
  vnedata <- vnedata[,!duplicated(names(vnedata))]
  vne_data <- as.h2o(vnedata)
  vne_data <- vne_data[-1,-1]
  id <- vnedata[,1]
  ###prediction
  saved_model <- h2o.loadModel("/home/ztc-linux/R/model/DeepLearning_model_R_1487060413798_1")
  
  pred <- h2o.predict(saved_model, vne_data)
  pred <- as.data.frame(pred[,1:3])
  pred <- as.data.frame(cbind(id, pred))
  
  pred$regdate <- as.character(Sys.time())
  names(pred) <- c("id", "yn", "percent0", "percent1", "regdate")
  pred_result <- pred
  pred_result$percent1 <- as.numeric(as.character(pred_result$percent1))
  pred_result$percent1 <- round(pred_result$percent1, digits=4)*100
  pred_result$percent0 <- round(pred_result$percent0, digits=4)*100
  tail(pred_result)
  
  pred_result$yn <- 0
  ##### 불필요한 for문제거 ===> 사라수정
  # for (j in 1:length(pred_result[,1])){
  #   if (pred_result$percent1[j] > 95) pred_result$yn[j] <- 1
  # }
  pred_result$yn[pred_result$percent1 > 97] <- 1
  table(pred_result$yn)
  # count <- table(pred, new$like_count)
  # sum(count[1,1],count[2,2])/sum(count)*100   
  # count <- table(pred, new$like_count)
  # sum(count[1,1],count[2,2])/sum(count)*100                                                         ## calculate accuracy
  
  id <- paste(pred_result$id,collapse = ",")
  
  ## DB Upload
  con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
  # for문을 왜쓰지 ...? ----> 사라수정
  # for (i in 1:length(pred_result[,1])){
  #   id <- pred_result$id[i]
  #   print(i)
  dbGetQuery(con,paste0("update blog_raw set complete=1 where id in(",id,")"))
  # }
  dbGetQuery(con,"set names utf8")
  dbWriteTable(con, "blog_recom", pred_result, append=T, row.names=F)
  dbDisconnect(con)
}
