packageurl <- "https://cran.r-project.org/src/contrib/Archive/h2o/h2o_3.10.3.3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
library(h2o)
# Finally, let's load H2O and start up an H2O cluster

h2o.init()



#sessionInfo()
#install.packages("h2o")
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
library(plyr)
#library(h2o)

#query <- paste("select id, text from news_raw where regdate > '",from,"' and regdate < '",to,"'", sep="") ## 쿼리작성
query <- "select id, header, text from all_news_raw where view > 0" ## 쿼리작성
query1 <- "select id from news_like"## 쿼리작성

require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
data <- dbGetQuery(con, query) ## 데이터 받아오기
like <- dbGetQuery(con, query1) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제

like$like_count <- 1
data <- join(data, like, by="id")

data$like_count[is.na(data$like_count)] <- 0
table(data$like_count)

parts<-createDataPartition(data$id,p=0.7)
train<-data[parts$Resample1,]
test<-data[-parts$Resample1,]

(numCores <- detectCores() - 2)
registerDoParallel(cores=numCores)

trword        <- list()                                                                                ## trword 리스트 만들기
word          <- c()          
extract_nouns     <- function(i, data, j){                                                                          
  trword    <- RMeCabC(data[i,j])                                                                    ## RMeCabC : trdata[i,2] 데이터 형태소 분석
  trword2   <- unlist(trword)                                                                        ## unlist : a list into a single vector
  trword    <- trword2[names(trword2)=="NNG"|names(trword2)=="NNP"|                                  ## NNG, NNP, SL, SH(명사) 추출
                         names(trword2)=="SL"|names(trword2)=="SH"]                                     
  word      <- paste(trword, collapse=" ")                                                           ## list 형태인 trword에서 단어 사이의 공백 제거
  print(i)
  return(word)
}

tr.word <- foreach(i=1:length(train[,1]), .combine=rbind, .packages=c("ff","RMeCab")) %dopar% {extract_nouns(i, train, 3)}      
te.word <- foreach(i=1:length(test[,1]), .combine=rbind, .packages=c("ff","RMeCab")) %dopar% {extract_nouns(i, test, 3)}      

tr.words          <- Corpus(VectorSource(tr.word))                                                         ## 말뭉치 데이터 만들기
tdm           <- TermDocumentMatrix(tr.words, control=list(wordLengths=c(1,Inf)))          
rs                  <- row_sums(tdm)                                                                   ## tdm에서 counting된 빈도테이블의 
highlyrepeatedwords <- findFreqTerms(tdm, max(rs)*0.01)                                                 ## 제일 높은 빈도수*0.1 까지 counting된 단어만 추출
highlyrepeatedwords <- highlyrepeatedwords[nchar(highlyrepeatedwords) > 1]

### Spam/Ham
#This function makes vector from the news
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

#vectorized training data set
vtrdata=NULL

#vectorized test data set
vtedata=NULL

for(i in 1:length(train[,1])){                                                                        ## highlyrepeatedwords : 빈도수 높은 단어
  if(train$like_count[i] == 1){                                                                                  ## data == 1 : like count가 1이면
    vtrdata <- rbind(vtrdata, c(1,vsm(train$text[i],highlyrepeatedwords)))                               ## vsm 함수를 이용하여 trdata의 텍스트에서
  }                                                                                                    ## highlyrepeatedwords가 몇 번 나타나는지 체크
  else {
    vtrdata <- rbind(vtrdata, c(0,vsm(train$text[i],highlyrepeatedwords)))
  }
}

for(i in 1:length(test[,1])){                                                                        ## 80번째 line 참고
  if(test$like_count[i] == 1){
    vtedata <- rbind(vtedata, c(1,vsm(test$text[i],highlyrepeatedwords)))
  }
  else {
    vtedata <- rbind(vtedata, c(0,vsm(test$text[i],highlyrepeatedwords)))
  }
}


vtedata     <- as.data.frame(vtedata)                                                                 ## vtedata를 데이터프레임으로 변환
vtedata     <- cbind(test[,1], vtedata)                                                         ## (tedata[,1] : id) 와 vtedata를 combine by columns
vtedata[,2] <- as.factor(vtedata[,2])                                                            ## vtedata[,2] : like count가 zero or 1 -> factor로 변환

vtrdata <- as.data.frame(vtrdata)
vtrdata <- cbind(train[,1], vtrdata)
colnames(vtrdata)[3:length(vtrdata[1,])] <- highlyrepeatedwords
colnames(vtedata)[3:length(vtedata[1,])] <- highlyrepeatedwords
vtrdata[,2] <- as.factor(vtrdata[,2])

h2o.init()
vtrdata <- vtrdata[, -1]  # remove the ID column

vtedata <- vtedata[, -1]  # remove the ID column

vtr_data <- as.h2o(vtrdata)
vtr_data <- vtr_data[-1,]
vte_data <- as.h2o(vtedata)
vte_data <- vte_data[-1,]

dl <- h2o.deeplearning(x = 2:length(vtr_data), y = 1, training_frame = vtr_data, hidden=c(50,50,50,50,50),
                       ignore_const_cols=F)
# now make a prediction
predictions1 <- h2o.predict(dl, vtr_data[,2:length(vtr_data)])
predictions2 <- h2o.predict(dl, vte_data[,2:length(vte_data)])
pred1 <- as.data.frame(predictions1)
pred2 <- as.data.frame(predictions2)
check1 <- as.data.frame(cbind(vtrdata[,1], pred1$predict))
check2 <- as.data.frame(cbind(vtedata[,1], pred2$predict))
confusionMatrix(table(check1))
confusionMatrix(table(check2))

model_path <- h2o.saveModel(object=dl, path=getwd(), force=TRUE)
model_path
rm(list= ls()[!(ls() %in% c('highlyrepeatedwords'))])

save.image(file="dl2.RData")
