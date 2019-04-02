rm(list=ls())
gc()
library(lsa); library(RMeCab); library(tm); library(doParallel); library(RMySQL); library(plyr)
library(data.table); library(dplyr); library(gtools)
to <- Sys.Date() ## 1분전
from <- Sys.Date()-2

cons <- dbListConnections(MySQL())
for(con in cons) dbDisconnect(con)

curdate <- as.character(Sys.Date())
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0) {
  cat(sprintf("execute for date : %s\n", args[1]))
  query <- paste("select * from news_raw where date = '",args[1],"' and complete in (1,2)", sep="") ## 쿼리작성
} else {
  cat(sprintf("execute for date : %s\n", curdate))
  query <- paste("select * from news_raw where date = '",curdate,"' and complete in (1,2)", sep="") ## 쿼리작성
}
cat(query)
dbGetQuery(con, "set names utf8")
data <- dbGetQuery(con, query)
dbDisconnect(con)

rm(list=setdiff(ls(),c("data")))

## 데이터 분석
nouns    <- list()
nounstmp <- list()
name     <- c() 
header   <- c()

#ext_info <-foreach(i = 1:length(data[,1]), .combine=rbind,.packages=c("ff","RMeCab"))%dopar% {
for(i in 1:length(data[,1])){
#   print(i)
  if (length(header) < 1) {
    header <- data[i,c(1,4)]
  } else { 
    header <- rbind(header, data[i,c(1,4)])
    
    ## 헤더에서 단어 추출
    for(j in 1:length(header[,1])){
      nouns    <- RMeCabC(header[j,2])
      nounstmp <- unlist(nouns)
      nouns    <- nounstmp[names(nounstmp)=="NNG"|names(nounstmp)=="NNP"|names(nounstmp)=="SL"|names(nounstmp)=="SH"]
      z        <- paste(nouns, collapse=" ")
      if (j == 1) {
        z1 <- z
      } else {
        z1 <- rbind(z1,z)
      }
    }
    
    ## Create the frequency table
    z1   <- Corpus(VectorSource(z1))
    z1   <- TermDocumentMatrix(z1, control=list(wordLengths=c(1,Inf)))
    z1   <- as.matrix(z1)
    text <- as.textmatrix(z1)
    colnames(text) <- header[,1]
    
    rm(z1)
    
    ## LSA
    myMatrix    = lw_bintf(text) * gw_idf(text)
    ## Create the latent semantic space
    myLSAspace  = lsa(myMatrix, dims=dimcalc_raw())
    ## display it as a textmatrix again
    myNewMatrix = as.textmatrix(myLSAspace)
    ## compare two documents with cosine measure
    cortmp <- matrix(, nrow=(length(myNewMatrix[1,])-1), ncol=1)
    
    for(k in 1:length(cortmp[,1])){
      cortmp[k,1] <- cor(myNewMatrix[,k], myNewMatrix[,i], method="pearson")
    }
    
    cortmp <- as.data.frame(cortmp)
    row.names(cortmp) <- header$id[1:length(cortmp[,1])]
    colnames(cortmp)  <- header$id[i]
    
    name_row <- row.names(cortmp)[which(cortmp[,1] >= 0.425)]
    
    if(length(name_row) > 0){name <- rbind(name, paste(paste0(name_row, collapse=","),colnames(cortmp), sep=","))}
    else{name_row <- NA
         name <- rbind(name_row, name)}
  }
}

name <- na.omit(name)

# rm(list=setdiff(ls(), c("name")))

#### hw code
name <- as.data.frame(as.character(name))

for (i in 1:length(name[,1])){
  a <- strsplit(as.character(name[i,1]),",")[[1]][1]
  if(i == 1) key <- a else key <- rbind(key, a)
}

key <- as.character(key)
name$key <- key
name$key <- as.numeric(name$key)
name <- name[order(name$key),]
names(name)[1] <- "id"
name <- ddply(name,.(key), transform, total=paste(id, collapse=","))
name <- as.character(name$total)
name <- unique(name)
temp1 <- as.character(paste(name,collapse=","))
name <- as.data.frame(name)

## DB Upload
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
cat(paste0("SELECT * FROM news_raw WHERE id in(",temp1,")"))
data1 <- dbGetQuery(con,paste0("SELECT * FROM news_raw WHERE id in(",temp1,")"))
dbDisconnect(con)


for(i in 1:length(name[,1])){
  t <- name[i,1]
  t <- strsplit(as.character(t),",")[[1]]
  t <- unique(t)
  for(k in 1:length(t)){
    a <- cbind(t[k],data1[data1$id == t[k],"type"],nchar(as.character(data1[data1$id == t[k],"keyword"])),i) 
    if(k == 1) t1 <- a else t1 <- rbind(t1,a)
   }
  if (i == 1)temp <- t1 else temp <- rbind(temp, t1)
}

temp <- as.data.frame(temp, row.names=F)
names(temp) <- c("id","type","keyword","cat")
temp$id <- as.character(temp$id)
temp$type <- as.character(temp$type)
temp$keyword <- as.character(temp$keyword)
temp$cat <- as.character(temp$cat)
temp <- unique(temp)
# rm(list=setdiff(ls(), c("temp","name")))

#중복값 제거후 남은 id
length(table(temp$id))

#all이 있는 cat 만 캐치한 후 keyword갯수가 높은아이 -> 갯수가똑같으면 아이디가 낮은애로 남겨줌
table_1 <- temp[temp$type == 'all',] 
table_1 <- table_1 %>% group_by(cat) %>% filter(keyword == max(keyword))  %>% filter(id == min(id))

table_2 <- temp[!(temp$cat %in% table_1$cat),]
table_2 <- table_2 %>% group_by(cat,type)  %>% filter(keyword == max(keyword)) %>% filter(id == min(id))
# table_2 <- table_2[order(as.numeric(table_2$cat)),]

tables <- rbind(table_1,table_2)
tables$cat <- as.character(tables$cat)
tables <- as.data.frame(tables %>% group_by(id) %>% filter(cat == min(cat)))
tables <- as.data.frame(tables[order(as.numeric(tables$cat)),])

delete_id <- unique(temp[!(temp$id %in% tables$id),'id'])
delete_id <- paste(delete_id, sep = "", collapse = ",")

## table list to be deleted
targ_tb <- c("news_ext_info","news_like","news_recom","news_related",
             "news_summary" ,"news_term_freq","news_raw","all_news_raw")

con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
id <- paste(data$id,collapse=",")
cat(paste0("update news_raw set complete = 2 where id in (",id,")"))
dbSendQuery(con,paste0("update news_raw set complete = 2 where id in (",id,")"))
dbDisconnect(con)

#### MAX dbConnection MySQL #### 
cons <- dbListConnections(MySQL())
for(con in cons) dbDisconnect(con)

lapply(seq_along(targ_tb), function(i) {
  if (nchar(delete_id) > 0) {
    con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
    strSql <- sprintf(paste("delete from %s where id IN (%s)"), targ_tb[i],delete_id)
    print(strSql)
    try (
      dbSendQuery(con, strSql)
    )
  }
})
# rm(list=ls())
# gc()
