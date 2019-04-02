rm(list=ls())
gc()
library(lsa) ; library(RMeCab) ; library(tm) ; library(doParallel); library(RMySQL);library(plyr)

to <- Sys.Date() ## 1분전
from <- Sys.Date()-2

registerDoParallel(cores = 6)

### exist data Download
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
query <- paste("select * from news_raw where date > '",from,"' and date < '",to,"'", sep="") ## 쿼리작성
dbGetQuery(con, "set names utf8")
data <- dbGetQuery(con, query)
dbDisconnect(con)

rm(list=setdiff(ls(),c("data")))

## 데이터 분석
nouns    <- list()
nounstmp <- list()
name     <- c() 
header   <- c()

# ext_info <-foreach(i = 1:length(data[,1]), .combine=rbind,.packages=c("ff","RMeCab"))%dopar% {
for(i in 1:length(data[,1])){
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
    
    name_row <- row.names(cortmp)[which(cortmp[,1] > 0.5)]
    
    if(length(name_row) > 0){name <- rbind(name, paste(paste0(name_row, collapse=","),colnames(cortmp), sep=","))}
    else{name_row <- NA
    name <- rbind(name_row, name)}
  }
}

name <- na.omit(name)
rm(list=setdiff(ls(), c("name")))

#### hw code
name <- as.data.frame(as.character(name))

for (i in 1:length(name[,1])){
  a <- strsplit(as.character(name[i,1]),",")[[1]][1]
  if(i == 1) key <- a else key <- rbind(key, a)
}
key <- as.character(key)
name$key <- key
name <- name[order(as.numeric(name$key)),]
names(name)[1] <- "id"
name <- ddply(name,.(key), transform, total=paste(id, collapse=","))
name <- as.character(name$total)
name <- unique(name)

for(i in 1:length(name)){
  t <- name[i]
  t <- strsplit(as.character(t),",")[[1]]
  t <- unique(t)
  for (j in 1:length(t)){
    t1 <- unique(c(t[j],t))
    if (j == 1) t2 <- t1 else t2 <- rbind(t2, t1)
  }
  if (length(t2[1,]) < 7) t2 <- cbind(t2,matrix(,ncol=(7-length(t2[1,])),nrow=length(t2[,1]))) else
    if (length(t2[1,]) > 7) t2 <- t2[,1:7]
  if (i == 1)temp <- t2 else temp <- rbind(temp, t2)
}
temp <- as.data.frame(temp, row.names=F)
data <- temp
names(data) <- c("id","rid1","rid2","rid3","rid4","rid5","rid6")


head(data)
data <- data[order(data$id),]
data$regdate <- as.character(Sys.time())
id <- data$id
data2 <- dbGetQuery(con,paste0("SELECT id,header,type FROM all_news_raw WHERE id =",id))

## DB Upload
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbWriteTable(con, "news_related", data, append=T, row.names=F)
dbDisconnect(con)
