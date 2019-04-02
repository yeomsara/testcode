rm(list=ls())
#install.packages("RMeCab", repos="http://rmecab.jp/R")
library(RMeCab) ## 은전한닢 형태소분석기
library(KoNLP) ## 한나눔 형태소 분석기
library(RMySQL)
library(stringi)
library(rvest)
library(XML)
library(RCurl)
library(LSAfun)
library(tm)
library(parallel)
library(doParallel)
library(ff)
library(plyr)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)

to <- Sys.Date()
from <- Sys.Date()-3

query <- paste0("SELECT L1.id,L1.date,L1.header,L1.text,L1.keyword,L2.yn FROM  news_raw AS L1 LEFT JOIN news_recom AS L2 ON L1.id=L2.id where L1.date >= '",from,"' and L1.date <= '",to,"'")

## data download
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8")
#data <- dbGetQuery(con, "select * from news_raw where id='1193'")
data <- dbGetQuery(con, query)
dbDisconnect(con)
data <- data[data$yn == 1,]
data$date <- as.Date(as.character(data$date))
#data <- data[data$date >= "2016-08-08",]
data <- na.omit(data)
tt <- data$text
tt <- gsub("[[:space:]]", " ", tt)
tt.vec <- strsplit(tt, split="\\.")
tt.vec <- unlist(tt.vec)
testObject <- function(object)
{
  exists(as.character(substitute(object)))
}

(numCores <- detectCores() - 5) # CPU를 모두 사용할 경우 R 이외의 작업할 때 컴퓨터가 버벅거리기도 해서 1개 뺌 
(cl <- makeCluster( numCores ))
registerDoParallel(cl)

mecab <- foreach(i=1:length(tt.vec), .packages=c("ff","RMeCab"), .combine=rbind) %dopar%{
  tryCatch({
    res <- RMeCabC(tt.vec[i])
    res <- unlist(res)
    df <- as.data.frame(cbind(names(res),as.character(res)))
    df$j <- 1:length(df[,1])
    
    rm(tt)
    for (j in 1:length(df[,1])){
      if (df[j,1] == "SN" && df[j+1,1] == "NNBC"){
        t1 <- paste(df[j,2],df[j+1,2], sep="")
        t1 <- cbind(as.character(df[j,1]),t1,j)
        if (testObject(tt) == F) tt <- t1 else tt <- rbind(tt, t1)   
      }
    }
    tt <- as.data.frame(tt)
    
    df1 <- df[df$V1 == "NNG"|df$V1 == "SH"|df$V1 == "SL"|df$V1 == "SH",]
    names(tt) <- c("pum","word","id")
    names(df1) <- c("pum","word","id")
    df2 <- rbind(df1, tt)
    df2 <- df2[order(as.numeric(df2$id)),]
    text <- paste(df2$word, collapse=" ")
    text
  }, error=function(e){cat(i," ERROR :",conditionMessage(e), "\n")})
}

stopImplicitCluster()

mecab <- as.list(strsplit(mecab, split=" "))
tran <- unique(mecab)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y) {
  nchar(y) > 1 && is.hangul(y)}
  ,x)} )
tran <- Filter(function(x){length(x) >= 2}, tran)
names(tran) <- paste("Tr", 1:length(tran), sep="")
wordtran <- as(tran, "transactions")
ares <- apriori(wordtran, parameter=list(minlen=2,supp=0.01, conf=0, target="rules"))

# remove  subsets 
#ares.sorted <- sort(ares, by="lift")
#subset.matrix <- is.subset(ares.sorted, ares.sorted)
#subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#redundant <- colSums(subset.matrix, na.rm=T) >= 1
#ares.pruned <- ares.sorted[!redundant]
#inspect(ares.pruned)
par(mar=c(0,0,0,0))
png("R/png/rules.png", width=700,height=600,bg="transparent")
plot(head(sort(ares, by="lift"), 30), method="graph", control=list(cex=.7, type="items"), main="")
dev.off()

ftpUpload(what = "R/png/rules.png",
          to = "ftp://192.168.0.21//home/ftp/var/etc/rules.png",
          verbose = TRUE,
          userpwd = "ztc-server:ztcserver@@", prequote="CWD /home/ftp/var/etc")


#b <- plot(head(sort(ares.pruned, by="lift"), 30), method="graph", control=list(cex=.7, type="items"))

#ig_df <- get.data.frame( b, what = "both" )
#graph <- visNetwork(
#  nodes = data.frame(
#    id = ig_df$vertices$name
#    ,value = ig_df$vertices$support # could change to lift or confidence
#    ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
#    ,ig_df$vertices
#  )
#  , edges = ig_df$edges
#) %>%
#visEdges( arrows="to" ) %>%
#  visOptions( highlightNearest = T,nodesIdSelection = TRUE ) %>% 
#  visInteraction(navigationButtons = TRUE)

#visSave(graph, "rule.html", selfcontained = TRUE)
#visSave(graph, "rule2.html", selfcontained = F)

## ftp upload
#ftpUpload(what = "rule.html",
#to = "ftp://192.168.0.21//home/ftp/var/etc/rule.html",
#verbose = TRUE,
#         userpwd = "ztcit:ztc20110104", prequote="CWD /home/ftp/var/etc")
#
#
### ftp upload
#ftpUpload(what = "rule2.html",
#to = "ftp://192.168.0.21//home/ftp/var/etc/rule2.html",
#         verbose = TRUE,
#         userpwd = "ztcit:ztc20110104", prequote="CWD /home/ftp/var/etc")
#