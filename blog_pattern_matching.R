rm(list=ls(all=T))
gc()
localeToCharset()
Sys.getlocale()
library(XML)
library(RCurl)
library(RMySQL)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)

args = commandArgs(trailingOnly=TRUE)
if(length(args) > 0) {
  curdate = args[1]
} else {
  curdate <- as.character(Sys.Date())
}
testObject <- function(object){exists(as.character(substitute(object)))}
(numCores <- detectCores() - 1)
registerDoParallel(cores=numCores)

query3 <- paste0("SELECT id
                 FROM all_blog_raw
                 WHERE date =  '",curdate,"'
                 ORDER BY date desc")
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbSendQuery(con,"set names utf8")
last_id <- dbGetQuery(con, query3)

query <- paste0("SELECT A.*,B.yn,B.percent1
                FROM blog_raw A
                INNER JOIN blog_recom B on B.id = A.id
                WHERE A.complete = 2 and A.date = '",curdate,"'
                AND B.yn = 1
                ORDER BY A.date desc")

query2 <- paste0("SELECT name,type FROM company_default")
data <- dbGetQuery(con, query)
data <- data[!(data$id %in% last_id$id),]
company <- dbGetQuery(con, query2)
dbDisconnect(con)

select_blog <- function(company, data, i) {
  name <- paste(company[str_detect(data$text[i],company$name) == TRUE,1], collapse=", ")
  return(cbind(data[i,],name))
}

system.time(
  xx <- foreach(i=1:length(data$text), .combine = rbind) %dopar% select_blog(company, data, i)
)

tables <- xx
tables <-tables[,!(names(tables) %in% 'complete')]   
colnames(tables)[colnames(tables)=="regdate"] <- "raw_regdate"
colnames(tables)[colnames(tables)=="percent1"] <- "percent"
names(tables)

## DB Upload
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
dbWriteTable(con, "all_blog_raw", tables, append=T, row.names=F)

