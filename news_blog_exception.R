rm(list=ls())
gc()
library(RCurl)
library(XML)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)
library(dplyr)
(numCores <- detectCores() - 3) 
registerDoParallel(cores=numCores)

curdate <- as.character(Sys.Date())

require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
query <- paste0("SELECT cat,major,exp_keyword FROM exception_keyword WHERE cat in('news','blog')")
args = commandArgs(trailingOnly=TRUE)

if(length(args) > 0) {
  query2 <- paste0("SELECT * FROM all_blog_raw WHERE date = '",args[1],"' and yn = 1 ")
  query3 <- paste0("SELECT * FROM all_news_raw WHERE date = '",args[1],"' and yn = 1 ")
} else {
  query2 <- paste0("SELECT * FROM all_blog_raw WHERE date = '",curdate,"' and yn = 1 ")
  query3 <- paste0("SELECT * FROM all_news_raw WHERE date = '",curdate,"' and yn = 1 ")
}
# query2 <- paste0("SELECT * FROM all_blog_raw WHERE date between '2017-06-01' and '2017-06-08' and yn = 1 ")
# query3 <- paste0("SELECT * FROM all_news_raw WHERE date between '2017-06-01' and '2017-06-08' and yn = 1 ")
b.data <- dbGetQuery(con,query2)
n.data <- dbGetQuery(con,query3)
exp <- dbGetQuery(con,query)
b.exp <- exp[exp$cat == 'blog' , ]
n.exp <- exp[exp$cat == 'news' , ]
dbDisconnect(con)
names(b.exp)
b.exp <- split(b.exp,b.exp$major)
n.exp <- split(n.exp,n.exp$major)

blog_matching <- function(b.data, b.exp, j) { 
  for(i in 1:length(b.exp)){
    if(str_detect(b.data$keyword[j],names(b.exp)[i])== TRUE){
      exp <- paste(b.exp[[i]][str_detect(b.data$text[j],b.exp[[i]][,'exp_keyword'])==TRUE,'exp_keyword'], collapse=",")
      return(cbind(b.data[j,],exp)) 
    }
  }
}

system.time(
  blog_raw <- foreach(j=1:length(b.data$text), .combine = rbind) %dopar% blog_matching(b.data, b.exp,j)
)

news_matching <- function(n.data, n.exp, j) { 
  for(i in 1:length(n.exp)){
    if(str_detect(n.data$keyword[j],names(n.exp)[i])== TRUE){
      exp <- paste(n.exp[[i]][str_detect(n.data$text[j],n.exp[[i]][,'exp_keyword'])==TRUE,'exp_keyword'], collapse=",")
      return(cbind(n.data[j,],exp))
    }
  } 
}

system.time(
  news_raw <- foreach(j=1:length(n.data$text), .combine = rbind) %dopar% news_matching(n.data, n.exp, j)
)

blog_raw$exp <- as.character(blog_raw$exp)
news_raw$exp <- as.character(news_raw$exp)

n.data <- merge(n.data, news_raw, all=T)
b.data <- merge(b.data, blog_raw, all=T)

n.data$exp[is.na(n.data$exp)] <- ''
b.data$exp[is.na(b.data$exp)] <- ''

blog_matching2 <- function(b.data, b.exp, j) {
  if(b.data$exp[j] == ''){
    all_exp <- paste(b.exp[[1]][str_detect(b.data$text[j],b.exp[[1]][,'exp_keyword'])==TRUE,'exp_keyword'], collapse=",")
  }else{
    all_exp <- ''
  }
  return(cbind(b.data[j,],all_exp))
}

system.time(
  blog_raw <- foreach(j=1:length(b.data$text), .combine = rbind) %dopar% blog_matching2(b.data, b.exp,j)
)

news_matching2 <- function(n.data, n.exp, j) {  
  if(n.data$exp[j] == ''){
    all_exp <- paste(n.exp[[2]][str_detect(n.data$text[j],n.exp[[2]][,'exp_keyword'])==TRUE,'exp_keyword'], collapse=",")
  }else{
    all_exp <- ''
  }
  return(cbind(n.data[j,],all_exp))
}

system.time(
  news_raw <- foreach(j=1:length(n.data$text), .combine = rbind) %dopar% news_matching2(n.data, n.exp, j)
)

blog_raw$all_exp <- as.character(blog_raw$all_exp)
news_raw$all_exp <- as.character(news_raw$all_exp)

# tables <- rbind(blog_raw,news_raw)
# tables <- tables[tables$exp !='',]
# write.csv(tables,file='/home/ztc-server/n.data.csv')
b.exception.id <- paste(blog_raw[blog_raw$exp != '' | blog_raw$all_exp != '','id'],collapse=",") 
n.exception.id <- paste(news_raw[news_raw$exp != '' | news_raw$all_exp != '','id'],collapse=",")

# write.csv(news_raw,'/home/ztc-server/news_expceion_data.csv')

cons <- dbListConnections(MySQL())
for(con in cons) dbDisconnect(con)


require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
dbSendQuery(con,paste0("UPDATE all_blog_raw SET yn = 0 WHERE id in(",b.exception.id,") "))
dbSendQuery(con,paste0("UPDATE blog_recom SET yn = 0 WHERE id in(",b.exception.id,") "))
dbSendQuery(con,paste0("UPDATE all_news_raw SET yn = 0 WHERE id in(",n.exception.id,") "))
dbSendQuery(con,paste0("UPDATE news_recom SET yn = 0 WHERE id in(",n.exception.id,") "))
dbDisconnect(con)




