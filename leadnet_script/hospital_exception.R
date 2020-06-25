rm(list=ls())
gc()
library(RCurl)
library(XML)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)
getwd()
(numCores <- detectCores() - 1) 
registerDoParallel(cores=numCores)

exception <- function(){
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
  dbGetQuery(con,"set names utf8")
  query <- paste0("SELECT exp_keyword FROM exception_keyword WHERE cat = 'hospital' ")
  query2 <- paste0("SELECT * FROM hospital_raw_test")
  h.data <- dbGetQuery(con,query2)
  exp <- dbGetQuery(con,query)
  exp <- exp[exp != ""]
  dbDisconnect(con)
  
  exp_matching <- function(h.data, exp, i) {  
    for(j in 1:length(exp)){
      if(stringr::str_detect(h.data$header[i],exp[j]) == TRUE){
        return(cbind(h.data[i,],exp[j]))
      }
    }
  }
  
  system.time(
    tt <- foreach(i=1:length(h.data$header), .combine = rbind) %dopar% exp_matching(h.data, exp, i)
  )
  
  colnames(tt)[colnames(tt)=="exp[j]"] <- "exception"
  exp_id <- unique(tt$id)
  
  h.data2 <- merge(x = h.data, y = tt[ , c("exception", "id")], by = "id", all.x=TRUE)
  h.data3 <- as.data.frame(h.data2[is.na(h.data2$exception),])
  data <- h.data3[,names(h.data3) %in% c("date","url","source","header")]
  
  
  con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
  dbGetQuery(con,"set names utf8")
  dbGetQuery(con,"delete from hospital_raw")
  dbWriteTable(con, "hospital_raw", data, append=T, row.names=F)
  dbDisconnect(con)
  
}

#try(exception())