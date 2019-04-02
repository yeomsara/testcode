rm(list=ls())
gc()
options(warn=-1)
Sys.getlocale()
library(RCurl)
library(XML)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)
library(dplyr)

(numCores <- detectCores() - 4) 
registerDoParallel(cores=numCores)

require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
dbGetQuery(con,"set names utf8")
query <- paste0("SELECT A.* 
                from kiscon_raw A
                LEFT OUTER JOIN kiscon_recom B on A.id = B.id 
                where B.id is null")
query2 <- paste0("SELECT * FROM exception_keyword WHERE cat = 'public'")
data <- dbGetQuery(con,query)
exp <- dbGetQuery(con,query2)
dbDisconnect(con)


##### 분류별 포함조건 제외조건 ########
gong_meta   <- unlist(str_split(exp[exp$major == '공사' & exp$minor == '포함','exp_keyword'],pattern=","))
gong_exp    <- unlist(str_split(exp[exp$major == '공사' & exp$minor == '제외','exp_keyword'],pattern=","))
mul_meta    <- unlist(str_split(exp[exp$major == '물품' & exp$minor == '포함','exp_keyword'],pattern=","))
mingan_meta <- unlist(str_split(exp[exp$major == '민간' & exp$minor == '포함','exp_keyword'],pattern=","))
lease_meta  <- unlist(str_split(exp[exp$major == '리스' & exp$minor == '포함','exp_keyword'],pattern=","))
etc_meta    <- unlist(str_split(exp[exp$major == '기타' & exp$minor == '포함','exp_keyword'],pattern=","))
etc_exp     <- unlist(str_split(exp[exp$major == '기타' & exp$minor == '제외','exp_keyword'],pattern=","))

##### 분류별 데이터 정제 #######

g_data  <- data[data$work == '공사' ,]
m_data  <- data[data$work == '물품' ,]
mg_data <- data[data$work == '민간' ,]
l_data  <- data[data$work == '리스' ,]
e_data  <- data[data$work == '기타' ,]


etc_data <- data[!(data$work %in% c('공사','물품','민간','리스','기타')),]
######## 포함 조건만 ######### 
matching <- function(data, match, i) {  
  mat <- paste(match[str_detect(data$header[i],match) == TRUE], collapse=",")
  return(cbind(data[i,],mat))
}

############### 포함,제외 조건 ############
matching2 <- function(data, match, match2, i) {  
  mat <- paste(match[str_detect(data$header[i],match) == TRUE], collapse=",")
  exp <- paste(match2[str_detect(data$header[i],match2) == TRUE], collapse=",")
  return(cbind(data[i,],mat,exp))
}

if (length(g_data[,1]) >= 1)  {
  g_data <- foreach(i=1:length(g_data[,1]), .combine = rbind) %dopar% matching2(g_data, gong_meta, gong_exp, i) 
}
if (length(m_data[,1]) >= 1)  {
  m_data <- foreach(i=1:length(m_data[,1]), .combine = rbind) %dopar% matching(m_data, mul_meta, i)
}
if (length(mg_data[,1]) >= 1)  {
  mg_data <- foreach(i=1:length(mg_data[,1]), .combine = rbind) %dopar% matching(mg_data, mingan_meta, i)
}
if (length(l_data[,1]) >= 1)  {
  l_data <- foreach(i=1:length(l_data[,1]), .combine = rbind) %dopar% matching(l_data, lease_meta, i)
}
if (length(e_data[,1]) >= 1)  {
  e_data <- foreach(i=1:length(e_data[,1]), .combine = rbind) %dopar% matching2(e_data, etc_meta, etc_exp, i)
}



##포함조건만 있는 데이터 
t1   <- rbind(m_data,mg_data,l_data)
t1_1 <- t1[t1$mat != '',]
t1_2 <- t1[!(t1$id %in% t1_1$id) ,]
##포함조건후 제외조건 
t2   <- rbind(g_data,e_data)
t2_1 <- t2[t2$mat !=''&t2$exp =='',]
t2_2 <- t2[!(t2$id %in% t2_1$id) ,]

############## 인공지능 추천 데이터 ################
tables1 <- try(rbind(t1_1[,!(names(t1_1) %in% c('mat'))],t2_1[,!(names(t2_1) %in% c('exp','mat'))]))
############## 인공지능 비추천 데이터 ##############
tables2 <- try(rbind(t1_2[,!(names(t1_2) %in% c("mat"))],t2_2[,!(names(t2_2) %in% c('exp','mat'))],etc_data))

yn_1 <- 1
yn_0 <- 0

insertdb <- function(tables,yn){
  require(RMySQL)
  con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
  dbGetQuery(con,"set names utf8")
  strSQL <- paste(
    'INSERT INTO kiscon_recom (id, yn, percent1) VALUES',
    paste(sprintf("('%s', %i, %i)", tables$id, yn, 99), collapse=','),sep ="")
  try(
    dbSendQuery(con, strSQL)
  )
  dbDisconnect(con)
}

insertdb(tables1,yn_1)
insertdb(tables2,yn_0)
print('완료')

# rm(list=ls())
# gc()