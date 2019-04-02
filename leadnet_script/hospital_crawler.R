rm(list=ls(all=T))
gc()
library(XML)
library(RCurl)
library(rvest)
library(RMySQL)
library(stringr)


hosp_mysqldb <- function(r_tables){
  require(RMySQL)
  require(RH2)
  r_tables <- r_tables[complete.cases(r_tables), ]
  con <- dbConnect(MySQL(), user="root", password="20110104", dbname="leadnet", host="192.168.0.21")
  #dbWriteTable(con, "hospital_raw", r_tables, append=T, row.names=F)
  strSQL <- paste(
    'INSERT INTO hospital_raw_test (date, url, source, header) VALUES',
    paste(sprintf("('%s', '%s','%s','%s')", r_tables$date, r_tables$url, r_tables$source, r_tables$header), collapse=', '),
    'ON DUPLICATE KEY UPDATE url = values(url)',sep = ' '
  )
  print(strSQL)
  tryCatch({
    dbSendQuery(con, strSQL)
  }, error=function(e){cat(" ERROR :",conditionMessage(e), "\n")})
  dbDisconnect(con)  
}

############## 원주세브란스기독병원 ################
H.1 <- function(){
  thisMon <- substr(as.character(Sys.Date()),1,7)
  url <- paste0("http://www.wch.or.kr/hos_info/tender/board.asp?id=tender&cat_no=40&page=1")
  html <- read_html(url,encoding="euc-kr")
  html1 <- html %>% html_nodes(xpath='//a[@class="aTitle"]') %>% html_attr("href")
  html1 <- paste0('http://www.wch.or.kr/hos_info/tender/',html1)
  
  require(XML)
  r_tables <- readHTMLTable(url,encoding="euc-kr",trim=T,stringsAsFactors = FALSE)
  r_tables <- r_tables[42]
  r_tables <- as.data.frame(r_tables)
  head(r_tables)
  names(r_tables) <- c("id","header","none","date","dd","aa","bb")
  
  for (i in 1:length(r_tables[1,])){
    for (j in 1:length(r_tables[,1])){
      if (is.na(r_tables[j,i])) r_tables[j,i] <- ""
    }
  }
  
  drops <- c("id","none","dd","aa","bb")
  r_tables <-r_tables[,!(names(r_tables) %in% drops)]
  r_tables <- r_tables[nchar(r_tables$date) > 0,]
  r_tables$url <- html1[1:length(r_tables[,1])]
  r_tables$source <- '원주세브란스기독병원'  
  r_tables <- r_tables[substr(r_tables$date,1,7) == thisMon ,]
  r_tables = r_tables[complete.cases(r_tables), ]
  hosp_mysqldb(r_tables)
}
getwd()
################# 강원대학교병원(kangwon_hospital) ##################
H.2 <- function(){
  tryCatch({
  thisMon <- substr(Sys.Date(),1,7)
  url <- paste0("https://www.knuh.or.kr/hospitalinfo/hospitalinfo_03_04_list.asp")
  html <- read_html(url,encoding="utf-8")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
  r_tables <- html1[[1]]
  names(r_tables) <- c('id','header','aa','bb','date','cc')
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,1,12) == 'hospitalinfo']
  a <- a[substr(a,48,50) == 'idx']
  url.r <-  paste0('https://www.knuh.or.kr/hospitalinfo/',a)
  r_tables$date <- gsub('/','-',r_tables$date)
  r_tables$url <- url.r
  drops <- c("id","none","cc","aa","bb")
  r_tables <-r_tables[,!(names(r_tables) %in% drops)]                                                         
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,]
  r_tables$source <- '강원대학교병원'
  
  for(i in 1:length(r_tables$url)){
    name1 <- read_html(r_tables$url[i])
    name2 <- name1 %>% html_nodes(xpath='//h4') %>% html_text()
    if(i == 1) name <- name2  else name <- rbind(name, name2)   
  }

  r_tables <- r_tables[,c("date", "url", "source", "header")]
  r_tables = r_tables[complete.cases(r_tables), ]
  #return(r_tables)
  hosp_mysqldb(r_tables)
  }, error=function(e){cat(" ERROR :",conditionMessage(e), "\n")})
}

################# 근로복지공단태백병원(taeback_hospital)##################
H.3 <- function(){
  tryCatch({
  thisMon <- substr(Sys.Date(),1,7)
  url <- paste0("https://www.kcomwel.or.kr/taebaek/info/bidd.jsp")
  html <- read_html(url,encoding="utf-8")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
  r_tables <- html1[[1]] 
  names(r_tables) <- c('id','header','aa','bb','date','cc')
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,1,10) == '?mode=view']
  url.r <-  paste0('https://www.kcomwel.or.kr/taebaek/info/bidd.jsp',a)
  drops <- c("id","none","cc","aa","bb")
  r_tables <-r_tables[,!(names(r_tables) %in% drops)]  
  r_tables$url <- url.r
  r_tables$source <-'근로복지공단태백병원'
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,]
  r_tables <- r_tables[,c("date", "url", "source", "header")]
  r_tables <- r_tables[complete.cases(r_tables), ]
  #return(r_tables)
  hosp_mysqldb(r_tables)
  }, error=function(e){cat(" ERROR :",conditionMessage(e), "\n")})
}


################# 강원도원주의료원(wonju_hospital)##################
H.4 <- function(){
  thisMon <- substr(Sys.Date()-1,1,7)
  url <- paste0("http://www.kwmc.or.kr/community/bid.html")
  html <- read_html(url,encoding="euc-kr")
  #rows <- html_nodes(html, "tr")
  #rows <- lapply(rows,"html_nodes",xpath = ".//td|.//th")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
  
  r_tables <- html1[[20]]
  
  for (i in 1:length(r_tables[1,])){
    for (j in 1:length(r_tables[,1])){
      if (is.na(r_tables[j,i])) r_tables[j,i] <- ""
    }
  }
  
  r_tables <-r_tables[nchar(r_tables[,1]) != 0 ,]  
  names(r_tables) <- c('X1','X2','header','X4','date','X6','X7','X8','X9','X10','X11')
  in.r_tables <- c("date","header")
  r_tables <-r_tables[,(names(r_tables) %in% in.r_tables)]  
  
  r_tables$date <- as.character(as.Date(as.character(r_tables$date),"%y/%m/%d"))
  
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,nchar(a)-7,nchar(a)-4) == 'num=']
  a <- a[2:length(a)]
  url.r <-  paste0('http://www.kwmc.or.kr/',a)
  r_tables$url <- url.r
  r_tables$source <-'강원도원주의료원'
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,]
  r_tables <- r_tables[,c("date", "url", "source", "header")]
  r_tables <- r_tables[complete.cases(r_tables), ]
  #return(r_tables)
  hosp_mysqldb(r_tables)
}

################# 강원도속초의료원(sokcho_hospital)##################
H.5 <- function(){
  thisMon <- substr(Sys.Date()-1,1,7)
  url <- paste0("http://www.sokchomc.co.kr/bbs/board.php?bo_table=inews")
  html <- read_html(url,encoding="utf-8")
  #rows <- html_nodes(html, "tr")
  #rows <- lapply(rows,"html_nodes",xpath = ".//td|.//th")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
  
  r_tables <- html1[[1]]
  
  names(r_tables) <- c('id','header','X4','date','X6')
  in.r_tables <- c("date","header")
  r_tables <-r_tables[,(names(r_tables) %in% in.r_tables)]  
  
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,nchar(a)-8,nchar(a)-4) == 'wr_id']
  a <- a[2:length(a)]
  r_tables$url <- a
  r_tables$source <-'강원도속초의료원'
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,]
  r_tables <- r_tables[complete.cases(r_tables), ]
  return(r_tables)
  hosp_mysqldb(r_tables)
}


################# 근로복지공단동해산재병원 ##################
H.6 <- function(){
  thisMon <- substr(Sys.Date()-1,1,7)
  url <- paste0("https://www.kcomwel.or.kr/donghae/info/bidd.jsp")
  html <- read_html(url,encoding="utf-8")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
  r_tables <- html1[[1]]
  names(r_tables) <- c('id','header','X4',"X7",'date','X6')
  in.r_tables <- c("date","header")
  r_tables <-r_tables[,(names(r_tables) %in% in.r_tables)]  
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,nchar(a)-8,nchar(a)-4) == 'rd_no']
  url.r <-  paste0('https://www.kcomwel.or.kr/donghae/info/bidd.jsp',a)
  r_tables$url <- url.r
  r_tables$source <-'근로복지공단동해산재병원'
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,]
  r_tables <- r_tables[,c("date", "url", "source", "header")]
  #return(r_tables)
  r_tables <- r_tables[complete.cases(r_tables), ]
  hosp_mysqldb(r_tables)
}


################# 국립춘천병원 ##################
H.7 <- function(){
  thisMon <- substr(Sys.Date()-1,1,7)
  url <- paste0("http://www.cnmh.go.kr/cnmh/board/cnmhNewsList.jsp?menu_cd=M_06_03&ctx=tender")
  html <- read_html(url,encoding="euc-kr")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
  r_tables <- html1[[1]]
  names(r_tables) <- c('id','header','X4','date',"X7",'X6')
  in.r_tables <- c("date","header")
  r_tables <-r_tables[,(names(r_tables) %in% in.r_tables)]  
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,nchar(a)-8,nchar(a)) == '&sub_loc=']
  url.r <-  paste0('http://www.cnmh.go.kr/cnmh/board',substr(a,2,nchar(a)))
  r_tables$url <- url.r
  r_tables$source <-'국립춘천병원'
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,]
  r_tables <- r_tables[,c("date", "url", "source", "header")]
  #return(r_tables)
  r_tables <- r_tables[complete.cases(r_tables), ]
  hosp_mysqldb(r_tables)
}

################# 강원도재활병원##################
H.8 <- function(){
  thisMon <- substr(Sys.Date(),1,7)
  url <- paste0("https://www.grh.or.kr/notice/notice_03.asp?board_id=BBCC03")
  html <- read_html(url,encoding="utf-8")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
  r_tables <- html1[[1]]
  names(r_tables) <- c('id','header','X4',"X7",'date','X6')
  in.r_tables <- c("date","header")
  r_tables <-r_tables[,(names(r_tables) %in% in.r_tables)]  
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,nchar(a)-14,nchar(a)-11) == '&seq']
  url.r <-  paste0('https://www.grh.or.kr',a)
  r_tables$url <- url.r
  r_tables$source <-'강원도재활병원'
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,] 
  r_tables <- r_tables[,c("date", "url", "source", "header")]
  #return(r_tables)
  r_tables <- r_tables[complete.cases(r_tables), ]
  hosp_mysqldb(r_tables)
}

################# 한양대학교의료원 ##################
H.9 <- function() {
  page <- 1
  thisMon <- substr(Sys.Date()-1,1,7)
  ## 
  url  <- "http://www.hyumc.com/bbs/list.asp?page=%s&idx=33&cat_no=07070000&bd_code=biding_bbs&Search_Field=&Search_Word=&dptCode=&dptflg=&DptMenuCode="
  url2 <- "http://www.hyumc.com/bbs/view.asp?idx=33&cat_no=07070000&bd_code=biding_bbs&Search_Field=&Search_Word=&Search_Cate=&page=1&bbs_idx=%s&dptCode=&dptflg=&DptMenuCode="
  url <- sprintf(url, page)
  r_tables <- c()
  lapply(seq_along(url), function(i) {
    url <- enc2utf8(url)
    url <- GET(url, timeout(200))
    html <- read_html(as.character(url), encoding="UTF-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    
    get_p_num <- html %>%  html_nodes("a") %>% html_attr("onclick")
    get_p_num <- get_p_num[!is.na(get_p_num)]
    
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("[[:digit:]]+", get_p_num)))
    url.r <- sprintf(url2,bbs_idx)
    
    r_tables <- html1[[1]]
    names(r_tables) <- c('id','header','X1','date')
    r_tables$X1 <- NULL
    
    r_tables$url <- url.r
    r_tables$source <-'한양대학교의료원'
    
    r_tables <- r_tables[,c("id", "date", "url", "source", "header")]
    r_tables$date <- gsub('\\.', '-', r_tables$date)
    r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) >= thisMon ,]  
    
    r_tables <<- r_tables
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################# 분당서울대학교병원 ##################
H.10 <- function() {
  page <- 1:5
  
  thisMon <- substr(Sys.Date()-1,1,7)
  url  <- "https://www.snubh.org/intro/news/bid/list.do?cPage=%s"
  url2 <- "https://www.snubh.org/intro/news/bid/"

  url <- sprintf(url, page)
  r_tables <- c()

  lapply(seq_along(url), function(i) {
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    names(h_table) <- c('id','header','date','X1')
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    h_table$url <- sprintf(paste0(str_replace(paste0(url2, unique(a[grep("view.do", a)])),"cPage=1",""),"cPage=%s"),i)
    h_table$source <-'분당서울대병원'
    h_table <- h_table[,c("id", "date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7),"\\.","-") >= thisMon ,]
    h_table$date <- gsub('\\.', '-', h_table$date)
    r_tables <<- rbind(r_tables, h_table)
  })

  aa <- r_tables %>% filter(id == '공지')
  aa1<- data.frame(do.call('rbind', strsplit(as.character(aa$url),'&',fixed=TRUE)))
  aa1$X2  <- substr(aa1$X2, nchar(as.character(aa1$X2)), nchar(as.character(aa1$X2)))
  aa1 <- aa1 %>% group_by(X1) %>% filter(X2 == min(X2))  
  aa2<-c()
  aa2$url <- paste0(aa1$X1,'&cPage=',aa1$X2) 
  aa2 <- as.data.frame(aa2)
  
  bb <- r_tables %>% filter(id != '공지')
  r_tables <- rbind(r_tables[r_tables$url %in% as.character(aa2$url), ], bb)
  r_tables <- r_tables[,c("date", "url", "source", "header")]

  for (i in 1:length(r_tables$url)) {
    html2 <- read_html(r_tables$url[i], encoding="utf-8")
    r_tables$header[i] <- html2 %>% html_nodes(xpath='//h1[@class="tit"]/span[1]') %>% html_text()
    if (r_tables$header[i] == '공지') { r_tables$header[i] <- html2 %>% html_nodes(xpath='//h1[@class="tit"]/span[2]') %>% html_text()}
  }
  r_tables
  hosp_mysqldb(r_tables)
}

################# 아주대학교병원 ##################
H.11 <- function() {
  page <- 1:1
  
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "http://hosp.ajoumc.or.kr/HospitalInfo/BiddingRetrieve.aspx?cp=%s&sid=&rc=10&rp=1"  
  url2 <- "http://hosp.ajoumc.or.kr/HospitalInfo/"
  
  url <- sprintf(url, page)
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    #Sys.setlocale("LC_ALL", "English")
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    
    names(h_table) <- c('id','header','source','date')
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")        
    
    h_table$url <- sprintf(paste0(str_replace(paste0(url2, unique(a[grep("BiddingView.aspx", a)])),"cp=1&sid=",""),"cp=%s&sid="),i)
    
    h_table$source <-'아주대학교병원'
    
    h_table <- h_table[,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7),"\\.","-") >= thisMon ,]
    h_table$date <- gsub('\\.', '-', h_table$date)
    r_tables <<- rbind(r_tables, h_table)         
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################# 순천향대학교부천병원 ##################
H.12 <- function() {
  page <- 1:1
  
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "http://www.schmc.ac.kr/bucheon/kor/hospitalintro/hospitalNotice.do"  
  url2 <- "http://www.schmc.ac.kr/bucheon/kor/hospitalintro/"
  
  url <- sprintf(url, page)
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    
    h_table$X1 <- h_table$X4 <- NULL
    h_table$id <- 1
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    h_table <- h_table[h_table$X2 != "",]
    
    h_table$url <- paste0(url2, unique(a[grep("hospitalNoticeView.do", a)]))    
    h_table$source <-'순천향대학교부천병원'
    
    names(h_table) <- c("header", "date", "id", "url", "source")
    h_table <- h_table[,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7),"\\.","-") >= thisMon ,]
    
    r_tables <<- rbind(r_tables, h_table)
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)  
}


################ 국민건강보험공단일산병원 ##################
H.13 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "http://www.nhimc.or.kr/hospital/hospitalNews/bidding/board_list.do?BCode=3&Page=%s&meddp=&SeqNo=&SearchType=title&SearchText="  
  url2 <- "http://www.nhimc.or.kr/hospital/hospitalNews/bidding/board_view.do?BCode=3&Page=%s&meddp=&SeqNo=%s&SearchType=title&SearchText="
  
  url <- sprintf(url, page)
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    
    names(h_table) <- c("id","header","source", "date","ct")
    
    h_table$ct <- NULL
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("goView", a)]
    
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("[[:digit:]]+", get_p_num)))          
    h_table$url <- sprintf(url2,i,bbs_idx)
    h_table$source <-'국민건강보험공단일산병원'
    
    h_table <- h_table[,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7),"\\.","-") >= thisMon ,]
    
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 고려대학교의료원 ##################
H.14 <- function() {
  page <- 1:2
  
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "http://www.kumc.or.kr/introduction/biddingList.do?cPage=%s"  
  url2 <- "http://www.kumc.or.kr/introduction/biddingView.do?BID_NO=%s&cPage=%s"
  
  url <- sprintf(url, page)
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    i <-1
    html <- read_html(url[[i]],encoding="euc-kr")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
    h_table <- html1[[2]]
    
    names(h_table) <- c("id","X1","X2", "header","status","date","file")
    
    h_table$X1 <- h_table$X2 <- h_table$file <- NULL    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("biddingView.do", a)]     
    get_p_num <- unlist(regmatches(get_p_num,gregexpr("&cPage=[[:digit:]]+", get_p_num),  invert = TRUE))
    get_p_num <- get_p_num[get_p_num != ""]
    bbs_idx <- unlist(regmatches(get_p_num,gregexpr("[[:digit:]]+", get_p_num)))
    
    h_table$url <- sprintf(url2,bbs_idx,i)    
    h_table$source <-'고려대학교의료원'
    
    h_table <- h_table[,c("date", "url", "source", "header","status")]
    h_table$date <- gsub('\\.', '-', h_table$date)
    r_tables <<- rbind(r_tables, h_table)  
  })  
  r_tables <- r_tables[which(r_tables$status == "진행중"),]
  r_tables$status <- NULL  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}

################ 국립암센터병원 ##################
H.15 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "http://www.ncc.re.kr/board.ncc?uri=notice04&searchKey=total&searchValue=&pageNum=%s"
  url2 <- "http://www.ncc.re.kr/boardView.ncc?uri=notice04&ntcId=%s&searchKey=total&searchValue=&pageNum=%s"
  
  url <- sprintf(url, page)
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
    h_table <- html1[[1]]  
    
    names(h_table) <- c("header","date")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("onclick")
    
    get_p_num <- a[grep("fncView", a)]     
    get_p_num <- unlist(regmatches(get_p_num,gregexpr("[[:digit:]]+", get_p_num)))
    bbs_idx <- get_p_num[get_p_num != ""]
    
    h_table$url <- sprintf(url2,bbs_idx,i)    
    h_table$source <-'국립암센터병원'    
    h_table$id <- 1
    
    h_table <- h_table[,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7),"\\.","-") >= thisMon ,]
    
    h_table$date <- gsub('\\.', '-', h_table$date)
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 원광대학교의과대학산본병원 ##################
H.16 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "https://www.wmcsb.co.kr/buy/list.asp?mode=&sc_field=&sc_word=&page=%s"
  url2 <- "https://www.wmcsb.co.kr/buy/"
  
  url <- sprintf(url, page)
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
    h_table <- html1[[1]]  
    
    names(h_table) <- c("id","header","file","date")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("read.asp", a)]
    
    bbs_idx <- get_p_num[get_p_num != ""]
    
    h_table$url <- paste0(url2,bbs_idx)    
    h_table$source <-'원광대학교의과대학산본병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & substr(h_table$date,1,7) >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 경기도의료원의정부 ##################
H.17 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "http://www.medical.or.kr/_wj_gpmc/news/sub_04.html?page=%s&bd=tender&md=l"
  
  url <- sprintf(url, page)
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
    h_table <- html1[[1]]  
    
    names(h_table) <- c("id","header","writer","date", "ct")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("seq=", a)]
    
    url2 <- get_p_num[get_p_num != ""]
    
    h_table$url <- url2
    h_table$source <-'경기도의료원의정부'    
    
    h_table$header <- gsub("\t", "", h_table$header)    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & substr(h_table$date,1,7) >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 경기도의료원 ##################
H.18 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  kyoungi <- c("_wj_gpmc", "_pj_gpmc", "_as_gpmc", "_sw_gpmc", "_pc_gpmc", "_ic_gpmc")
  kyoungi_kor <- c("경기도의료원의정부","경기도의료원파주","경기도의료원안성","경기도의료원수원","경기도의료원포천","경기도의료원이천")
  
  r_tables <- c()
  
  lapply(seq_along(kyoungi), function(i) {
    
    url  <- "http://www.medical.or.kr/%s/news/sub_04.html?page=%s&bd=tender&md=l"    
    url <- sprintf(url, kyoungi[i], page)    
    
    lapply(seq_along(url), function(j) {
      
      html <- read_html(url[[j]],encoding="utf-8")
      html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
      h_table <- html1[[1]]  
      
      names(h_table) <- c("id","header","writer","date", "ct")
      
      a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
      
      get_p_num <- a[grep("seq=", a)]
      get_p_num <- get_p_num[grep("https://", get_p_num)]
      
      url2 <- get_p_num[get_p_num != ""]
      
      h_table$url <- url2
      h_table$source <- kyoungi_kor[i]  
      
      h_table$header <- gsub("\t", "", h_table$header)    
      
      h_table <- h_table[ ,c("date", "url", "source", "header")]
      h_table <- h_table[h_table$header != "" & substr(h_table$date,1,7) >= thisMon ,]
      r_tables <<- rbind(r_tables, h_table)  
    })    
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 양산부산대병원 ##################
H.19 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url  <- "http://www.pnuyh.or.kr/pnuh/board/list.do?rbsIdx=278&page=%s"  
  
  url2  <- "http://www.pnuyh.or.kr/pnuh/board/"  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
    h_table <- html1[[1]]  
    
    names(h_table) <- c("id","header","writer","date", "ct")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("view.do", a)]
    
    bbs_idx <- get_p_num[get_p_num != ""]
    
    h_table$url <- paste0(url2, bbs_idx)
    h_table$source <-'양산부산대병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & substr(h_table$date,1,7) >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 경상대학교병원 ##################
H.20 <- function() {
  page <- 1:4
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "https://www.gnuh.co.kr/gnuh/board/list.do?rbsIdx=108&page=%s"    
  url2  <- "https://www.gnuh.co.kr/gnuh/board/"  
  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
    h_table <- html1[[1]]  
    
    names(h_table) <- c("id","header","writer","date", "ct")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("view.do", a)]
    
    bbs_idx <- get_p_num[get_p_num != ""]
    
    h_table$url <- paste0(url2, bbs_idx)
    h_table$source <-'경상대학교병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]
    h_table <- h_table[h_table$header != "" & substr(h_table$date,1,7) >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 대우병원 ##################
H.21 <- function() {
  page <- 1:1
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://www.dwho.or.kr/bbs/board.php?bo_table=dwhobid&page=%s"    
  url <- sprintf(url, page)
  
  r_tables <- c()
  lapply(seq_along(url), function(i) {    
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[9]]  
    
    names(h_table) <- c("id","header","writer","date", "ct")
    
    h_table$date <- str_replace(h_table$date, "\\.", "-")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("http://www.dwho.or.kr/bbs/board.php\\?bo_table=dwhobid&wr_id=", a)]
    
    h_table$url <- get_p_num[get_p_num != ""]    
    
    h_table$source <-'대우병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 대우병원 ##################
H.22 <- function() {
  page <- 1:1
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://www.dwho.or.kr/bbs/board.php?bo_table=dwhobid&page=%s"    
  url <- sprintf(url, page)
  
  r_tables <- c()
  lapply(seq_along(url), function(i) {    
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[9]]  
    
    names(h_table) <- c("id","header","writer","date", "ct")
    
    h_table$date <- str_replace(h_table$date, "\\.", "-")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("http://www.dwho.or.kr/bbs/board.php\\?bo_table=dwhobid&wr_id=", a)]
    
    h_table$url <- get_p_num[get_p_num != ""]    
    
    h_table$source <-'대우병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 순천향대학교부속구미병원 ##################
H.23 <- function() {
  page <- 1:1
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://www.schmc.ac.kr/gumi/kor/hospitalintro/hospitalNotice.do?"    
  url2  <- "http://www.schmc.ac.kr/gumi/kor/hospitalintro/"
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {    
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]  
    
    names(h_table) <- c("id","header","date", "ct")
    
    h_table <- h_table[!is.na(h_table$ct),]      
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("hospitalNoticeView.do\\?IDX=", a)]      
    h_table$url <- paste0(url2, get_p_num[grep("/gumi/kor/hospitalintro/", get_p_num, invert=TRUE)])
    h_table$source <-'순천향대학교부속구미병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 경상북도김천의료원 ##################
H.24 <- function() {
  page <- 1:1
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://www.schmc.ac.kr/gumi/kor/hospitalintro/hospitalNotice.do?"    
  url2  <- "http://www.schmc.ac.kr/gumi/kor/hospitalintro/"
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {    
    
    html <- read_html(url[[i]],encoding="utf-8")
    html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]  
    
    names(h_table) <- c("id","header","date", "ct")
    
    h_table <- h_table[!is.na(h_table$ct),]      
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("hospitalNoticeView.do\\?IDX=", a)]      
    h_table$url <- paste0(url2, get_p_num[grep("/gumi/kor/hospitalintro/", get_p_num, invert=TRUE)])
    h_table$source <-'경상북도김천의료원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })  
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 영남대학교의대부속영천병원  ##################
H.25 <- function() {
  page <- 1:3
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://yumc.ac.kr:8888/yc/bbs/List.do?bbsId=news4&pageNum=%s"    
  url2  <- "http://yumc.ac.kr:8888/yc/bbs/view.do?bbsId=news4&pageNum=%s&wr_id=%s"
  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html    <- read_html(url[[i]],encoding="utf-8")
    html1   <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]  
    
    names(h_table) <- c("id","header", "writer", "date", "ct")    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("view.do\\?bbsId=news4&pageNum=", a)]
    
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("wr_id=[[:digit:]]+", get_p_num)))
    bbs_idx <- unlist(regmatches(bbs_idx, gregexpr("[[:digit:]]+", bbs_idx)))
    h_table$url <- sprintf(url2,i,bbs_idx)
    
    h_table$source <-'영남대학교의대부속영천병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################  을지대학병원  ##################
H.26 <- function() {
  page <- 1:1
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://www.emc.ac.kr/info/info_pg06_06.jsp?str_page=1&search=&find=&dept=&doct=&str_board_category=&dept_gubun=&board_code=&screenKey="    
  url2  <- "http://www.emc.ac.kr/info/info_pg06_07.jsp?board_code=BOARD_5&board_sequence=%s"
  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html    <- read_html(url[[i]],encoding="utf-8")
    html1   <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    
    names(h_table) <- c("id","header", "file", "date", "ct")    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("BOARD_5", a)]
    
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("board_sequence=[[:digit:]]+", get_p_num)))
    bbs_idx <- unlist(regmatches(bbs_idx, gregexpr("[[:digit:]]+", bbs_idx)))
    h_table$url <- sprintf(url2,bbs_idx)
    
    h_table$source <-'을지대학병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    h_table$date <- gsub('\\.', '-', h_table$date)
    r_tables <<- rbind(r_tables, h_table)  
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)
}



################  단국대학교의과대학부속병원  ##################
H.27 <- function() {
  page <- 1:1
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "https://www.dkuh.co.kr/board5/bbs/board.php?bo_table=01_03_04&page=%s"    
  url2  <- "https://www.dkuh.co.kr/board5/bbs/board.php?bo_table=01_03_04&wr_id=%s&page=%s"
  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html    <- read_html(url[[i]],encoding="utf-8")
    html1   <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    
    names(h_table) <- c("id","header", "writer", "date", "ct")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("wr_id", a)]
    
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("wr_id=[[:digit:]]+", get_p_num)))
    bbs_idx <- unlist(regmatches(bbs_idx, gregexpr("[[:digit:]]+", bbs_idx)))
    h_table$url <- sprintf(url2,bbs_idx,i)
    
    h_table$source <-'단국대학교의과대학부속병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 한일병원 ##################
H.28 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7)
  
  url   <- "https://www.hanilmed.net/portal/bbs/selectBoardList.do?bbsId=BBSMSTR_000000000012&menuNo=20107040&option1=&searchCnd=&searchWrd=&dept=&pageIndex=%s"
  url2  <- "https://www.hanilmed.net/portal/bbs/selectBoardArticle.do?bbsId=BBSMSTR_000000000012&nttId=%s&pageIndex=%s&searchCnd=&searchWrd=&menuNo=20107040"
  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {
    
    html    <- read_html(url[[i]],encoding="utf-8")
    html1   <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    
    names(h_table) <- c("id","header", "writer", "date", "ct")
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    
    get_p_num <- a[grep("selectBoardArticle.do", a)]
    
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("nttId=[[:digit:]]+", get_p_num)))
    bbs_idx <- unlist(regmatches(bbs_idx, gregexpr("[[:digit:]]+", bbs_idx)))
    h_table$url <- sprintf(url2,bbs_idx,i)
    
    h_table$source <-'한일병원'    
    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 한양대학교의대부속병원 ##################
H.29 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://www.hyumc.com/bbs/list.asp?page=%s&idx=33&cat_no=07070000&bd_code=biding_bbs&Search_Field=&Search_Word=&dptCode=&dptflg=&DptMenuCode="
  url2  <- "http://www.hyumc.com/bbs/view.asp?idx=33&cat_no=07070000&bd_code=biding_bbs&Search_Field=&Search_Word=&Search_Cate=&page=%s&bbs_idx=%s&dptCode=&dptflg=&DptMenuCode="
  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {      
    
    html    <- read_html(url[[i]],encoding="euc-kr")
    html1   <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    
    names(h_table) <- c("id","header", "ct", "date" )
    
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("onclick")
    
    get_p_num <- a[grep("goView", a)]
    
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("[[:digit:]]+", get_p_num)))
    bbs_idx <- unlist(regmatches(bbs_idx, gregexpr("[[:digit:]]+", bbs_idx)))
    h_table$url <- sprintf(url2, i, bbs_idx)
    h_table$source <-'한양대학교의대부속병원'    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 부산의료원 ##################
H.30 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  
  url   <- "http://www.busanmc.or.kr/busanmc/index.php?pCode=bidding&pg=%s"
  url2  <- "http://www.busanmc.or.kr/busanmc/index.php?pCode=bidding&pg=%s&mode=view&idx=%s"
  
  url <- sprintf(url, page)
  
  r_tables <- c()
  
  lapply(seq_along(url), function(i) {      
    
    html    <- read_html(url[[i]],encoding="utf-8")
    html1   <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
    h_table <- html1[[1]]
    names(h_table) <- c("id","header", "writer", "date" )
    a <- html %>% html_nodes(xpath='//table') %>% html_nodes(xpath='//a') %>% html_attr("href")
    get_p_num <- a[grep("idx=", a)]
    bbs_idx <- unlist(regmatches(get_p_num, gregexpr("idx=[[:digit:]]+", get_p_num)))
    bbs_idx <- unlist(regmatches(bbs_idx, gregexpr("[[:digit:]]+", bbs_idx)))
    h_table$url <- sprintf(url2, i, bbs_idx)
    h_table$source <-'부산의료원'    
    h_table <- h_table[ ,c("date", "url", "source", "header")]  
    h_table <- h_table[h_table$header != "" & str_replace(substr(h_table$date,1,7), "\\.","-") >= thisMon ,]
    r_tables <<- rbind(r_tables, h_table)  
  })
  #return(r_tables)
  hosp_mysqldb(r_tables)
}


################ 고신대학교복음병원 ##################1
##(sara 수정)
H.31 <- function() {
  page <- 1:2
  thisMon <- substr(Sys.Date()-1,1,7)
  # thisMon <- substr(seq(Sys.Date(), length = 2, by = "-1 months")[2],1,7) 
  url   <- "https://www.kosinmed.or.kr/05service/service10.htm?i_menu=&bid=7&page=%s&cat=2"
  url2  <- "https://www.kosinmed.or.kr/05service/service10.htm?i_menu=&bid=7&mode=view&uid=%s&no=28"
  url <- sprintf(url, page)
  
  lapply(seq_along(url), function(i) {      
    html    <- read_html(url[[i]],encoding="euc-kr")
    date <- html %>% html_nodes(xpath='//p[@class="date"]') %>% html_text()
    date <- data.frame(do.call('rbind', strsplit(as.character(date),':',fixed=TRUE)))
    date <- data.frame(do.call('rbind', strsplit(as.character(date$X3),'/',fixed=TRUE)))
    date <- gsub(" ","",date$X1)
    header   <- html %>% html_nodes(xpath='//p[@class="title"]') %>% html_text()
    a_href <- html %>% html_nodes(xpath='//a') %>% html_attr('href')
    a_href <- a_href[str_detect(a_href,'uid=') == TRUE]
    url <- paste0('https://www.kosinmed.or.kr',a_href)
    source <- '고신대학교복음병원'
    tables <- as.data.frame(cbind(header,date,url,source))
    tables <- tables[tables$header != "" & substr(tables$date,1,7) >= thisMon ,]
    hosp_mysqldb(tables)
  })
}


##################충청북도청주의료원##################3

H.32 <- function(){
  thisMon <-substr(Sys.Date()-1, 1, 7)
  url <- "https://www.cjmc.or.kr/list.php?bbs_id=bid_info"
  html<- read_html(url, encoding="euc-kr")
  html1 <- html %>%  html_nodes(xpath='//table') %>% html_table(fill=TRUE)
  
  tables <- html1[[2]]
  #unlist(html1)
  names(tables) <- c('id','header','x4','x5','x6','date')
  in.tables <-c("date","header")
  tables <-tables[,(names(tables) %in% in.tables)]
  
  
  a <- html %>% html_nodes(xpath= '//a') %>% html_attr("href")
  a <- a[str_detect(a,'view.php') == TRUE] #
  a <- a[1:length(a)]
  
  url.r <-paste0('https://www.cjmc.or.kr/',a)
  tables$url <- url.r
  tables$source <- '충청북도청주의료원'
  tables <- tables[tables$header != "" & substr(tables$date,1,7) == thisMon ,]
  hosp_mysqldb(tables)
}

##################국립공주병원##################3

H.33 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "http://www.knmh.go.kr/knmh/board/knmhNewsList.jsp?menu_cd=M_06_03_03&ctx=pds-3"
  html <- read_html(url, encoding="euc-kr")
  html1 <- html %>% html_nodes(xpath='//table') %>%  html_table(fill=TRUE)
  
  tables <- html1[[1]]
  names(tables) <- c('id', 'header', 'x3','date','x5','x6')
  in.tables <-c("date","header")
  tables <-tables[,(names(tables) %in% in.tables)]
  
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  
  ## 왜안될까 -> a <- a[str_detect(a, 'board.php') == TRUE]
  a <- a[substr(a,nchar(a)-8,nchar(a)-4) == 'wr_id']
  a <- a[1:length(a)]
  
  url.r <- paste0('http://www.knmh.go.kr/',a)
  tables$url <- url.r
  tables$source <-'국립공주병원'
  tables <- tables[tables$header != ""  & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}




##################전라북도남원의료원##################3
# 특이케이스
H.34 <- function(){
  thisMon <-substr(Sys.Date()-1, 1, 7)
  url <- "https://www.namwonmed.or.kr:19005/ds6_3_1.html?db=notice&c=list&page=&kind3=&SK=&SN=&idx="
  html <- read_html(url, encoding = "ecu-kr")
  html1 <- html %>% html_nodes(xpath = '//table') %>%  html_table(fill=TRUE)
  
  tables <- html1[[1]]
  names(tables) <- c('id','x2','header','date','x5','x6','x7')
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  a <- html %>%  html_nodes(xpath='//a') %>%  html_attr("href")
  a <- a[str_detect(a, '/ds6_3_1.html?') == TRUE]
  a <- a[substr(a,61,65) == '&idx=']
  
  # https://www.namwonmed.or.kr:19005/ds6_3_1.html?db=bidding&no=715&c=view&page=1&SK=&SN=&kind3=&idx=
  
  url.r <- paste0('https://www.namwonmed.or.kr:19005', a)
  tables$url <- a
  tables$source <- '전라북도남원의료원'
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}


##################인제대학교부속상계백병원##################3

H.35 <- function(){
  thisMon <-substr(Sys.Date()-1, 1, 7)
  url <- "http://www.paik.ac.kr/sanggye/abou/sub3_4.asp"
  html <- read_html(url, encoding = "UTF-8")
  html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table(fill=TRUE)
  
  tables <- html1[[1]]
  names(tables) <- c('id','header','date','x4')
  in.tables <- c("date", "header")
  tables <-tables[,(names(tables) %in% in.tables)]
  
  tables$date <- gsub("\\.","-",tables$date)
  
  a <- html %>% html_nodes(xpath='//a') %>%  html_attr("href")
  a <- a[str_detect(a, 'view.asp')  == TRUE]
  a <- a[1:length(a)]
  
  url.r <-paste0('http://www.paik.ac.kr/sanggye/abou/',a)
  tables$url <- url.r
  tables$source <- '인제대학교부속상계백병원'
  tables <-tables[tables$header != "" & str_replace(substr(tables$date,1,7), "\\.","-") >= thisMon ,]
  hosp_mysqldb(tables)
}


##################순천향대학교서울병원##################3

H.36 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "http://www.schmc.ac.kr/seoul/kor/hospitalintro/hospitalNotice.do"
  html <- read_html(url, encoding = "UTF-8")
  html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table(fill= TRUE)
  
  tables <- html1 [[1]]
  names(tables) <- c('id','header','date','x4')
  in.tables <- c("date", "header")
  tables <-tables[,(names(tables) %in% in.tables)]
  
  tables <- tables[-1, ]
  
  a <- html %>% html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_detect(a, 'hospitalNoticeView.do?') == TRUE]
  a <- a[1:length(a)]
  
  url.r <- paste0('http://www.schmc.ac.kr/seoul/kor/hospitalintro/', a)
  tables$url <- url.r
  tables$source <- '순천향대학교서울병원'
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
  
}

##################건국대학교병원##################3

H.37 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "http://www.kuh.ac.kr/bin/introduce/bid_list.asp?pBbsNo=48&pMode=introduce&pMenu=3&pTab=2"
  html <- read_html(url, encoding = "euc-kr")
  html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table(fill= TRUE)
  
  tables <- html1[[1]]
  names(tables) <- c('id','header','date','x4')
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  a <- html %>%  html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_detect(a, 'bid_view.asp') == TRUE]
  a <- a[1:length(a)]
  
  url.r <- paste0('http://www.kuh.ac.kr/bin/introduce/', a)
  tables$url <-  url.r
  tables$source <- '건국대학교병원'
  tables$date <- gsub("\\.","-",tables$date)
  ## as.character(as.Date(as.character(tables$date),"%y/%m/%d"))
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}


##################고려대학교의대부속구로병원##################3

# H.38 <- function(){
#   thisMon <- substr(Sys.Date()-1, 1, 7)
#   url<- "http://www.kumc.or.kr/introduction/biddingList.do"
#   html <- read_html(url, encoding = "euc-kr")
#   html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table(fill=TRUE)
#   
#   tables <- html1[[2]]
#   names(tables) <- c('id', 'x2','x3','header','x5','date','x7')
#   in.tables <- c("date", "header")
#   tables <- tables[,(names(tables) %in% in.tables)]
#   
#   tables$date <- gsub("\\.","-",tables$date)
#   
#   a <- html %>%  html_nodes(xpath = '//a') %>%  html_attr("href")
#   a <- a[str_detect(a, 'biddingView.do') == TRUE]
#   a <- a[1:length(a)]
#   
#   
#   url.r <- paste0('http://www.kumc.or.kr/introduction/', a)
#   tables$url <- url.r
#   tables$source <- '고려대학교의대부속구로병원'
#   tables$date <- gsub("\\.","-",tables$date)
#   tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
#   hosp_mysqldb(tables)
# }



##################연세대학교의과대학세브란스병원##################3

H.39 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "http://sev.iseverance.com/guidance/news/bid_notice/"
  html <- read_html(url, encoding = "UTF-8")
  html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table(fill=TRUE)
  
  tables <- html1[[1]]
  names(tables) <- c('header','x2','date')
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  a <- html %>%  html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_detect(a, 'view.asp') == TRUE]
  a <- a[1:length(a)]
  
  url.r <- paste0("http://sev.iseverance.com/guidance/news/bid_notice/", a)
  url.r <- url.r[c(1:2)]
  tables$url <- url.r
  tables$source <- '연세대학교의과대학세브란스병원'
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}

##################근로복지공단대전병원##################3

H.40 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "https://www.kcomwel.or.kr/daejeon/info/bidd.jsp"
  html <- read_html(url, encoidng = "UTF-8")
  html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table()
  
  tables <- html1[[1]]
  names(tables) <- c('id', 'header', 'x3', 'x4', 'date', 'x6')
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  a <- html %>%  html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_detect(a, 'view&article_no=') == TRUE]
  a <- a[1:length(1)]
  
  url.r <- paste0("https://www.kcomwel.or.kr/daejeon/info/bidd.jsp", a)
  tables$url <- url.r
  tables$source <- '근로복지공단대전병원'
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}

##################대전보훈병원##################3
#잘안됨
H.41 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "http://daejeon.bohun.or.kr/010intro/intro05.php?left=7&boardid=cbid3"
  html <- read_html(url, encoding = "euc-kr")
  html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table(fill=TRUE)
  
  
  tables <-html1[[1]]
  names(tables) <-c('id', 'header', 'x3', 'x4', 'date','x5')
  #tables$date <- gsub("\\.","-",tables$date)
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  a <- html %>%  html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_detect(a, './5_body_view.php') == TRUE]
  a <- a[1:length(a)]
  
  url.r <- paste0("http://daejeon.bohun.or.kr/010intro/", a)
  
  tables$url <- url.r
  tables$source <- '대전보훈병원'
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}



##################충남대학교병원##################3

H.42 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "https://www.cnuh.co.kr/home/01_guide/tender_list.jsp"
  html <- read_html(url, encoding = "UTF-8")
  html1 <- html %>%  html_nodes(xpath= '//table') %>%  html_table(fill=TURE)
  
  
  tables <- html1[[1]]
  names(tables) <- c('id','header','x3','date','x5')
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  
  tables$date <- as.character(as.Date(as.character(tables$date),"%y-%m-%d"))
  
  
  a <- html %>%  html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_detect(a, 'tender_read.jsp') == TRUE]
  a <- a[1:length(a)]
  
  url.r <- paste0("https://www.cnuh.co.kr/home/01_guide/", a)
  tables$url <- url.r
  tables$source <- '충남대학교병원'
  
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}


##################곽병원##################3
#잘안됨
H.43 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "https://www.kwakh.co.kr/index.php?status=menu5&code=07"
  html <- read_html(url, encoding = "UTF-8")
  html1 <- html %>%  html_nodes(xpath = '//table') %>%  html_table(fill=TRUE)
  
  tables <- html1[[]]
  names(tables) <- c('')
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  
  
  a <- html %>% html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_dectect(a, 'index.php') == TRUE]
  a <- a[1:length(a)]
  
  url.r <- paste0("https://www.kwakh.co.kr/", a)
  tables$url <- url.r
  tables$source <- '곽병원'
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}

##################대구의료원##################3

H.44 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "http://www.daegumc.co.kr/content/05community/03_01.php"
  html <- read_html(url, encoding = "UTF-8")
  html1 <- html %>%  html_nodes(xpath = '//table') %>% html_table(fill=TRUE)
  
  
  tables <- html1[[1]]
  names(tables) <- c('id', 'header','date','x', 'x5')
  in.tables <- c("date", "header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  tables$header <- gsub("\t","", tables$header)
  tables$date <- gsub("\\.","-",tables$date)
  
  a <- html %>%  html_nodes(xpath = "//a") %>%  html_attr("href")
  a <- a[str_detect(a, '&b_num=') == TRUE] 
  a <- a[1:length(a)]
  
  url.r <- paste0("http://www.daegumc.co.kr/content/05community/03_01.php?proc_type=view&a_num=17145949&", a)
  tables$url <- url.r
  tables$source <- '대구의료원'
  tables$date <- substr(tables$date,1,10)
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}


##################대구가톨릭대학교병원##################3

H.45 <- function(){
  thisMon <- substr(Sys.Date()-1, 1, 7)
  url <- "http://www.dcmc.co.kr/content/07community/01_04.asp"
  html <- read_html(url, encdoing = "UTF-8")
  html1 <- html %>% html_nodes(xpath = '//table') %>%  html_table(fill=TRUE)
  
  tables <- html1[[1]]
  names(tables) <- c('id', 'header', 'x3','date','x5')
  in.tables <- c("date","header")
  tables <- tables[,(names(tables) %in% in.tables)]
  
  tables$date <- gsub("\\.","-",tables$date)
  
  a <- html %>%  html_nodes(xpath = '//a') %>%  html_attr("href")
  a <- a[str_detect(a, 'b_num=') ==TRUE]
  a <- a[1:length(a)]
  
  url.r <- paste0("http://www.dcmc.co.kr/content/07community/01_04.asp", a)
  tables$url <- url.r
  tables$source <-'대구가톨릭대학교병원' 
  tables <- tables[tables$header != "" & substr(tables$date, 1, 7) == thisMon ,]
  hosp_mysqldb(tables)
}

##################국립재활원재활병원##################2

H.46 <- function(){
  thisMon <- substr(Sys.Date(),1,7)
  url <- "http://www.nrc.go.kr/nrc/nrc/nrcList.jsp?menu_cd=M_06_03&sub_loc=10&fno=2"
  html <- read_html(GET(url),encoding="euc-kr")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
  r_tables <- html1[[1]]
  names(r_tables) <- c('id','header','aa','date','bb', 'cc')
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,1,15) == 'nrcView.jsp?no=']
  url.r <-  paste0('http://www.nrc.go.kr/nrc/nrc/',a)
  r_tables$url <- url.r
  drops <- c("id","aa","bb", "cc")
  r_tables$date <- substr(r_tables$date,1,10)
  r_tables <- r_tables[,!(names(r_tables) %in% drops)]                                                         
  r_tables <- r_tables[r_tables$header != "" & substr(r_tables$date,1,7) == thisMon ,]
  r_tables$source <- '국립재활원재활병원'
  hosp_mysqldb(tables)
}



##################강남을지병원##################2

H.47 <- function(){
  thisMon <- substr(Sys.Date(),1,7)
  url <- "http://gangnam.eulji.or.kr/info/info_pg06_06.jsp"
  html <- read_html(GET(url),encoding="utf-8")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table(fill=TRUE)
  tables <- html1[[1]]
  names(tables) <- c('id','header','aa','date','bb')
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,35,41) == 'BOARD_7']
  url.r <-  paste0('http://gangnam.eulji.or.kr',a)
  tables$url <- url.r
  contains <- c("header","date","url")
  tables <-tables[,(names(tables) %in% contains)]
  tables$date <- gsub('\\.','-',tables$date)                                                         
  tables <- tables[tables$header != "" & substr(tables$date,1,7) == thisMon ,]
  tables$source <- '강남을지병원'
  hosp_mysqldb(tables)
}


##################전라북도군산의료원##################2

H.48 <- function(){
  thisMon <- substr(Sys.Date(),1,7)
  url <- "https://www.kunmed.or.kr/board/bbs/board.php?bo_table=online02&lnb=602060"
  httr::set_config( config( ssl_verifypeer = 0L ) )
  html <- read_html(GET(url),encoding="utf-8")
  html1 <- html %>% html_nodes(xpath='//table') %>% html_table()
  tables <- html1[[1]]
  names(tables) <- c('id','header','aa','date','bb')
  
  a <- html %>% html_nodes(xpath='//a') %>% html_attr("href")
  a <- a[substr(a,18,25) == 'bo_table']
  a <- substr(a,3,nchar(a))
  url.r <-  paste0('https://www.kunmed.or.kr/board',a)
  #tables$date <- gsub('/','-',tables$date) #error
  tables$url <- url.r
  drops <- c("id","aa","bb")
  tables <-tables[,!(names(tables) %in% drops)]                                                         
  tables <- tables[tables$header != "" & substr(tables$date,1,7) == thisMon ,]
  tables$source <- '전라북도군산의료원'
  hosp_mysqldb(tables)
}



