#### Extract Information
rm(list=ls()) ## Clear Environment
gc()
library(RMeCab) ## 은전한닢 형태소분석기
library(RMySQL) ## MySQL
library(stringi) ## Text Handling
library(LSAfun) ## LSA Function(summary function)
library(tm) ## text mining
library(parallel) ## parallel
library(doParallel) ## parallel
library(ff) ## virtual Memory
library(plyr) ## data Handling
library(stringi)
library(stringr)
library(devtools)

to <- as.character(Sys.Date())
from <- as.character(Sys.Date()-2)


## parallel
registerDoParallel(cores = 6)

# query <- paste("select id, text from blog_raw where regdate > '",from,"' and regdate < '",to,"'", sep="") ## 쿼리작성
query <- paste0("SELECT A.id,A.text,IFNULL(B.id,0) as cnt
                FROM blog_raw A
                LEFT OUTER JOIN blog_ext_info B on A.id = B.id
                WHERE A.date = '",to,"' and B.id is null")
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con, "set names utf8") ## DB Charset UTF-8로 변경
text <- dbGetQuery(con, query) ## 데이터 받아오기
dbDisconnect(con) ## 연결 해제
# text <- text[text$cnt == 0,]
text <- text[,c("id","text")]
text$text <- gsub("  ","", text$text)
text <- text[text$text != "",]

ext_info <-foreach(j = 1:length(text[,1]), .combine=rbind,.packages=c("ff","RMeCab"))%dopar% {
  # for (j in 1:length(text[,1])){
  tryCatch({
    text1 <- as.character(text$text[j])
    text2 <- unlist(str_split(text1, " ")) ## unlist
    
    ## 대상, 지역, 규모 
    ## 지역
    trword    <- RMeCabC(text1)
    trword2   <- unlist(trword)
    trword    <- trword2[names(trword2)=="NNG"|names(trword2)=="NNP"|
                           names(trword2)=="SL"|names(trword2)=="SH"]
    word      <- paste(trword, collapse=" ")
    ## term Freq
    locale <- na.omit(as.data.frame(str_extract_all(word, "(강화|김포|파주|고양|연천|포천|동두천|양주|의정부|남양주|가평|구리|양평|여주|광주|이천|하남|성남|용인|안성|평택|오산|화성|수원|군포|안산|시흥|의왕|안양|과천|광명|부천|옹진|철원|화천|양구|인제|고성|속초|양양|강릉|홍천|춘천|횡성|평창|원주|영월|정선|동해|삼척|태백|태안|서상|당진|아산|천안|예산|홍성|청양|공주|연기|계룡|논산|부여|보령|서천|대전|금산|진천|음성|충주|제천|단양|괴산|청주|청원|보은|옥천|영동|
                                                    독일|군산|익산|완주|진안|무주|장수|남원|임실|정읍|부안|고창|순창|영광|장성|담양|곡성|구례|광양|순천|여수|고흥|보성|화순|광주|나주|함평|무안|목포|영암|강진|장흥|해남|완도|진도|남해|문경|예천|영주|봉화|울진|영양|영덕|안동|의성|상주|김천|구미|칠곡|성주|고령|대구|경산|영천|포항|경주|청도|거창|함양|산청|합천|하동|사천|진주|고성|의령|함안|창원|창녕|밀양|김해|양산|울산|부산|거제|경기|강원|전라도|전라북도|전라남도|경상도|경상북도|경상남도|충청도|충청남도|충청북도|전남|경남|전북|경북|충북|충남)",simplify=T)))
    word1 <- strsplit(word,split=" ",fixed=T)[[1]]
    area <- c("강화","김포","파주","고양","연천","포천","동두천","양주","의정부","남양주","가평","구리","양평","여주","광주","이천","하남","성남","용인","안성","평택","오산",
              "화성","수원","군포","안산","시흥","의왕","안양","과천","광명","부천","옹진","철원","화천","양구","인제","고성","속초","양양","강릉","홍천","춘천","횡성","평창",
              "원주","영월","정선","동해","삼척","태백","태안","서상","당진","아산","천안","예산","홍성","청양","공주","연기","계룡","논산","부여","보령","서천","대전","금산",
              "진천","음성","충주","제천","단양","괴산","청주","청원","보은","옥천","영동","군산","익산","완주","진안","무주","장수","남원","임실","정읍","부안","고창","순창",
              "영광","장성","담양","곡성","구례","광양","순천","여수","고흥","보성","화순","광주","나주","함평","무안","목포","영암","강진","장흥","해남","완도","진도","남해",
              "문경","예천","영주","봉화","울진","영양","영덕","안동","의성","상주","김천","구미","칠곡","성주","고령","대구","경산","영천","포항","경주","청도","거창","함양",
              "산청","합천","하동","사천","진주","고성","의령","함안","창원","창녕","밀양","김해","양산","울산","부산","거제","경기","강원","전라도","전라북도","전라남도","경상도",
              "경상북도","경상남도","충청도","충청남도","충청북도","충북","경북","전북","전남","경남","충남")
    
    # Return matching elements
    locale <- word1[word1 %in% area] 
    #locale <- system("mecab -d /usr/local/lib/mecab/dic/mecab-ko-dic", input=text1, intern=T) ##System에서 mecab 실행
    #locale <- locale[str_detect(locale, ",지명,")]
    #locale <- strsplit(locale, ",", fixed=T)
    #if (length(locale) > 0){
    #for (i in 1:length(locale)){
    #  temp <- locale[[i]][4]
    #  if (i == 1) locale1 <- temp else locale1 <- c(locale1, temp)
    #}
    locale <- unique(locale)
    locale <- as.character(locale)
    locale <- paste(locale, collapse=", ")
    
    #} else locale <- ""
    
    
    ## 금액
    money1 <- na.omit(as.data.frame(str_extract(text2, "[0-9]+(만원|억원|천원|조|억|천만원)")))
    money1$id <- rownames(money1)
    names(money1) <- c("money", "id")
    money <- unique(money1)
    id <- unique(money$id)
    money <- text2[as.numeric(id)]
    money <- gsub("이며","",money)
    money <- gsub("을","",money)
    money <- gsub("은","",money)
    money <- gsub("는","",money)
    money <- gsub("고","",money)
    money <- gsub("에","",money)
    money <- gsub("밖","",money)
    money <- gsub("까지","",money)
    money <- gsub("으로","",money)
    money <- gsub("과","",money)
    money <- gsub("이","",money)
    money <- gsub("의","",money)
    money <- gsub("대","",money)
    money <- gsub("\\)","",money)
    money <- gsub("\"","",money)
    money <- gsub(",","",money)
    money <- as.character(money)
    money <- unique(money)
    if (length(money) == 0) money <- "" else  money <- paste(money, collapse=", ")
    
    # 규모
    scale1 <- as.character(na.omit(str_extract(text1, "[0-9]+(톤|㎞|km|㎥|㎡|여평|평|명|대)")))
    scale <- unique(scale1)
    scale <- as.character(scale)
    if (length(scale)== 0) scale <- "" else  scale <- paste(scale, collapse=", ")
    
    ## 기간
    period1 <- na.omit(as.data.frame(str_extract(text1, "[0-9]+(년|월|일|분기)")))
    period2 <- na.omit(as.data.frame(str_extract(text1, "(상반기|하반기)")))
    period1$id <- rownames(period1)
    period2$id <- rownames(period2)
    names(period1) <- c("period", "id")
    names(period2) <- c("period", "id")
    period <- rbind(period1, period2)
    period <- period[order(as.numeric(period$id)),]
    period <- paste(as.character(period$period), collapse=", ")
    if (period == "integer(0)") period <- "" else  period <- paste(period, collapse=", ")
    
    return(as.data.frame(cbind(locale, money, scale, period,text$id[j])))
    # if ( j == 1) ext_info <- temp else ext_info <- rbind(ext_info, temp)
  }, error=function(e){cat(j," ERROR :",conditionMessage(e), "\n")})
}
ext_info$regdate <- as.character(Sys.time())
names(ext_info)[5] <- "id"

## DB Upload
require(RMySQL)
con <- dbConnect(MySQL(), user="root", password="20110104",dbname="leadnet",host="192.168.0.21") ## DB 접속
dbGetQuery(con,"set names utf8")
db.data <- dbGetQuery(con,paste0("SELECT id FROM blog_ext_info"))
db.data <- as.numeric(db.data$id)
ext_info <- ext_info[!(ext_info$id %in% db.data),]
dbWriteTable(con, "blog_ext_info", ext_info, append=T, row.names=F)
dbDisconnect(con)

