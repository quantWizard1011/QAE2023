install.packages("RSelenium")
install.packages("dplyr")
install.packages("stringr")
install.packages("rvest")
install.packages("kableExtra")
install.packages("tidyverse")
install.packages("DT")
install.packages("ggplot2")
install.packages("treemapify")
install.packages("zoo")
install.packages("shiny")

library(RSelenium)
library(dplyr)
library(stringr)
library(rvest)
library(kableExtra)
library(tidyverse)
library(DT)
library(ggplot2)
library(treemapify)
library(zoo)
library(shiny)
library(openxlsx)


rD <- rsDriver(port=4727L, chromever="113.0.5672.63")
remDr <- rD$client

##로그인
URL_rogin <- str_c("https://www.gurufocus.com/login?ref=%2F")
remDr$navigate(URL_rogin)

id <- "rr32121@gmail.com"
pw <- "1q2w#E$R%T"

pattern_id <- "#login-dialog-name-input"
element_id <- remDr$findElement(using = "css", pattern_id)
element_id$clickElement()
element_id$clearElement()
element_id$sendKeysToElement(list(as.character(id)))

pattern_pw <- "#login-dialog-pass-input"
element_pw <- remDr$findElement(using = "css", pattern_pw)
element_pw$clickElement()
element_pw$clearElement()
element_pw$sendKeysToElement(list(as.character(pw)))

pattern_click <- ".el-button--submit"
element_click <- remDr$findElement(using = "css", pattern_click)
element_click$clickElement()

Sys.sleep(3)


##페이지 넘기기 함수
more.items <- function() {
  pattern <- ".data-table-footer-pagination .el-icon-arrow-right"
  element <- remDr$findElement("css", pattern)
  element$clickElement()
}


## 투자구루별 투자종목 데이터 수집
list <- c("warren%2Bbuffett", "ken%2Bfisher","dodge%2B%26%2Bcox","vanguard%2Bhealth%2Bcare%2Bfund","first%2Beagle%2Binvestment","bill%2Bgates","ron%2Bbaron","hotchkis%2B%26%2Bwiley","barrow,%2Bhanley,%2Bmewhinney%2B%26%2Bstrauss","richard%2Bpzena","carl%2Bicahn","t%2Browe%2Bprice%2Bequity%2Bincome%2Bfund","chris%2Bdavis","bill%2Bnygren","jefferies%2Bgroup","chuck%2Bakre","steve%2Bmandel","john%2Brogers","bill%2Backman","yacktman%2Basset%2Bmanagement","mario%2Bgabelli","tom%2Bgayner","yacktman%2Bfund","ruane%2Bcunniff","seth%2Bklarman","charles%2Bbrandes","smead%2Bvalue%2Bfund","joel%2Bgreenblatt","george%2Bsoros","yacktman%2Bfocused%2Bfund","glenn%2Bgreenberg","mason%2Bhawkins","donald%2Bsmith%2B%26%2Bco","david%2Btepper","david%2Beinhorn","tweedy%2Bbrowne","wallace%2Bweitz","prem%2Bwatsa","bruce%2Bberkowitz","john%2Bpaulson")
file_name <- NULL
collect <- NULL

for (i in list){
  URL <- str_c("https://www.gurufocus.com/guru/",i,"/stock-picks?view=table")
  remDr$navigate(URL)
  res <- read_html(URL)
  result2 <- NULL
  Sys.sleep(2)
  more.items()
  Sys.sleep(1)
  more.items()
  
  pattern <- ".aio-tabs-item"
  number <- res %>% 
    html_nodes(pattern) %>% 
    html_text() %>% 
    str_trim() %>% 
    str_replace(.,"Total ","") %>% 
    as.numeric()
  number <- number %/% 100 + 1
    
    for (i in 1:number){
      txt <- remDr$getPageSource()[[1]]
      res <- read_html(txt)
      
      # 투자대가 이름
      pattern <- ".name"
      file <- res %>% 
        html_nodes(pattern) %>% 
        html_text() 
      file <- str_trim(file)
      file <- str_replace_all(file, " ", "_")
      
      # 테이블 불러오기(미국종목만)
      table <- res %>% 
        html_table()
      table <- table[[3]]
      
      us <- table$`Price Range`[1:nrow(table)]
      start_us <- str_locate(us, "\\(")[,1]+1
      end_us <- str_locate(us, "\\(")[,1]+1
      
      table$us <- str_sub(us, start=start_us, end=end_us)
      table <- table[grepl("\\$", table$us),]
      
      # ticker
      ticker <- table$Ticker[1:nrow(table)]
      
      # name
      name <- table$Company[1:nrow(table)]
      
      # date
      date <- table$Date[1:nrow(table)]
      
      # action
      action <- table$Action[1:nrow(table)]
      
      # price
      a <- table$`Price Range`[1:nrow(table)]
      
      start <- str_locate(a, "\\(")[,1]+2
      end <- str_locate(a, "\\)")[,1]-1
      
      price <- str_sub(a, start=start, end=end) %>% as.numeric()
      
      # change(종목수 변화)
      change <- table$`Share Change`[1:nrow(table)]  
      change <- as.numeric(gsub(",","",change)) / 1000000
      
      
      # chnage value(million)(종목수 * 매매 평균 가격)
      c_value <- price * change
      
      result <- cbind(ticker,name,date,action,price,change,c_value)
      result <- result[,c("ticker","date","change","c_value")] %>% as.data.frame() 
      
      result2 <- rbind(result2,result) 
      
      print(file)
      
      more.items()
      Sys.sleep(5)
      
    }
    
  assign(file,result2)
  file_name <- cbind(file_name,file)
  
  if (is.null(collect)){
    collect <- result2
  } else {
    collect <- merge(collect, result2, by=c('ticker','date'), all=TRUE)
  }
  
}


## 크롤링 데이터 전처리
collect[is.na(collect)] <- 0
collect[,3:ncol(collect)] <- sapply(collect[,3:ncol(collect)],as.character)
collect[,3:ncol(collect)] <- sapply(collect[,3:ncol(collect)],as.numeric)

# 동일 종목에 대한 주식수 변화, 투자규모 변화 합산
names(collect) <- make.unique(names(collect))

change_cols <- grep("change",names(collect),value = TRUE)
value_cols <- grep("value",names(collect),value = TRUE)

collect$sum_shares <- rowSums(collect[,change_cols],) %>% round(.,digits = 1)
collect$sum_values <- rowSums(collect[,value_cols],) %>% round(.,digits = 1)

# 종목별 산업구분 데이터 입력
gics = read.csv("us_gics.csv")
names(gics) <- c('ticker','gics','name')

# 필요한 열만 남기기
collect <- merge(collect, gics, by='ticker')
collect_col <- c("date","ticker","name","gics","sum_shares","sum_values")
collect <- collect[, collect_col]


## 종목별 상장주식수 데이터 입력
shares = read.csv("outstanding.csv")

# 결과를 저장할 벡터 초기화
result_share <- vector("list", nrow(collect))

# collect의 각 행에 대해 조건 검사 후 상장주식수 값 추출
for (i in 1:nrow(collect)) {
  matched_row <- collect$date[i] %in% shares$X.1
  matched_col <- collect$ticker[i] %in% colnames(shares)
  
  if (matched_row & matched_col) {
    result_shares <- shares[which(str_detect(shares$X.1,collect$date[i])), which(colnames(shares) == collect$ticker[i])]
  } else {
    result_shares <- 0
  }
  
  result_share[i] <- result_shares
}

collect$shares <- result_share %>% as.numeric() %>% round(.,digits=0)
collect$shares <- collect$shares / 1000
collect$shares <- round(collect$shares, digit=0)


## 전처리 결과값 
result_bs <- collect[order(as.Date(collect$date, format ="%Y-%m-%d"),decreasing = TRUE, collect$sum_values),]
names(result_bs) <- c("Date","Ticker","Name","Industry","buy/sell(mil shares)","buy/sell(mil $)","total outstandings(mil shares)")

result_bs$percent <- result_bs$`buy/sell(mil shares)`/result_bs$`total outstandings(mil shares)`*100 
result_bs$percent <- round(result_bs$percent,digits = 1)


# 날짜를 분기표시 형식으로 변환
result_bs$quarter <- as.yearqtr(result_bs$Date, format = "%Y-%m-%d", "%q%y")
result_bs$quarter <- gsub("\\s", "", result_bs$quarter)
result_bs$quarter <- paste0(str_sub(result_bs$quarter,6,7),"Q",str_sub(result_bs$quarter,3,4))

# 오류값 제거
result_bs <- distinct(result_bs)
result_bs <- result_bs %>% filter(!is.infinite(percent))
rownames(result_bs) <- NULL

## 최종 테이블별 데이터 
buy <- result_bs[result_bs$`buy/sell(mil $)` > 0,c("quarter","Ticker","Name","Industry","buy/sell(mil $)")]

buy_percent <- result_bs[result_bs$`buy/sell(mil $)` > 0,c("quarter","Ticker","Name","Industry","buy/sell(mil shares)","percent")] %>% arrange(desc(percent))

sell <- result_bs[result_bs$`buy/sell(mil $)` <= 0,c("quarter","Ticker","Name","Industry","buy/sell(mil $)","Date")] %>% arrange(desc(Date),`buy/sell(mil $)`)

sell <- sell[,c("quarter","Ticker","Name","Industry","buy/sell(mil $)")]

sell_percent <- result_bs[result_bs$`buy/sell(mil $)` <= 0,c("quarter","Ticker","Name","Industry","buy/sell(mil shares)","percent","Date")] %>% arrange(desc(Date),percent)

sell_percent <- sell_percent[,c("quarter","Ticker","Name","Industry","buy/sell(mil shares)","percent")]


# treemap 용 
buy_map <- result_bs %>% filter(Date == "2023-03-31", percent >0)
buy_map <- distinct(buy_map)
buy_map$aaa <- buy_map$`buy/sell(mil $)`

sell_map <- result_bs %>% filter(Date == "2023-03-31", percent <0)
sell_map <- distinct(sell_map)
sell_map$aaa <- sell_map$`buy/sell(mil $)` *(-1)
sell_map$percent <- sell_map$percent*(-1)

# CSV파일로 저장
write.csv(buy,"buy.csv",row.names=FALSE)
write.csv(buy_percent,"buy_percent.csv",row.names=FALSE)
write.csv(sell,"sell.csv",row.names=FALSE)
write.csv(sell_percent,"sell_percent.csv",row.names=FALSE)
write.csv(buy_map,"buy_map.csv",row.names=FALSE)
write.csv(sell_map,"sell_map.csv",row.names=FALSE)
