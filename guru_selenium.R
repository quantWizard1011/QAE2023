library(RSelenium)
library(dplyr)
library(stringr)
library(rvest)
library(kableExtra)
library(tidyverse)

rD <- rsDriver(port=4719L, chromever="113.0.5672.63")
remDr <- rD$client

more.items <- function() {
  
  pattern <- ".data-table-footer-pagination .el-icon-arrow-right"
  element <- remDr$findElement("css", pattern)
  element$clickElement()
}


#login
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



list <- c("warren%2Bbuffett", "ken%2Bfisher","dodge%2B%26%2Bcox")

file_name <- NULL

for (i in list){
  URL <- str_c("https://www.gurufocus.com/guru/",i,"/stock-picks?view=table")
  remDr$navigate(URL)
  res <- read_html(URL)
  result2 <- NULL
  Sys.sleep(2)
  more.items()
  Sys.sleep(1)
  more.items()
  
  
 
  
  for (i in 1:3){
    txt <- remDr$getPageSource()[[1]]
    res <- read_html(txt)
    
    # 파일명
    pattern <- ".name"
    file <- res %>% 
      html_nodes(pattern) %>% 
      html_text() 
    file <- str_trim(file)
    file <- str_replace_all(file, " ", "_")
    
    #데이터 불러오기
    table <- res %>% 
      html_table()
    table <- table[[3]]
    
    # ticker
    ticker <- table$Ticker[2:101]
    
    # name
    name <- table$Company[2:101]
    
    # date
    date <- table$Date[2:101]
    
    # action
    action <- table$Action[2:101]
    
    # price
    a <- table$`Price Range`[2:101]
    
    start <- str_locate(a, "\\(")[,1]+2
    end <- str_locate(a, "\\)")[,1]-1
    
    price <- str_sub(a, start=start, end=end) %>% as.numeric()
    
    # change
    change <- table$`Share Change`[2:101]  
    change <- as.numeric(gsub(",","",change))
    
    
    # chnage value(million)
    change_value <- price * change / 1000000
    
    result <- cbind(ticker,name,date,action,price,change,change_value)
    
    result2 <- rbind(result2,result) %>% as.data.frame()
    
    print(file)

    more.items()
    Sys.sleep(3)
    result <- NULL
  }
  assign(file,result2)
  
  
  file_name <- cbind(file_name,file)
}



## 날짜별 종목 가져오기
filtered_data <-  Warren_Buffett %>% filter(date=="2023-03-31") %>% .[,c(1,7)] 
filtered_data2 <- Ken_Fisher %>% filter(date=="2023-03-31") %>% .[,c(1,7)] 


# 모은 데이터 전처리
collect <- merge(filtered_data,filtered_data2, by="ticker" , all= TRUE)

collect[is.na(collect)] <- 0
collect[,2:ncol(collect)] <- sapply(collect[,2:ncol(collect)],as.character)
collect[,2:ncol(collect)] <- sapply(collect[,2:ncol(collect)],as.numeric)

# 날짜, 종목별 합산(sum)
collect$sum <- rowSums(collect[,2:ncol(collect)],)

## 종목별 업종 데이터
gics = read.csv("us_gics.csv")
names(gics) <- c('ticker','gics','name')


# 합치기 

a <- merge(collect, gics, by='ticker')
b <- c("ticker","name","gics","sum")

d <- a[, b]
d
result <- d[order(d$sum, decreasing = TRUE),]
result$sum <- round(result$sum, digit = 1)

names(result) <- c("Ticker","Name","Industry","buy/sell(mil$)")
rownames(result) <- NULL
result
df <- result %>% kable(escape=FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, width = "5em") %>%
  column_spec(2, width = "20em") %>%
  column_spec(3, width = "15em") %>%
  column_spec(4, width = "10em")

df

############################################################

## 날짜별 종목 가져오기
filtered_data <-  Warren_Buffett %>% filter(date=="2023-03-31") %>% .[,c(1,6)] 
filtered_data2 <- Ken_Fisher %>% filter(date=="2023-03-31") %>% .[,c(1,6)] 


# 모은 데이터 전처리
collect <- merge(filtered_data,filtered_data2, by="ticker" , all= TRUE)

collect[is.na(collect)] <- 0
collect[,2:ncol(collect)] <- sapply(collect[,2:ncol(collect)],as.character)
collect[,2:ncol(collect)] <- sapply(collect[,2:ncol(collect)],as.numeric)

# 날짜, 종목별 합산(sum)
collect$sum <- rowSums(collect[,2:ncol(collect)],) 
collect$sum <- collect$sum / 1000 

## shares
shares = read.csv("outstanding.csv")
date <- "2023-03-31"
sss <- subset(shares, X.1 == date)
sss <- t(sss) %>% as.data.frame()
sss <- na.omit(sss)
sss[, 1] <- as.numeric(as.character(sss[, 1])) 
sss <-  round(sss, digit = 1)
sss$ticker <- row.names(sss)
names(sss) <- c("shares","ticker")



# 합치기
a <- merge(merge(collect, gics, by='ticker'),sss, by='ticker')
b <- c("ticker","name","gics","sum","shares")
d <- a[, b]

d$percent <- d$sum / d$shares * 100 
d$percent <- round(d$percent, digits = 2)
d$sum <- round(d$sum, digits=0)
d$shares <- round(d$shares, digits=0)
head(d)

result <- d[order(d$percent, decreasing = TRUE),]


names(result) <- c("Ticker","Name","Industry","buy/sell(thou shares)")


rownames(result) <- NULL
result
df <- result %>% kable(escape=FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, width = "5em") %>%
  column_spec(2, width = "20em") %>%
  column_spec(3, width = "15em") %>%
  column_spec(4, width = "10em")

df

