# 필요한 라이브러리 불러오기
library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(ggplot2)


# 마지막 페이지를 구하는 함수
get_last_page <- function(input_url){
  html <- read_html(input_url, encoding ="euc-kr")
  sise <- html %>% html_nodes(".pgRR") %>%
    html_nodes("a") %>% html_attr("href") %>%
    str_split(., pattern = "page=")
  
  last_page <- sise[[1]][2]
  return(last_page)
}

# 페이지와 기본 url을 받아 기본정보 데이터 프레임을 반환해주는 함수
basic_info <- function(temp_html){
  # 숫자형 데이터 리스트 만들기
  number_data <- temp_html %>% html_nodes(".number") %>% html_text() %>%
    parse_number(na="N/A")
  
  # 종목명 데이터 리스트 만들기
  Name_jongmok <- temp_html %>%
    html_nodes(".tltle") %>%
    html_text()
  
  # 각 종목별 코드 가져오기
  codes <- temp_html %>% html_nodes("td") %>% 
    html_nodes("a") %>% html_attr("href")
  codes <- codes[grepl("main", codes)]
  code_arr <- array(NA, dim=c(length(codes), 1))
  
  for (i in 1:length(codes)){
    code_arr[i, 1] <- strsplit(codes[i], "code=")[[1]][2]
  }
  
  # 각 카테고리 명 가져오기
  category <- temp_html %>% html_nodes("th") %>% html_text()
  category <- category[3:12]
  
  # 기본 정보 데이터 프레임 만들고 반환하기
  return_dataFrame <- data.frame(matrix(number_data, ncol=length(category), 
                                        byrow = T))
  names(return_dataFrame) <- category
  return_dataFrame$종목명 <- Name_jongmok
  return_dataFrame$종목코드 <- code_arr[,1]
  # 종목명, 종목코드, 액면가, 상장주식수, PER, ROE만 따로 빼서 반환
  #return_dataFrame <- return_dataFrame %>%
  #  select(종목명, 종목코드, 거래량, 등락률, 액면가, 상장주식수, PER, ROE)
  
  return(return_dataFrame)
}



remDr <- remoteDriver(remoteServerAddr = 'localhost', 
                      port = 4487L , browserName = "chrome")

#remDr <- remoteDriver(remoteServerAddr = 'localhost', port = 4444, browserName = "chrome") 


remDr$open() 
remDr$close()
remDr$getStatus()



get_site_info <- function(mode){
  remDr$navigate("https://finance.naver.com/sise/sise_market_sum.naver")
  temp_url <- "https://finance.naver.com/sise/sise_market_sum.nhn?&page="
  if(mode == 1) {
    select_num <- c(1,4,6,12,15,17)
  }else if(mode == 2){
    select_num <- c(21, 22, 23, 24, 25, 26)
  }
  
  # checked 속성이 있는 요소를 찾기 위한 XPath
  xpath <- '//input[@type="checkbox" and @checked]'
  
  # 요소 선택
  elements <- remDr$findElements(using = "xpath", value = xpath)
  
  # 선택된 요소의 checked 속성 제거
  for (element in elements) {
    remDr$executeScript("arguments[0].removeAttribute('checked')", list(element))
  }
  
  #remDr$screenshot(display = TRUE)
  # option1부터 option6까지 체크박스 클릭
  for (i in select_num) {
    element_id <- paste0("option", i)
    checkbox <- remDr$findElement(using = "xpath", value = sprintf('//input[@type="checkbox" and @id="%s"]', element_id))
    checkbox$clickElement()
  }
  element <- remDr$findElement(using = "css", "div.item_btn > a")
  element$clickElement()
  
  final_data <- data.frame()
  for (i in 1:get_last_page(paste0(temp_url, 1))){
    remDr$navigate(paste0(temp_url, i))
    html <- read_html(remDr$getPageSource()[[1]])
    final_data <- rbind(final_data, basic_info(html))
  }
  return(final_data)
}

# get_site_info함수로 원하는 열 가져오기 
final_data1 <- get_site_info(1)
final_data2 <- get_site_info(2)

# 데이터프레임에 중복되는 열 제거하기 
final_data2 <- subset(final_data2, select = -c(1,2,3,4,11,12))

# 데이터 프레임 합치기
real_final_data <- cbind(final_data1, final_data2)


summary(real_final_data$외국인비율)

# 외국인 비율 히스토그램 확인하기
log_real_final_data <- real_final_data
log_real_final_data$외국인비율 <- log10(log_real_final_data$외국인비율)

# 처음 외국인 비율은 비교가 어렵다 
hist(real_final_data$외국인비율, breaks = 100)

# 로그변환 실시해서 히스토 그램 그려본 것
hist(log_real_final_data$외국인비율, breaks = 30)


ggplot(data = log_real_final_data, aes(x = 외국인비율, y = 거래량, fill = 외국인비율)) +
  geom_point() +
  labs(x = "외국인비율", y = "거래량", title = "외국인 비율에 따른 거래량") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 거래량도 조정이 필요해 보인다. 
hist(log_real_final_data$거래량)

# 로그변환 
log_real_final_data$거래량 <- log10(log_real_final_data$거래량)

# 재실시
ggplot(data = log_real_final_data, aes(x = 외국인비율, y = 거래량, fill = 외국인비율)) +
  geom_point() +
  labs(x = "외국인비율", y = "거래량", title = "외국인 비율에 따른 거래량") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 시가총액 로그 변환 
log_real_final_data$시가총액 <- log10(log_real_final_data$시가총액)

# 외국인비율에 따른 시가총액
ggplot(data = log_real_final_data, aes(x = 외국인비율, y = 시가총액, fill = 외국인비율)) +
  geom_point() +
  labs(x = "외국인비율", y = "시가총액", title = "외국인 비율에 따른 시가총액") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 시가총액 로그 변환 
log_real_final_data$매출액 <- log10(log_real_final_data$매출액)

# 외국인비율에 따른 매출액
ggplot(data = log_real_final_data, aes(x = 외국인비율, y = 매출액, fill = 외국인비율)) +
  geom_point() +
  labs(x = "외국인비율", y = "매출액", title = "외국인 비율에 따른 매출액") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 시가총액 로그 변환 
log_real_final_data$당기순이익 <- log10(log_real_final_data$당기순이익)

# 외국인비율에 따른 당기순이익
ggplot(data = log_real_final_data, aes(x = 외국인비율, y = 당기순이익, fill = 외국인비율)) +
  geom_point() +
  labs(x = "외국인비율", y = "당기순이익", title = "외국인 비율에 따른 당기순이익") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 로그변환
log_real_final_data$보통주배당금 <- log10(log_real_final_data$보통주배당금)

# 보통주배당금 
ggplot(data = log_real_final_data, aes(x = 외국인비율, y = 보통주배당금, fill = 외국인비율)) +
  geom_point() +
  labs(x = "외국인비율", y = "보통주배당금", title = "외국인 비율에 따른 보통주배당금") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
