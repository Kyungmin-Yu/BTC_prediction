library(rvest)
library(dplyr)
library(stringr)
setwd("C:/Users/ykm25/Desktop/FR/특별세미나/data")
#### 기본세팅 ####
date<-seq(as.Date('2017/12/30', '%Y/%m/%d'), as.Date('2021/4/30', '%Y/%m/%d'), 1)
date1<-gsub("-",".",date)
date2 <- gsub("-","",date)

basic_url <- 'https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%BD%94%EC%9D%B8&sort=2&photo=0&field=0&pd=3&ds=2017.12.28&de=2018.01.06&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20171228to20180106,a:all&start=51'
nchar(basic_url)

grid_date <- 1
basic_url1 <- substr(basic_url,1,regexpr('ds=',basic_url)+2)
basic_url2 <- '&de='
basic_url3 <- substr(basic_url,regexpr('&mynews',basic_url),regexpr('from',basic_url)+3)
basic_url4 <- 'to'
basic_url5 <- substr(basic_url,regexpr('from',basic_url)+22,nchar(basic_url))


## url 만들기
url = paste0(basic_url1, date1,basic_url2, date1, basic_url3,
       date2,basic_url4,date2,basic_url5)

page = rep(100, length(url))
## 페이지 넘기는 url
page_url<-c()
for(i in 1:length(url)){
  temp = url[i]
  for (k in 0:(page[i]%/%10)){
    front=substr(temp,1,regexpr('&start=',temp)+6)
    back=substr(temp,regexpr('&start=',temp)+7,nchar(temp))
    page_temp=paste0(front, k, '1')
    page_url<-c(page_url,page_temp)
  }
  }
length(page_url)
url_vector = page_url
news_df = NULL
{ #여기돌리세요.
  news_link <-c()
  print('기사title따오기시작')
  start_time <- Sys.time()
  for (i in 3932:length(url_vector)){
    order = (i-1)%/%11+1
    day = date[order]
    naver_url_1 <- url_vector[i]
    html <- read_html(naver_url_1)
    temp <- unique(html_nodes(html, "#main_pack") %>% 
                     html_nodes("li") %>% 
                     html_nodes(css=".news_area")%>%
                     html_nodes("a") %>% 
                     html_attr("title"))
    # news_link <- c(news_link, temp[!is.na(temp)])
    if(length(temp[!is.na(temp)]) == 0){temp = "no news"}
    tmp_df = data.frame(title = temp[!is.na(temp)], date=day)
    news_df = rbind(news_df, tmp_df)
    if (i %% 50==0){
      elapsed_time = Sys.time()-start_time
      print(paste(round(i/length(url_vector)*100,2),"% complete  ",i,'/',length(url_vector),sep="",
                  ' ,    경과시간 : ',round(elapsed_time,2)))}
    write.csv(news_df, 'news_title2.csv', row.names=F)
  }
  
  news_link <- news_link[nchar(news_link)>=2]
  print('url긁어오기완료')
}

final = news_df %>% filter(title!="no news") %>% unique()
View(final)
write.csv(final, 'final_title.csv', row.names=F)
