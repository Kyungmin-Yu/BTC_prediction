setwd("C:/Users/ykm25/Desktop/FR/특별세미나/data")

library(tidyverse)
library(data.table)
library(magrittr)
library(imputeTS)
library(tm)

data_list = list.files()

## btc info
btc_info = fread(data_list[1])
btc_info %<>% filter(date >"2017-12-31" & date < "2021-05-01") %>% mutate(date=as.Date(date))

## btc price
btc_price = fread(data_list[2], encoding='UTF-8')
btc_price$date = as.Date(btc_price$date)
btc_price %<>% filter(date>"2017-12-31"& date < "2021-05-01") %>% select(종가, date) %>% unique()
colnames(btc_price) = c("btc", "date")

invest_preprocess = function(data){
  data$date = as.Date(data$날짜, format="%Y년 %m월 %d일")
  data %<>%  filter(date>"2017-12-31"& date < "2021-05-01") %>% select(종가, date)
  if(is.character(data$종가)){data$종가 = as.numeric(removePunctuation(data$종가))}
  return(data)
}

## VIX
vix = fread(data_list[3], encoding = 'UTF-8') %>% invest_preprocess()
colnames(vix) = c("vix", "date")

## vkospi
vkospi = fread(data_list[4], encoding='UTF-8')%>% invest_preprocess()
colnames(vkospi) = c("vkospi", "date")

## NVDA
nvda = fread(data_list[5], encoding='UTF-8')%>% invest_preprocess()
colnames(nvda) = c("nvda", "date")

## USD-KRW
usdkrw = fread(data_list[6], encoding='UTF-8')%>% invest_preprocess()
colnames(usdkrw) = c("usdkrw", "date")

## us epu
epu = fread(data_list[7]) %>% filter(DATE >"2017-12-31")
colnames(epu) = c("date","epu")
epu$date = as.Date(epu$date)

## gold
gold = fread(data_list[8], encoding='UTF-8')%>% invest_preprocess()
colnames(gold) = c("gold", "date")

## dollar index
dollaridx = fread(data_list[9], encoding='UTF-8')%>% invest_preprocess()
colnames(dollaridx) = c("dollaridx", "date")

## cme btc futures
futures = fread(data_list[10], encoding='UTF-8')%>% invest_preprocess()
colnames(futures) = c("futures", "date")

## kospi
kospi = fread(data_list[11], encoding='UTF-8')%>% invest_preprocess()
colnames(kospi) = c("kospi", "date")

final_data = left_join(btc_info ,btc_price, by='date') %>% 
  left_join(vix) %>% 
  left_join(vkospi) %>% 
  left_join(nvda) %>% 
  left_join(usdkrw) %>% 
  left_join(epu) %>% 
  left_join(gold) %>% 
  left_join(dollaridx) %>% 
  left_join(futures) %>% 
  left_join(kospi) %>% 
  arrange(date)


final_data %<>% mutate(vix = na_interpolation(vix, option = "spline"),
                      vkospi = na_interpolation(vkospi, option = "spline"),
                      nvda = na_interpolation(nvda, option = "spline"),
                      usdkrw = na_interpolation(usdkrw, option = "spline"),
                      dollaridx = na_interpolation(dollaridx , option = "spline"),
                      kospi= na_interpolation(kospi, option = "spline"),
                      futures= na_interpolation(futures, option = "spline"),
                      gold =  na_interpolation(gold, option = "spline"))

colSums(is.na(final_data))
write.csv(final_data, 'C:/Users/ykm25/Desktop/FR/특별세미나/data/final_data.csv', row.names=F)
