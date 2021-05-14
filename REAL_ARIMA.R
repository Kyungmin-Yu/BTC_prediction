
########## Train Test Split ##########
setwd("C:/Users/ykm25/Desktop/FR/특별세미나/data/final_data")
library(tidyverse)
library(data.table)
library(forecast)
data = fread('real_final_data2.csv')
data$Y = lead(data$btc, lag=1) 
data = data[!is.na(data$Y),] 
train = data %>% filter(date< "2021-03-01")
train_x = train %>% select(-c(date, btc,Y))
armx_train_x = train_x %>% mutate_at(setdiff(colnames(train_x), "sentiment"), log) %>% as.matrix()
train_y = train$Y %>% log()

RMSE = function(pred, true){sqrt(mean((pred-true)^2))}

order = expand.grid(p = seq(0,2), d = seq(0,3), q = seq(0,3))

for(o in 1:nrow(order)){
  p = order[o,'p']
  q = order[o, 'q']
  d = order[o, 'd']
  fit.arimax = Arima(train_y,order=c(p,d,q),method="ML")
  order[o,'AIC'] = AIC(fit.arimax)
  print(paste0(o/nrow(order)*100, "% 완료"))
}
optimal_model = order[which.min(order$AIC),] # best model : arima(2,1,2)
p = optimal_model$p
d = optimal_model$d
q = optimal_model$q

########## Find optimal window size ##########

val_idx = seq(from = 590, to = 790, by = 50) %>% lapply(function(x){seq(x-99, x, by=1)})
# val_idx = list(701:900, 876:1075, 1016:1215)
grid.window = data.frame(window_size = seq(50, 300, by=20))

for(k in 1:nrow(grid.window)){
  result = numeric(5)
  baseline = numeric(5)
  window = grid.window[k, 'window_size']
  for(j in 1:length(val_idx)){
    validation = val_idx[[j]]
    ytrue = train_y[validation]
    ypred = c()
    for(i in 1:length(validation)){
      start = validation[1]-window
      end = start+window
      tmp_train = armx_train_x[start:end,]
      fit.arimax = Arima(train_y[start:end],xreg= tmp_train  , order=c(p,d,q),method="ML")
      ypred = c(ypred, predict(fit.arimax,newxreg =armx_train_x[(end+1),] %>% t() , h=1)$pred[1])
    }
    result[j] = RMSE(exp(ypred), exp(ytrue))
  }
  grid.window[k, 'RMSE'] = mean(result)
  print(paste0(k/nrow(grid.window)*100, "% 완료"))
}
grid.window[which.min(grid.window$RMSE),]# optimal window size = 90
window = grid.window[which.min(grid.window$RMSE), 'window_size']

ggplot(grid.window)+
  geom_line(aes(x=window_size, y=RMSE))+
  theme_minimal()+
  labs(x="", y="")
########## Test ##########

test = data %>% filter(date >=as.Date("2021-03-01")-window)
test_y = test$Y %>% log()
n = length(test_y)

ypred= c()
for(i in 1:(n-window)){
  start = i
  end = i+window-1
  tmp_test_y = test_y[start:end]
  fit.arima = Arima(tmp_test_y, order=c(p,d,q),method="ML")
  ypred = c(ypred, predict(fit.arima, h=1)$pred[1])
}
RMSE(exp(ypred), exp(test_y[(length(test_y)-59):length(test_y)]))

RMSE(exp(test_y[(length(test_y)-60):(length(test_y)-1)]), exp(test_y[(length(test_y)-59):length(test_y)])) # baseline : 2405269

data.frame(pred = exp(ypred), true = exp(test_y[(length(test_y)-59):length(test_y)]), date = seq(as.Date("2021-03-01"), as.Date("2021-04-29"), by=1)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=true))+
  geom_line(aes(x=date, y=pred),color="red")
# (2,1,0): 2050691

RMSE(exp(test_y[(length(test_y)-60):(length(test_y)-1)]), exp(test_y[(length(test_y)-59):length(test_y)])) # baseline : 2405269

options(scipen = 999)
data.frame(pred = exp(ypred), true = exp(test_y[(length(test_y)-59):length(test_y)]), date = seq(as.Date("2021-03-01"), as.Date("2021-04-29"), by=1)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=true))+
  geom_line(aes(x=date, y=pred),color="red")+
  theme_minimal()+
  labs(x="", y="")

########## GARCH ##########

library(fGarch)
Model.Arima = arima(train_y, order=c(2,1,3),method="ML")
Resids = resid(Model.Arima)
Result=data.frame(Model="m",AIC=0)
Result=data.frame(Model="m",AIC=0)
q=0
for (i in 1:9){
  for (j in 1:9){
    q=q+1
    fit=garchFit(substitute(~garch(p,q),list(p=i,q=j)),data=Resids,trace=F)
    
    Result=rbind(Result,data.frame(Model=paste("m-",i,"-",j),AIC=fit@fit$ics[1]))
    
  }
}
Result[which.min(Result$AIC),]
window=90

ypred= c()
for(i in 1:(n-window)){
  start = i
  end = i+window-1
  tmp_test_y = test_y[start:end]
  fit.arima = arima(tmp_test_y, order=c(2,1,3),method="ML")
  fit.garch = garchFit(~garch(3,5), data=resid(fit.arima),trace=F)
  arima_pred = predict(fit.arima, h=1)$pred[1]
  garch_pred =  predict(fit.garch )$meanForecast[1]
  pred = arima_pred + garch_pred
  ypred = c(ypred, pred)
}
RMSE(exp(ypred), exp(test_y[(length(test_y)-59):length(test_y)])) # (2,1,3): 2361882, garch(3,5):2377644

RMSE(exp(test_y[(length(test_y)-60):(length(test_y)-1)]), exp(test_y[(length(test_y)-59):length(test_y)])) # baseline : 2405269

data.frame(pred = exp(ypred), true = exp(test_y[(length(test_y)-59):length(test_y)]), date = seq(as.Date("2021-03-01"), as.Date("2021-04-29"), by=1)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=true))+
  geom_line(aes(x=date, y=pred),color="red")
