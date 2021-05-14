
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
########## Model Selection ##########
order = expand.grid(p = seq(0,5), d = seq(0,3), q = seq(0,5))

for(o in 1:nrow(order)){
  p = order[o,'p']
  q = order[o, 'q']
  d = order[o, 'd']
  fit.arimax = Arima(train_y,xreg=armx_train_x, order=c(p,d,q),method="ML")
  order[o,'AIC'] = AIC(fit.arimax)
  print(paste0(o/nrow(order)*100, "% 완료"))
}
optimal_model = order[which.min(order$AIC),] # best model : arima(2,1,2)
p = optimal_model$p
d = optimal_model$d
q = optimal_model$q
p;d;q
##### Variable Selction #####
variable = colnames(armx_train_x)
i = length(variable)
best_score = Inf
min_temp_aic = Inf
variable_df = NULL

while(i>0){
  temp_comb = combn(variable, i)
  for(j in 1:ncol(temp_comb)){
    variable_df = rbind(variable_df,c(paste(temp_comb[,j],collapse='/'),0))
  }
  variable_df = data.frame(variable_df,stringsAsFactors = F)
  colnames(variable_df) = c('variable','AIC')
  
  for(k in 1:nrow(variable_df)){
    var_name = strsplit(variable_df$variable[k],split='/')[[1]]
    tmp_train = armx_train_x %>% as.data.frame() %>% select(all_of(var_name)) %>% as.matrix()
    fit = Arima(train_y,xreg=tmp_train, order=c(p,d,q),method="ML")
    variable_df[k, 'AIC'] = AIC(fit)
  }
  idx = which.min(variable_df[,2])
  if (min_temp_aic == variable_df[idx,2]) break
  min_temp_var = variable_df[idx,1]
  min_temp_aic = variable_df[idx,2]
  min_temp_var = strsplit(min_temp_var,split='/')[[1]]
  print(paste0('AIC: ',round(as.numeric(min_temp_aic),6),
               '   delete var: ',setdiff(variable,min_temp_var)))
  variable = min_temp_var
  best_score = min_temp_aic
  i = i-1
  
}
using_var = strsplit(variable_df[which.min(variable_df$AIC), 'variable'], split='/')[[1]]; using_var
# c( "hash_rate" ,"vix"   ,    "nvda"  ,    "usdkrw" ,   "epu" ,      "gold"     , "futures"   ,"kospi"  ,   "sentiment")

########## Find optimal window size ##########

armx_train_x = armx_train_x %>% as.data.frame() %>% select(all_of(using_var)) %>% as.matrix()
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
      fit.arimax = Arima(train_y[start:end],xreg= tmp_train  , order=c(1,1,3),method="ML")
      ypred = c(ypred, predict(fit.arimax,newxreg =armx_train_x[(end+1),] %>% t() , h=1)$pred[1])
    }
    result[j] = RMSE(exp(ypred), exp(ytrue))
  }
  grid.window[k, 'RMSE'] = mean(result)
  print(paste0(k/nrow(grid.window)*100, "% 완료"))
}
grid.window[which.min(grid.window$RMSE),]# optimal window size = 70
window = grid.window[which.min(grid.window$RMSE), 'window_size']

########## Test ##########

test = data %>% filter(date >=as.Date("2021-03-01")-window)
test_x = test %>% select(all_of(using_var))
armx_test_x = test_x %>% mutate_at(setdiff(colnames(test_x), "sentiment"), log) %>% as.matrix()
test_y = test$Y %>% log()
n = length(test_y)

ypred= c()
for(i in 1:(n-window)){
  start = i
  end = i+window-1
  tmp_test_x = armx_test_x[start:end,]
  tmp_test_y = test_y[start:end]
  fit.arimax = Arima(tmp_test_y,xreg=tmp_test_x, order=c(p,d,q),method="ML")
  ypred = c(ypred, predict(fit.arimax,newxreg =armx_test_x[(end+1),] %>% t())$pred[1])
}
RMSE(exp(ypred), exp(test_y[(length(test_y)-59):length(test_y)])) # (2,1,0): 2050691, optimal-(1,1,3),window=130: 1978092

RMSE(exp(test_y[(length(test_y)-60):(length(test_y)-1)]), exp(test_y[(length(test_y)-59):length(test_y)])) # baseline : 2405269

data.frame(pred = exp(ypred), true = exp(test_y[(length(test_y)-59):length(test_y)]), date = seq(as.Date("2021-03-01"), as.Date("2021-04-29"), by=1)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=true))+
  geom_line(aes(x=date, y=pred),color="red")
summary(fit.arimax)
########## ARIMAX_GARCH ##########
d = armx_train_x %>% as.data.frame() %>% select(using_var) %>% as.matrix()
Model.Arima = arima(train_y,xreg= d, order=c(2,1,0),method="ML")
Resids = Model.Arima$residuals
library(fGarch)
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

test = data %>% filter(date >=as.Date("2021-03-01")-window)
test_x = test %>% select(all_of(using_var))
armx_test_x = test_x %>% mutate_at(setdiff(colnames(test_x), "sentiment"), log) %>% as.matrix()
test_y = test$Y %>% log()
n = length(test_y)
ypred= c()

for(i in 1:(n-window)){
  start = i
  end = i+window-1
  tmp_test_x = armx_test_x[start:end,]
  tmp_test_y = test_y[start:end]
  fit.arimax = Arima(tmp_test_y,xreg=tmp_test_x, order=c(1,1,3),method="ML")
  fit.garch = garchFit(~garch(3,5), data=resid(fit.arimax),trace=F)
  garch_pred =  predict(fit.garch )$meanForecast[1]
  arimax_pred = predict(fit.arimax,newxreg =armx_test_x[(end+1),] %>% t())$pred[1]
  pred = arimax_pred + garch_pred
  ypred = c(ypred, pred )
}
RMSE(exp(ypred), exp(test_y[(length(test_y)-59):length(test_y)])) # arima(2,1,0)+garch(3,5): 2090539

data.frame(pred = exp(ypred), true = exp(test_y[(length(test_y)-59):length(test_y)]), date = seq(as.Date("2021-03-01"), as.Date("2021-04-29"), by=1)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=true))+
  geom_line(aes(x=date, y=pred),color="red")

########## Interpretation ##########
using_var = c("hash_rate", "revenue" ,  "vix"    ,   "usdkrw",    "futures",   "kospi"  ,   "sentiment")
final_arima = Arima(train_y,xreg=armx_train_x, order=c(1,1,3),method="ML")
lmtest::coeftest(final_arima)
effect_size = final_arima$coef* final_arima$sigma2*1000/sd(train_y)
