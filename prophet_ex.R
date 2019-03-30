library(prophet)
library(dygraphs)
library(dplyr)
library(xts)


df1 = read.csv("./examples/example_wp_log_peyton_manning.csv")
bitcoin = read.table('./examples/bitcoin.csv', sep=",", header=TRUE, row.names="Date")
df = read.csv('./examples/bitcoin.csv', sep=",") %>% filter(Weighted.Price!=0)
row.names(df) = df$Date
dygraph(df[c(2:5, 8)], main="Bitcoin/USD") %>%
  dyCandlestick() %>%
  dyRangeSelector( c("2018-05-01", "2018-07-30")) 

df = read.csv('./examples/bitcoin.csv', sep=",") %>% filter(Weighted.Price!=0)
df = data.frame(ds = df$Date, y = df$Weighted.Price)
n = which(df$ds=="2018-01-01")
n = which(df$ds=="2014-01-07")
data = df[n:nrow(df),]

m = prophet(
  data, growth = "linear", #l'ogistic'
  seasonality.mode ='additive') #'multiplicative'

future = make_future_dataframe(m, periods = 100)
tail(future)

forecast = predict(m, future)
dyplot.prophet(m, forecast)

forecast[1:10, c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]
plot(m, forecast)
prophet_plot_components(m, forecast)


library(KFAS)
# 一連の処理
func.kfs = function(model, inits, n.ahead=5, level=0.95, nsim=1000){
  fit <- fitSSM(model, inits, method="BFGS")
  filter.model = KFS(fit$model, smoothing = "mean")
  if(n.ahead==0){
    pred = predict(fit$model,interval="confidence")#,level,nsim)
  }
  else{
    pred = predict(fit$model,
                   interval="confidence",
                   n.ahead=n.ahead)#, level,nsim)
  }
  return(list(fit=fit, filter=filter.model, pred=pred))
}

ssplot = function(dat){
  # data
  n.ahead=1
  if(n.ahead==0)
    d = dat$pred
  else{
    y = dat$fit$model$y
    d = data.frame(t=c(1:length(y)), y=y)
    b= data.frame(dat$pred) %>% rename(y=fit) %>% 
      mutate(t=time(dat$pred)) # ignore warning
    d = bind_rows(d,b)
  }
  # 可視化
  library(ggplot2)
  g = ggplot(d) +
    geom_line(aes(x=t, y=y), lwd=1) + 
    geom_ribbon(aes(x=t, ymin=lwr, ymax=upr), fill="blue", alpha=.2)
  print(g)
}

kplot = function(d){
  f <- ggplot(d, aes(x=index, y=obs)) 
  g = f + geom_line() + geom_line(aes(x=index, y=fit), alpha= 1, size=1., colour="blue") + 
    geom_line(aes(x=index, y=upr), size=0.1, colour="blue") +
    geom_line(aes(x=index, y=lwr), size=0.1, colour="blue") +
    #geom_line(aes(x=index, y=Pred), size=0.1, colour="red") +
    geom_ribbon(aes(x=index, ymin=lwr, ymax=upr), fill="blue", alpha=.2)
  print(g)
}

#kplot(d)
#dygraph(d) %>%  dySeries(c("lwr", "fit", "upr"), label = "BTC/USD")
update.fn = function(pars, model) {
  model = SSModel(y ~ SSMtrend(2, Q=list(0, 0)) +
                    SSMarima(ar =artransform(pars[1:2]), 
                             ma=pars[3], Q = exp(pars[4])), H = 0)
  model$T["slope", "slope", 1] <- pars[5]
  return(model)
}

# create model
crmodel = function(n.ahead=20){
  model <- SSModel(y~ SSMtrend(2, Q=list(0, 0)) + 
                     SSMarima(ar=c(0, 0), ma=0, Q=NA), H=0)
  fit.model <- fitSSM(model, inits=rep(.1, 5), 
                      updatefn = update.fn, method="SANN")
  
  filter.model = KFS(fit.model$model, smoothing = "mean")
  
  print(paste("Converged: ", fit.model$optim.out$convergence == 0))
  print(paste("log likelihood:", round(-fit.model$optim.out$value, 3)))
  
  pred = predict(fit.model$model, n.ahead = n.ahead, interval = "confidence", 
                 type="response", level=.95)
  
  return(list(fit=fit.model, filter=filter.model, pred=pred))
}

#=====================================================
# MAIN
#=====================================================
df = read.csv('./examples/bitcoin.csv', sep=",") %>% filter(Weighted.Price!=0)
n = which(df$Date=="2018-01-07")
y = df[n:nrow(df), "Weighted.Price"]

# 状態空間モデルの作成
mod = SSModel(y~SSMtrend(degree=1, Q=list(matrix(NA))),
               H=matrix(NA), distribution = "gaussian")

#mod = SSModel(y~SSMtrend(1, Q=NA), H=NA)          # 1次のモデル
mod = SSModel(y~SSMtrend(2, Q=list(NA, NA)), H=NA) # 二次のモデル
mod = SSModel(y~SSMarima(ar=NULL,ma=NULL, Q=list(NA, NA)), H=NA) # 二次のモデル

inits = numeric(3)

# 予測
#debug(ssplot)
a = crmodel(20)
ssplot(func.kfs(mod, inits, n.ahead = 10))
ssplot(a)
