setwd('C:/Users/ping ping/Desktop/Assignment/Assignment_04_hzp')
#数据缺少一部分九月的值和全部十月值，下载这些数据并补充在2281305.csv中
data4_2 <- read.csv('2281305.csv',header = T)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
#整理数据，保留日期（monthly）和月平均温度（℃）
data4_21 <- data4_2 %>%
  mutate(tem = as.numeric(substr(TMP,2,5))) %>%
  filter(tem != 9999) %>%
  mutate(tem = 0.1*tem) %>%
  mutate(date = substr(DATE,1,7) ) %>%
  select(date,tem) %>%
  group_by(date) %>%
  summarize(monthly_tem = mean(tem))

#4.2.1 Construct a time series
TMP <- ts(data4_21$monthly_tem, start=c(2010,1),end = c(2020,8), frequency=12)
plot(TMP)

#4.2.2 Decompose the time series
TMP_components <- decompose(TMP)
plot(TMP_components)
# 绘制直方图
hist(TMP_components$random, prob=TRUE,ylim = c(0,0.4))
# 添加pdf
curve(dnorm(x, mean=mean(TMP_components$random,na.rm=T),
            sd=sd(TMP_components$random,na.rm=T)),
      add=TRUE, col="red")
#Box.test : https://blog.csdn.net/qq_36810398/article/details/88876429
Box.test(TMP_components$random,type='Ljung-Box')
# p-value = 0.01904 < 0.05, 遵循高斯白噪声分布

#4.2.3 ARIMA model
acf(TMP)
pacf(TMP)
#可以发现TMP是平稳的,可以不进行差分

#利用auto.arima()建模
model <- auto.arima(TMP)
model
#得到的模型为ARIMA(0,0,2)(1,1,1)[12]

#该模型的推导过程如下，推导过程与王超同学进行了讨论

#第一组参数(0,0,2)代表去除季节性的ARIMA模型，是满足p = 0:5,q = 0:5,d = 0:2的条件下获得所有模型中，AIC最小的那个,可以通过以下方法验证
#AIC最小的原则来自于知乎，但页面关闭后无法找回原网址了
#p，q取值均为0:5;由于TMP是平稳的，因此d = 0:2,否则要根据差分情况确定d的取值
TMP_adjust <- TMP - TMP_components$seasonal
aic <- matrix(NA,6*3*6,2)
n =1
for (p in 0:5) {
  for (d in 0:2) {
    for (q in 0:5) {
      model1 <- arima(TMP_adjust,order = c(p,d,q))
      aic[n,1] = model1$aic
      aic[n,2] = paste(p,d,q)
      n = n+1
    }
  }
}
min_aic <- which(aic[,1]==min(aic[,1]))
aic[min_aic,]


#第二组参数(1,1,1)[12]代表季节性，是一个周期性循环的时间序列，可以通过以下方法验证
#参考了https://blog.csdn.net/tantaixf/article/details/83148901
#已知季节性的周期为12，因此对原数据进行差分，间隔为12
TMP_components$seasonal
TMP1 <- diff(TMP,12)
acf(TMP1)
pacf(TMP1)
#通过肉眼可以看出，P=1,Q=1，diff()只进行了一次，因此D=1

#4.2.4 Predict
months_forecast  <- 2
months_in_plot   <- 12
forecast <- forecast(model, months_forecast)
forecast

# Plot predictions along with real values
plot(forecast(model, months_forecast), include = months_in_plot , xlab="Time", 
     ylab="temperature",type="b",lwd=2) 


# Sep:
# Predicted value: 29.042 (27.607, 30.478)
# Real value: 28.800
# Relative bias: 
(29.042-28.800)/28.800*100

#Oct
# Predicted value: 26.401 (24.935, 27.868)
# Real value: 25.764
# Relative bias: 
(26.401-25.764)/25.764*100

# good work

