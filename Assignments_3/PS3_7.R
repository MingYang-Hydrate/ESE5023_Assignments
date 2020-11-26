setwd('C:/Users/ping ping/Desktop/Assignment/Assignment_03_hzp')
library(ggplot2)
library(dplyr)
library(tidyr)

#Cu为某铜尾矿库0.1m和1m矿渣中的铜浓度
Cu <- read.csv('Cu.csv',header = T,encoding = "UTF-8")
colnames(Cu) <- c('0.1m','1m')

#discharge为sac city水文观测站的北浣熊河径流数据和该地的降雨量数据
discharge <- read.csv('discharge.csv',header = T)

#PRCP为rockwell city的降雨量数据
PRCP <- read.csv('ROCKWELL CITY.csv')

#数据的预处理
#discharge
discharge <- as_tibble(discharge)
discharge1 <- discharge %>%
  filter(year >= 2008) %>%
  select(Dischrge.ft3.s , PRCP.inch.SAC.CITY)
#PRCP
PRCP1 <- PRCP %>%
  mutate(year = as.numeric(substr(DATE,1,4))) %>%
  filter(year >= 2008) %>%
  select(PRCP) %>%
  mutate(f = "rockwell")

#3.7.1 铜尾矿库0.1m和1m深的矿渣中，铜浓度有差别吗
par(mfrow=c(1,2))
hist(Cu[,1])
hist(Cu[,2])
t.test(Cu[,1] , Cu[,2])
#可以认为0.1m和1m深的Cu浓度无差别

#3.7.2 sac city和同纬度的rockwell city的降雨有差别吗
#对数据进一步整理
discharge2 <- discharge1 %>%
  select(PRCP.inch.SAC.CITY) %>%
  mutate(f = "sac")
s_r <- matrix()
s_r <- discharge2
s_r[4384:8728,] <- PRCP1[1:4345,]
colnames(s_r) <- c('PRCP','f')
s_r1 <- s_r %>%
  mutate(f = as.factor(f))
#方差分析
anova_one_way3_7 <- aov(PRCP ~ f,data = s_r)
summary(anova_one_way3_7)
#可以认为两者降水无显著差异

#3.7.3 sac city的降雨量与北浣熊河径流流量之间存在线性关系吗
#两组数据的直方图
par(mfrow=c(1,2))
hist(discharge1$Dischrge.ft3.s)
hist(discharge1$PRCP.inch.SAC.CITY)
#散点图及线性拟合
plot(Dischrge.ft3.s ~ PRCP.inch.SAC.CITY,data = discharge1)
fit3_7 <- lm(Dischrge.ft3.s ~ PRCP.inch.SAC.CITY,data = discharge1)
summary(fit3_7)
abline(fit3_7,lwd = 2, col = "red")
#线性关系并不是很好

# good work 
