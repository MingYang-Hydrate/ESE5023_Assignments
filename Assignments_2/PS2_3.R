setwd("C:/Users/ping ping/Desktop/Assignment/Assignment_02_hzp")
library(tidyr)
library(dplyr)
library(ggplot2)
data <- read.csv("SAC CITY.csv",T)
#3.1 数据的预处理
data1 <- data %>%
  filter(PRCP >= 0)%>%
  mutate(date1 = as.Date(DATE))
#3.2 
data1  %>%  
  ggplot(aes(x=date1, y=PRCP)) + 
  geom_line()  
#3.3
##最大降水量与所在的日期
data1 %>%
  select(date1,PRCP) %>%
  filter(PRCP == max(PRCP))
##年平均降水量
Total_PRCP <- data1 %>%
  mutate(YEAR = as.numeric(substr(DATE,1,4))) %>%
  group_by(YEAR) %>%
  summarize(total_PRCP = sum(PRCP)) 
Total_PRCP %>%                            
  summarize(mean(total_PRCP))
##年平均降水天数
Total_number <- data1 %>%
  filter(PRCP > 0) %>%
  mutate(YEAR = as.numeric(substr(DATE,1,4))) %>%
  mutate(frequency = 1) %>%
  group_by(YEAR) %>%
  summarize(total_number = sum(frequency)) 
Total_number %>%                         
  summarize(mean(total_number))
##用每年降水天数和年平均降水量绘图
left_join(Total_PRCP,Total_number,by="YEAR")  %>%  
  ggplot(aes(x=total_PRCP, y=total_number)) + 
  geom_point()  

 #good work
