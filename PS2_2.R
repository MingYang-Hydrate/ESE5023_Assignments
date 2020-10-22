setwd("C:/Users/ping ping/Desktop/Assignment/Assignment_02_hzp")
library(tidyr)
library(dplyr)
library(ggplot2)
Original_data <- read.csv("2281305.csv",T)
Original_data <- as_tibble(Original_data)
Original_data %>%
  select(WND,DATE) %>%
  mutate(WND_data = as.numeric(substr(WND,9,12))) %>%   
  #WND_data为风速
  mutate(WND_control = as.numeric(substr(WND,14,14))) %>%  
  ##WND_control为风速的质量控制
  mutate(month = substr(DATE,1,7)) %>%            
  ##month只保留年和月
  filter(WND_control == 0 |WND_control == 1|WND_control ==4|WND_control ==5)%>%
  group_by(month) %>%
  summarize(OBS_wind_speed = mean(WND_data)) %>%
  mutate(OBS_time = as.Date(paste0(month,"-01"))) %>%  
  ##将字符串month转化为时间
  select(OBS_time,OBS_wind_speed) %>%  
  ggplot(aes(x=OBS_time, y=OBS_wind_speed)) + 
  geom_line()



