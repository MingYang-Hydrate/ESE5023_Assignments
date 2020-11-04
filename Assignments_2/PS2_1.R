setwd("C:/Users/ping ping/Desktop/Assignment/Assignment_02_hzp")
library(tidyr)
library(dplyr)
library(ggplot2)
## 1.1
Sig_Eqs <- read.table("signif.txt",header = TRUE,sep = "\t",quote = "")
Sig_Eqs <- as_tibble(Sig_Eqs)
## 1.2
##求死亡人数，降序排列
Total_death <- Sig_Eqs %>%
  select(COUNTRY,TOTAL_DEATHS)%>%
  filter(TOTAL_DEATHS != "NA")%>%
  group_by(COUNTRY) %>%
  summarize(total_number = sum(TOTAL_DEATHS)) %>%
  arrange(desc(total_number))
##读前十列
head(Total_death,10)
## 1.3
Sig_Eqs %>%
  select(YEAR,EQ_PRIMARY) %>%
  filter(EQ_PRIMARY >= 6) %>%
  group_by(YEAR) %>%
  summarize(sum = n()) %>%  
  ggplot(aes(x=YEAR, y=sum)) + 
  geom_line()
## 1.4
##创建函数
CountEq_LargestEq <- function(country){
  Country_Data <- Sig_Eqs %>%
    filter( COUNTRY == country)
  a <- Country_Data %>%
    filter(EQ_PRIMARY != "NA") %>%
    filter(EQ_PRIMARY == max(EQ_PRIMARY)) %>%
    mutate(EQ_date = as.character(paste0(YEAR,"-",MONTH,"-",DAY))) %>%
    select(EQ_date)
  result <- c(country,nrow(Country_Data),as.character(a))
  result
}
##导出所有国家
Country <- Sig_Eqs %>%
  select(COUNTRY) %>%
  mutate(xyz = 1) %>%
  group_by(COUNTRY) %>%
  summarize(max(xyz)) %>%
  select(COUNTRY)
Country1 <- c()
for (ii in 1:nrow(Country)) {
  Country1[ii] = as.character(Country[ii,1])
}
##输出结果
result1 <- matrix(NA,155,3)
for (jj in 1:nrow(Country)) {
  result1[jj,] = CountEq_LargestEq(Country1[jj])
}
result1 <- as_tibble(result1)
result1 %>%
  mutate(country = V1,time_EQ = as.numeric(V2),date_EQ = V3) %>%
  select(country,time_EQ,date_EQ) %>%
  arrange(desc(time_EQ))


