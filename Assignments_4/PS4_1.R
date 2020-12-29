setwd('C:/Users/ping ping/Desktop/Assignment/Assignment_04_hzp')
library(ggplot2)
library(dplyr)
library(tidyr)
#加载美国爱荷华州五个观测站的降水数据
data4_1 <- read.csv('PRCP.csv',header = T)
data4_1 <- as_tibble(data4_1)


### Boxplot
#逐日的降水有很多0值，因此先将其转化为逐月的降水，再绘制箱线图
data4_11 <- data4_1 %>%
  mutate(y_m = as.character(paste(YEAR,MONTH,15,sep = "-"))) %>%
  mutate(group = paste(y_m,NAME)) %>%
  group_by(group) %>%
  summarize(PRCP_mon =  sum(PRCP,na.rm = TRUE)) %>%
  mutate(name = factor(gsub(" ","",substr(group,11,nchar(group))))) %>%
#https://www.imooc.com/wenda/detail/585314 学习了命令nchar(),计算字符串长度
#https://blog.csdn.net/weixin_43718786/article/details/107614620 学习了命令gsub(),替换字符串中的指定内容
#绘图  
  ggplot(aes(x = name, y = PRCP_mon, fill = name)) +
  geom_boxplot() +
  labs(title="monthly total precipitation from 2005 to 2019", x="City", 
       y="monthly total precipitation(mm)") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  scale_fill_discrete(name="City")
data4_11

### Time series
#将年、月、日组合，转化为时间
data4_12 <- data4_1 %>%
  mutate(DATE = as.Date(paste(YEAR,MONTH,DAY,sep = "-"))) %>%
#绘图,由于数据较多且较为相似，因此将其绘制在多个坐标系中
  ggplot( aes(x=DATE, y=PRCP, color=NAME) ) + 
  geom_line() + 
  labs(title="precipitation from 2005 to 2019", x="Date", y="precipitation(mm)") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) + 
  scale_fill_discrete(name="City") +
  facet_wrap( ~ NAME)
data4_12


### Histogram
#基于与箱线图相同的原因，直方图也使用逐月的降水数据
data4_13 <- data4_1 %>%
  mutate(y_m = as.character(paste(YEAR,MONTH,15,sep = "-"))) %>%
  mutate(group = paste(y_m,NAME)) %>%
  group_by(group) %>%
  summarize(PRCP_mon =  sum(PRCP,na.rm = TRUE)) %>%
  mutate(name = factor(gsub(" ","",substr(group,11,nchar(group))))) %>%
#绘图
  ggplot(aes(x = PRCP_mon, fill = name)) +
  geom_histogram() +
  labs(title="monthly total precipitation from 2005 to 2019", x="monthly total precipitation(mm)", 
       y="frequency") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) +
  scale_fill_discrete(name="City") +
  facet_wrap( ~ name)
data4_13



### Scatter plot 
#散点图使用逐月的降水数据，选择城市为ROCKWELLCITY和SACCITY
data4_14 <- data4_1 %>%
  mutate(y_m = as.character(paste(YEAR,MONTH,15,sep = "-"))) %>%
  mutate(group = paste(y_m,NAME)) %>%
  group_by(group) %>%
  summarize(PRCP_mon =  sum(PRCP,na.rm = TRUE)) %>%
  mutate(name = factor(gsub(" ","",substr(group,11,nchar(group))))) %>%
  select(PRCP_mon,name) %>%
  mutate(a = 1) %>%
  mutate(b = PRCP_mon)
ROC <- data4_14 %>%
  filter(name == "ROCKWELLCITY,IAUS")
SAC <- data4_14 %>%
  filter(name == "SACCITY,IAUS")
a <- ROC[,1]
a[,2] <- SAC[,1]
colnames(a) <- c('ROC','SAC')
lm <- lm(SAC ~ ROC, data = a)
#绘图
plot_point <- a %>%
  ggplot(aes(x=ROC, y=SAC)) +
#在ggplot中添加回归线：https://blog.csdn.net/weixin_40575651/article/details/107575012?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.compare&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.compare
  geom_abline(slope = lm$coefficients[2], intercept = lm$coefficients[1], 
              color = "red", size = 1, alpha = 0.5) +
  geom_point(color="blue") +
  labs(title="monthly total precipitation from 2005 to 2019", x="ROCKWELLCITY", 
                   y="SACCITY") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
plot_point



### Image plot
#绘制今年一月份全球的ndvi（归一化植被指数），数据来源为Giovanni
library(fields)
library(maps) 
library(RNetCDF)
#加载RColorBrewer包，用于绘制绿色的渐变色：https://www.jianshu.com/p/4e6fb1269ece
library(RColorBrewer)
ex.nc     <- open.nc("g4.subsetted.MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI.20200101.180W_90S_180E_90N.nc")
print.nc(ex.nc)
Lat <- var.get.nc(ex.nc, "lat")
Lon <- var.get.nc(ex.nc, "lon")
NDVI <- var.get.nc(ex.nc, "MOD13C2_006_CMG_0_05_Deg_Monthly_NDVI") 
close.nc(ex.nc)
#ndvi没有海洋的数据，此外由于MODIS范围为南北纬60°左右，因此南北极无数据
image.plot(Lon, Lat, NDVI,
           col=brewer.pal(7,"Greens"),
           horizontal=T,useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="ndvi",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("global ndvi in 2020.1"),
      cex.main=1,font.main=2)
map('world',add=T,lwd=0.75,col="black")
box(lwd=2)


# good work









 
