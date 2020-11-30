setwd('C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp')
library("sp")
library("rgdal")
library("sf")
library("raster")
library("maps")

#5.1.1
#load Solar radiation,2.5 minutes
#由于有12个tif文件，因此用列表批量读取数据，再求平均值
#参考了https://bbs.csdn.net/topics/394380528
rlist_srad=list.files("C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp/wc2.1_2.5m_srad", pattern="tif$", full.names=T) 
#给用于循环的b一个初始值
a <- raster("C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp/wc2.1_2.5m_srad/wc2.1_2.5m_srad_01.tif")
b <- a
for(i in rlist_srad){ 
  c <- raster(i) 
  b = c + b
}
#减去之前设置的初始值
b = b-a
#求平均值
srad <- b/12

#剩余两个变量和上述过程基本一致
#load Precipitation, 2.5 minutes
rlist_prec=list.files("C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp/wc2.1_2.5m_prec", pattern="tif$", full.names=T) 
a <- raster("C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp/wc2.1_2.5m_prec/wc2.1_2.5m_prec_01.tif")
b <- a
for(i in rlist_prec){ 
  c <- raster(i) 
  b = c + b
}
b = b-a
#降水不需要求平均值，保留年降水即可
prec <- b

#load Wind speed, 2.5 minutes
rlist_wind=list.files("C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp/wc2.1_2.5m_wind", pattern="tif$", full.names=T) 
a <- raster("C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp/wc2.1_2.5m_wind/wc2.1_2.5m_wind_01.tif")
b <- a
for(i in rlist_wind){ 
  c <- raster(i) 
  b = c + b
}
b = b-a
wind <- b/12

#5.1.2
China_map <- readOGR("C:/Users/ping ping/Desktop/Assignment/Assignment_05_hzp/China_map", "bou2_4p")

#plot Solar radiation,2.5 minutes
#选择大致为中国的范围
Crop_box <- c(75,140,15,55)
srad_CN <- crop(srad, Crop_box)
#添加掩膜
#mask函数来自王超同学
srad_CN_m <- mask(srad_CN,China_map)
#绘图
plot(srad_CN_m,main="annual mean solar radiation in China")
maps::map('world',add=T)

#plot Precipitation, 2.5 minutes
prec_CN <- crop(prec, Crop_box)
#添加掩膜
prec_CN_m <- mask(prec_CN,China_map)
#绘图
plot(prec_CN_m,main="annual total precipitation in China")
maps::map('world',add=T)

#plot Wind speed, 2.5 minutes
wind_CN <- crop(wind, Crop_box)
#添加掩膜
wind_CN_m <- mask(wind_CN,China_map)
#绘图
plot(wind_CN_m,main="annual mean wind speed in China")
maps::map('world',add=T)


#5.1.3 
#从网上得到的资料显示，发电风速的阈值在3m/s-4m/s左右
wind_CN_m1 <- wind_CN_m
#寻找风力大于3m/s的区域
wind_CN_m1[which(wind_CN_m1@data@values < 3)] <- NA
#绘图
plot(wind_CN_m1,main ='Possible sites for wind farms in China')
maps::map('world',add=T)



#5.1.4
srad_CN_m1 <- srad_CN_m
#光伏发电站的设置为年降雨量小于400ml，太阳辐射大于16000kJ/m^2/day
srad_CN_m1[which(prec_CN_m@data@values > 400|srad_CN_m1@data@values < 16000)] <- NA
#绘图
plot(srad_CN_m1,main ='Possible sites for photovoltaics farms in China')
maps::map('world',add=T)

 




