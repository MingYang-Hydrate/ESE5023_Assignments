#数据整理
Elevation <- c(180, 305, 381, 488, 549, 640, 762, 883)
Temperature <- c(13.3, 12.2, 13.3, 10.0, 8.3, 9.4, 8.3, 7.2)
data3_4 <- data.frame(Elevation, Temperature)
#绘散点图及线性回归
plot(Temperature ~ Elevation, data = data3_4)
Atmospheric_Lapse_Rate <-
  lm(Temperature ~ Elevation, data = data3_4)
abline(Atmospheric_Lapse_Rate, col = "red")
summary(Atmospheric_Lapse_Rate)
#得到气温直减率为-9.312℃/Km

# good work
