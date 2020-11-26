setwd('C:/Users/ping ping/Desktop/Assignment/Assignment_03_hzp')
data3_5 <- read.csv('3.5.csv')
colnames(data3_5) <- c("order","Nebula","Velocity","Distance")
#3.5.1
plot(Distance ~ Velocity ,data = data3_5)
#可以发现recession velocity与distance近似成正相关
#3.5.2
model3_5 <- lm(Distance ~ Velocity , data = data3_5)
coef(model3_5)
abline(model3_5, col = "red")
#3.5.3
#假设地球的速度为v0，宇宙诞生的时间为k，那么地球与宇宙大爆炸奇点之间的距离d0 = k * v0
#某星云i与地球之间的相对速度为vi，那么该星云i与奇点之间的距离di = k *（v0 + vi）
#因此地球与星云i之间的距离d = di- d0 = k * vi
#因此，斜率k为宇宙诞生的时间，截距为0
model3_5_2 <- lm(Distance ~ 0 + Velocity , data = data3_5)
#宇宙诞生的时间
summary(model3_5_2)$coefficients[1, 1] * 1000000 * 30.9 * 1000000000000 /
  60 / 60 / 24 / 365


#3.5.4
#显然，测距方法越准确，回归系数就越精确

# good work
