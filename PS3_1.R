#数据整理
Unseeded_rainfall <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0);
Seeded_rainfall <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1) 
f <- factor(rep(c("Unseeded_rainfall","Seeded_rainfall"), each=26)) 
#https://baijiahao.baidu.com/s?id=1608964775612234515&wfr=spider&for=pc
Rainfall <- c(Unseeded_rainfall,Seeded_rainfall)
data3_1 <- data.frame(Rainfall,f)
#3.1.1
boxplot(Rainfall~f,data3_1,ylim = c(0,800))
#3.1.2
par(mfrow=c(1,2))
#https://blog.csdn.net/never0822/article/details/81384235
hist(Unseeded_rainfall)
hist(Seeded_rainfall)
#可以发现两组数据均不是正态分布，因此使用ANOVA
anova_one_way3_1 <- aov(Rainfall ~ f,data = data3_1)
summary(anova_one_way3_1)
#0.0511>0.05，因此有90%的概率认为这两组降水数据存在差异