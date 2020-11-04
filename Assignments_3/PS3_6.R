library(MASS)
library(leaps)
data(cpus)
data3_6 <- cpus
#分组（训练组和测试组）
sample_index <- sample(nrow(data3_6),nrow(data3_6)*0.80)
cpus_train <- data3_6[sample_index,]
cpus_test  <- data3_6[-sample_index,]

#3.6.1
subset_result3_6 <- regsubsets(
  perf ~ syct + mmin + mmax + cach +
    chmin + chmax,
  data = cpus_train,
  nbest = 2,
  nvmax = 6
)
plot(subset_result3_6, scale = "bic")
nullmodel = lm(perf ~ 1, data = cpus_train)
fullmodel = lm(perf ~ syct + mmin + mmax + cach +
                 chmin + chmax, data = cpus_train)
model_step_b <- step(fullmodel, direction = 'backward')
model_step_f <-
  step(
    nullmodel,
    scope = list(lower = nullmodel, upper = fullmodel),
    direction = 'forward'
  )

model_step_s <-
  step(
    nullmodel,
    scope = list(lower = nullmodel, upper = fullmodel),
    direction = 'both'
  )
#因此最佳子集回归为syct，mmin，mmax，cach和chmax

#3.6.2
#线性拟合
trainmodel3_6  <- lm(perf ~ syct+ mmin + mmax + cach +
                    chmin + chmax, data=cpus_train)
summary(trainmodel3_6)
#应用于测试组
cups_predict <- predict(trainmodel3_6 ,cpus_test)
#拟合效果
plot(cpus_test$perf, cups_predict)
cor(cpus_test$perf, cups_predict)
mean(cpus_test$perf)
mean(cups_predict)
(mean(cups_predict) - mean(cpus_test$perf))/
  mean(cpus_test$perf)*100

