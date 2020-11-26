#数据整理
Zinc <- c(185,189,187,181,150,176,171,174,202,171,207,125,189,179,163,174,184,186,210,139,172,198,177)
If_Pregnant <- as.factor(c(rep('Pregnant',18),rep('Nonpregnant',5)))
If_Vegetarians <- as.factor(c(rep('nonvegetarians',6),rep('vegetarians',17)))
data3_3 <- data.frame(Zinc,If_Pregnant,If_Vegetarians)
#方差分析
anova_one_way3_3 <- aov(Zinc ~ If_Vegetarians,data = data3_3)
summary(anova_one_way3_3)
anova_two_way3_3 <- aov(Zinc ~ If_Pregnant + If_Vegetarians, data = data3_3)
summary(anova_two_way3_3)
#可以认为两种孕妇体内锌含量无显著区别

# good work
