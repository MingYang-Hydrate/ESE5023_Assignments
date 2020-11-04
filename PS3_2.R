#数据整理
Oxygen_isotopic_omposition <-c(11.10,11.22,11.29,11.49,11.32,11.40,11.71,11.60,11.78,12.05,
10.61,10.88,11.12,11.24,11.43,10.92,11.20,11.30,11.62,11.70,11.70,11.79,11.91,12.15,11.33,
11.41,11.62,12.15,12.30,11.32,11.65,11.96,12.15,11.54,11.89,12.04,10.93,11.01,11.08,11.12,
11.28,11.37,11.35,11.43,11.50,11.57,11.92,11.95,12.01,12.25,12.30,12.39)
Bone <- c(rep("Rib 16",4),rep("Gastralia",6),rep("Dorsal vertebra",10),rep("Femur",4),
          rep("Tibia",5),rep("Metatarsal",4),rep("Phalange",3),rep("Proximal caudal",6),
          rep("Mid-caudal",5),rep("Distal caudal",5))
Bone1 <- as.factor(Bone)
data3_2 <- data.frame(Oxygen_isotopic_omposition,Bone1)
#单因素方差分析
anova_one_way3_2 <- aov(Oxygen_isotopic_omposition ~ Bone1, data = data3_2)
summary(anova_one_way3_2)
TukeyHSD(anova_one_way3_2)
#有99.9%的概率认为这些骨头不是在同一温度下形成的，暴龙不属于恒温动物