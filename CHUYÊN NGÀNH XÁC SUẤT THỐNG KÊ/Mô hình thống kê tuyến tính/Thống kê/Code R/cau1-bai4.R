library(alr4)
y=lm(BMI18 ~ HT2 + WT2 + HT9 + WT9 + ST9, data = BGSgirls)
summary(y)
anova(y)
