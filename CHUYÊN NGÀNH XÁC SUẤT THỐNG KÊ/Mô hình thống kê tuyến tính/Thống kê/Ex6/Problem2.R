library(readxl)
library(corrplot)
library(leaps)
library(lmtest)
#read_data
data2=read_excel("C:\\Users\\tuanvl\\Desktop\\ThS\\Ex6\\data2.xls")
str(data2)

#multicollinearity
corrplot(cor(data2), method = "number")
corrplot(cor(data2[c("y","x2","x7","x10","x11")]),method="number")

#scope_model
mod0=lm(y~1,data=data2)
mod1=lm(y~.-x2,data=data2)

#Stepwise_backward
bestmod=step(mod1)
summary(bestmod)
bptest(bestmod)

par(mfrow=c(2,2))
plot(bestmod)


allmod=regsubsets(y~.,data=data2,nvmax=13,nbest=1)
sma=summary(allmod)
plot(sma$cp,type="b")
plot(sma$rsq,type="b")
plot(sma$bic,type="b")
plot(sma$adjr2,type="b")
coef(allmod,5)
coef(allmod,3)
