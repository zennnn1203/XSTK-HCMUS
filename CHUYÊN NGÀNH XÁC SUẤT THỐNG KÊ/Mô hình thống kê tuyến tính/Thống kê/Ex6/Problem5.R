install.packages("leaps")
install.packages("stagewise")
library(leaps)
#read_data
setwd("C:/Users/tuanvl/Desktop/ThS/Ex6")
fire=read.csv("fire.csv",header=TRUE)
str(fire)

fire$y=log(fire$Fire)
fire$Fire=NULL
#All_model
full=regsubsets(y~.,data=fire,nvmax=3,nbest=3)
smfull=summary(full)

#Selection_model
par(mfrow=c(2,2))
plot(smfull$rsq, ylab="R-squared",type = "b", pch = 16)
plot(smfull$adjr2, ylab="R-adj",type = "b", pch = 16)
plot(smfull$cp, ylab="Cp",type = "b", pch = 16)
plot(smfull$bic, ylab="BIC",type = "b", pch = 16)
coef(full,4)
bestmod=lm(y~X_2+X_3,data=fire)
summary(bestmod)
par(mfrow=c(2,2))
plot(bestmod)

par(mfrow=c(1,1))
#Tinh doc lap
lag.plot(bestmod$residuals)
car::durbinWatsonTest(bestmod)
#Tinh on dinh
bptest(bestmod)
#Tinh chuan
shapiro.test(bestmod$residuals)
