setwd("D:/data")
data<-read.csv("stockton4.csv",header = TRUE)
attach(data)
data
## cau 1:
#a
plot(livarea,sprice)
#b
model1<-lm(sprice~livarea)
model1
abline(modle1,col="red")
#c
attach(data)
livarea2<-livarea^2
model2<-lm(sprice~livarea2)
summary(model2)
#d
x<-sum((model1$residuals)^2)
x
y<-sum((model2$residuals)^2)
y
#e
plot( sprice,age)

#câu 2 
#a
model3<-lm(sprice~ livarea+ age+beds+baths)
model3
summary(model3)
#b
attach(data)
livarea3<-livarea^2
age3<-age^2
model3<-lm(sprice~livarea3+ age3+beds+baths)
summary(model3)
#c
qt(0.95,df=1498)

## câu 3:
#a
sprice1<-log(sprice)
livarea1<-livare^2
age1<-age^2
model4<-lm(sprice1~livarea+livarea1+ age+age1+beds)
summary(model4)
anova(model4)
#b
#KTC = 95% 
#livarea ----b1 = 10,75
qt(0.975,df=1494)

#livarea1 ----b2 = -5.036e-04
qt(0.975,df=1494) 
# beta2 thuoc [-9.283.10^-3  ; 8.2756.10^-3]
# neu khu vuc sinh song 1 tang/ giam 1 don vi thi gia nha giam /tang  tu -9.283.10^-3  den 8.2756.10^-3  voi khoang tin cay 95%
#c
qt(0.975,df=1494)
qt(0.975,df=1494)



