setwd("D:/data")
data<-read.csv("cps4_small.csv",header = TRUE)
attach(data)
data
### cau 1: estimate the linear regression and discuss
## cau 1: ve mo hinh hoi quy wage theo edu và nhan xét
#a. tìm beta1 beta2 và nêu ı nghia (wage theo edu )

model1<-lm(wage~educ)
model1

#b. tinh phan du va plot(calculate the least squares and plot them again EDUC)
res<-model1$residuals
plot(educ,res)

#c .estimate seqarate regressions for male,......
#c .uocc luong riêng ... và so sánh R bình nh???n du???c
#doi voi female
data1<-subset(data,female==0)
attach(data1)
model2<-lm(wage~educ)
model2
#doi voi male
data2<-subset(data,female==1)
attach(data2)
model2a<-lm(wage~educ)
model2a
summary(model2a)
#doi voi blacks
data3<-subset(data,blacks==0)
attach(data3)
model2b<-lm(wage~educ)
model2b
summary(model2b)
#doi voi white
data4<-subset(data,blacks==1)
attach(data4)
model2c<-lm(wage~educ)
model2c
summary(model2c)

#d tìm mô hình quadratic
attach(data)
educ2<-educ^2
model3<-lm(wage~educ2)
summary(model3)
##cau2:
#a
model4<-lm(wage~exper)
model4
#ve hinh wage theo exper
plot(exper,wage)
#b  beta2=0 #0
#Bác bo H0 n???u p_value < anpha
attach(data)
model4<-lm(wage~exper)
summary(model4)

confint(model4,level = 0.95)

#c
res1<-model4$residuals
plot(exper,res1)

## cau 3:hoi quy da
#a
lwage<-log(wage)
model5<-lm(lwage~educ+exper+hrswk)
summary(model5)
# KTC
confint(model5, level = 0.95)[2,2]
#b tang 1 nam di hoc làm ty le luong giam 10% , lay doi thuyet nho hon 10%
# beta2 >= 0.1 & beta2 <0.1
teduc<-(0.090306 - 0.1)/0.006078
teduc
qt(0.9, 996)
model5$coefficients[2]

#c tìm ktc 90% cho Beta4(cho wage tang lên phu thuoc vào so gio làm viec moi tuan (uoc luong cho hrswk)
## hàm tìm ra ktc (tìm uoc luong cho he so hoi quy)
confint(model5,lever = 0.95) # khoang uoc luong
model5$coefficients
#d
attach(data)
lwage<-log(wage)
educ2<-educ*exper
exper2<-educ^2
hrswk2<-exper^2
model6<-lm(wage~educ2+exper2+hrswk2)
summary(model6)









