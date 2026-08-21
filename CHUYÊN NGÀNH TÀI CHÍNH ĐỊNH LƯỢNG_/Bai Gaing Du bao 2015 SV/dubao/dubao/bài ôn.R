setwd("D:/dubao")
data<-read.csv("br2.csv",header = TRUE)
data
data$Bedrooms
data$Baths
data$price
data$sqft
str(data) ## mo ta tong quat ve kieu du lieu tung bien
attach(data)
price
sqft
#1)doi voi bedrooms
#a)ve bang tan so Bedrooms
bangBedrooms<-table(Bedrooms)
bangBedrooms
#b)v??? do thi quat tron bedroom
pie(bangBedrooms)
#2) doi vs Baths 
#a)ve bang tan so Baths 
bangBaths<-table(Baths)
bangBaths 
#do thi quat tron
pie(bangBaths)
####cau 5: doi voi bien price
##a) Tim cac so do Min, Max, Q1,Q2,Q3, mean
##b) Ve do thi tan suat cho bien price va nhan xet
##c) Ve do thi boxplot cho bien price

#a)
summary(price)
mean(price)
min(price)
max(price)
quantile(price,probs = 0.25)
quantile(price,probs = 0.5)
quantile(price,probs = 0.75)
#b)ve do thi tan so cho bien price 
bangprice<-table(price)
bangprice
#c) do th boxplot
boxplot(price)

####cau 6: doi voi bien spft
##a) ve do thi tan so va nhan xet
bangsqft<-table(sqft)
bangsqft
##b) Ve boxplot va ket luan sqft co outlier hay khong
boxplot(sqft)

#### cau 7: doi voi price va sqft
## Ve do thi diem (scatter plot) va nhan xet ve su tuong quan giua 2 bien
plot(price,sqft)


########## PHAN HOI QUY

setwd("D:/dubao")
data<-read.csv("csp4_small.csv",header = TRUE)
data
attach(data)

##cau 1
#a)
modle1<-lm(wage~educ)
model1
#b)
res<-model1$residuals
plot(educ,res)
#c)
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
#doi voi black
data3<-subset(data,black==0)
attach(data3)
model2b<-lm(wage~educ)
model2b
#doi voi while
data4<-subset(data,black==1)
attach(data4)
model2c<-lm(wage~educ)
model2c
#d)
attach(data)
educ2<-educ^2
model3<-lm(wage~educ2)
summary(model3)

##cau 2
#a)
modle4<-lm(wage~exper)
model4
plot(exper,wage)
#b)
attach(data)
model4<-lm(wage~exper)
summary(model4)

confint(model4,level = 0.95)
#c)
res1<-model1$residuals
plot(exper,res)

##cau 3
#a)
lwage<-logwage
model5<-lm(lwage~educ+exper+hrswk)
summary(model5)
#d)
attach(data)
educ2<-educ*exper
exper2<-educ^2
hrswk2<-exper^2
model6<-lm(lwage~educ2+exper2+hrswk2)
summary(model6)

