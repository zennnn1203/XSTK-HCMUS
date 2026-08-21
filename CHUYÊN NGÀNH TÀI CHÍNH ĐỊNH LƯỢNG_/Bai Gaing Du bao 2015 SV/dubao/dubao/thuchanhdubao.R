setwd("D:/dubao")
data<-read.csv("DataExcel.csv",header=TRUE)
data
data$T1
data$KT
str(data) ## mo ta tong quat ve kieu du lieu tung bien
attach(data)
T1
KT
KV
##### mo ta du lieu 
### voi bien dinh tinh 
### bien dinh tinh GT(gioi tinh)
#bang tan so- table(tenbien)
bangGT<-table(GT)
bangGT
## nhin vao bang tan so, ta thay so hoc sinh nam va so hoc sinh nu dong deu nhau
## bien dinh tinh KT(khoi thi)
bangKT<-table(KT)
bangKT
# dung do thi quat tron - pie(bangtanso)
pie(bangGT)

# dung do thi cot - barplot(bangtanso)
barplot(bangGT)

barplot(table(KT))
### mo ta 2 bien dinh tinh
## bien GT(gioi tinh)va bien KT(khoi thi)
## bang tan so dong thoi
bangGTKT<-table(GT,KT)
bangGTKT
# nhan xet : o cac khoi thi tu nhien: da so trong khoi A, nam chiem nhieu hon nu
# dung do thi dang cot
barplot(bangGTKT)
barplot(bangGTKT,beside = TRUE)

#### bien dinh luong
##bien T1(diem Toan HK1)
## mo ta bang so
summary(T1)
mean(T1)
min(T1)
max(T1)
quantile(T1,probs = 0.25)
quantile(T1,probs = 0.5)
quantile(T1,probs = 0.75)
x=c(2,3,5,7,9,10,11)
quantile(x,probs = 0.25)
var(T1)
sd(T1)
### mo ta bang do thi
#histogram
hist(T1)
hist(T1,freq = FALSE)
## do thi hinh hop - boxplot
boxplot(T1)
boxplot(V1)
##### hai bien dinh luong
## bien T1, T6
plot(T1,T6)


















