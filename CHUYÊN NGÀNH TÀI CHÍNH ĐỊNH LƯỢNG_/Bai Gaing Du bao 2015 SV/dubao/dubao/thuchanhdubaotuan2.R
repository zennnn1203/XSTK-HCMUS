### cai dat thu muc lam viec
### sao chep file du lieu vao thu muc lam viec
### doi dinh dang file.xlsx ve dang file.csv
####cau 1 luot ke cac bien dinh tinh, dinh luong
## dinh tinh 
## dinh luong
####cau 2: doi voi bien bedsroom
##a) tao bang tan so va nhan xet
##b) ve do thi quat tron cho bien bedsroom
####cau 3: doi voi bien baths
##a) tao bang tan so va nhan xet
##b) ve do thi quat tron cho bien baths
####cau 4: doi voi bien bedsroom va baths
##a) tao bang tan so va nhan xet
##b) ve do thi quat tron cho bien bedsroom va baths
####cau 5: doi voi bien price
##a) Tim cac so do Min, Max, Q1,Q2,Q3, mean
##b) Ve do thi tan suat cho bien price va nhan xet
##c) Ve do thi boxplot cho bien price
####cau 6: doi voi bien spft
##a) ve do thi tan so va nhan xet
##b) Ve boxplot va ket luan sqft co outlier hay khong
#### cau 7: doi voi price va sqft
## Ve do thi diem (scatter plot) va nhan xet ve su tuong quan giua 2 bien

setwd("D:/dubao")
data<-read.csv("br2.csv",header=TRUE)
data

### cau 1: Cac bien dinh tinh, dinh luong
## dinh tinh: 
## dinh luong: Age, Baths, Bedrooms, Fireplace, Owner,Pool, price,sqft,Traditional, Fireplace ,Waterfront
### cau 2:doi voi bien Bedrooms
data$Bedrooms
data$Baths
str(data) ## mo ta tong quat ve kieu du lieu tung bien
attach(data)
#a)
bangBedrooms<-table(Bedrooms)
bangBedrooms
## nhin vao bang tan so, so Bedrooms không dong deu nhau
#b) Ve do thi quat tron
pie(bangBedrooms)
###cau 3:doi voi bien 
##a)
BangBaths<-table(Baths)
BangBaths
##b
barplot(BangBaths)

###Cau 4
##a
BangBeB<-table(Bedrooms,Baths)
BangBeB
##b
barplot(BangBeB,beside=TRUE)
###cau 5: doi voi bien price
##a) Tim cac so do Min, Max, Q1,Q2,Q3, mean
summary(price)
mean(price)
min(price)
max(price)
quantile(price,probs = 0.25)
quantile(price,probs = 0.5)
quantile(price,probs = 0.75)
##b) Ve do thi tan suat cho bien price va nhan xet