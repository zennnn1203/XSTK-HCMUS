setwd("D:/Data")
data<-read.csv("star.csv",header=T)
attach(data)
plot(mathscore,readscore)
cor(mathscore,readscore)
model<-lm(readscore~mathscore)
model
abline(model)
summary(model)
model$residuals
hist(model$residuals,breaks = 30)
#### tim mo hinh noi quy read=betal0 + betal.mathscore2
mathscore2<-mathscore^2
mathscore2
model2<-lm(readscore~mathscore2)
model2
plot(mathscore2,readscore)
abline(model2)
data

data1<-subset(data,boy==1)
data1
attach(data1)


###BAI TAP
setwd("D:/Data")
data<-read.csv("br2.csv",header=T)
attach(data)
data
#A>
data1<-subset(data,Traditional==1) 
data1
attach(data1) 
plot(price,data1)
plot(price,Traditional)
cor(price,Traditional)
model<-lm(price~data1)
model
abline(model)
# B>
summary(model)
model$residuals
hist(model$residual,breaks = 30 )
#C>Traditional1<-Traditional^2
Traditional1
model2<-lm(price~Traditional1)
model2
lm(price~Traditional1)
#D>
plot(price,Traditional)
plot(price,Traditional1)
abline(model)
abline(model2)
#E>
price2<-price^2
price2
model3<lm(price~price^2)
model3







           