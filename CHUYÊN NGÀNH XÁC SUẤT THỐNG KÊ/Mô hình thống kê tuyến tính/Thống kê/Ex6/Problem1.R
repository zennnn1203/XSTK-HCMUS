install.packages("readxl")
library(readxl)

#read data
data1=read_excel("C:\\Users\\tuanvl\\Desktop\\ThS\\Ex6\\data1.xls")
data1
str(data1)

mod0=lm(data1$y~1,data=data1) #non
mod1=lm(data1$y~.,data=data1) #all

#StepWise_backward
bestmod=step(mod1)
summary(bestmod)
