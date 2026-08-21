#================= Import library ================#
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(GGally)
library(ggpubr)
library(agricolae)
library(car)
library(nortest)

setwd("E:/University/Second year/XSTK/BTL")
dataset = read.csv("E:/University/Second year/XSTK/BTL/CCPP/Folds5x2_pp.csv")
colSums(is.na(dataset))
TKM = c("mean", "median", "sd", "min", "max")
AT = c(mean(dataset$AT), median(dataset$AT), sd(dataset$AT), min(dataset$AT), max(dataset$AT))
V = c(mean(dataset$V), median(dataset$V), sd(dataset$V), min(dataset$V), max(dataset$V))
AP = c(mean(dataset$AP), median(dataset$AP), sd(dataset$AP), min(dataset$AP), max(dataset$AP))
RH = c(mean(dataset$RH), median(dataset$RH), sd(dataset$RH), min(dataset$RH), max(dataset$RH))
PE = c(mean(dataset$PE), median(dataset$PE), sd(dataset$PE), min(dataset$PE), max(dataset$PE))
sta = data.frame(TKM, AT, V, AP, RH,PE)

hist(dataset$AT,labels=T)
hist(dataset$V,labels=T)
hist(dataset$AP,labels=T)
hist(dataset$PE,labels=T)
ggpairs(dataset[, 1:5])


#=================== Linear Regression model ===============#

m1 <- lm(PE ~ ., data = dataset)
summary(m1)
m2 <- lm(PE ~ . - AP, data = dataset)
summary(m2)
anova(m1, m2)
par(mfrow = c(2,2))
plot(m1)
dataset$pred_PE = predict(m1, newdata = dataset)
dataset$residual = dataset$PE - dataset$pred_PE
residualsquare_sum = sum((dataset$pred_PE - dataset$PE)^2)
mse = residualsquare_sum/9568

