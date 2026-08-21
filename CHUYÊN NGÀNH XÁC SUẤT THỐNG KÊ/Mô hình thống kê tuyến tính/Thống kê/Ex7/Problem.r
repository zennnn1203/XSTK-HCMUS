library(corrplot)
library(leaps)
library(lmtest)
library(tseries)
library(car)
#read data
getwd()
dat=read.csv("wine.csv",header=TRUE)
str(dat)

#outliers
outliers <- function(x){
  # 1st and 3rd quantiles
  q75 = quantile(x, 0.75)
  q25 = quantile(x, 0.25)
  IQR = q75-q25
  # lower bound
  lower_bound = q25 - 3 * IQR
  # upper bound
  upper_bound = q75 + 3 * IQR
  # outliers
  outlier_ind <- which(x < lower_bound | x > upper_bound)
  if (length(outlier_ind) == 0){
    return (0)
  }
  return(outlier_ind)
}

par(mfrow=c(1,1))
boxplot(dat)
apply(dat,2,outliers)
dat=dat[-c(4,198,127,128,15,16,10,12,18,20,143,145,199,14,18,82,87,46,96,132,133),]
#dat=dat[-c(10,12,18,20,34,36,40,41,43,58,155,157,156,164,165,152,170,182),]
#Multicollinearity check
library(car)
vif(lm(dat$pH ~ ., data = dat))

#visualize
par(mfrow=c(1,1))
corrplot::corrplot(cor(dat), method="number")

#Backward selection _ BIC
modAll <- lm(formula =dat$pH ~., data = dat)
summary(modAll)
n=nrow(dat)
last_mod <- step(modAll, direction = 'backward', k = log(n))
summary(last_mod)
par(mfrow=c(2,2))
plot(last_mod)

#independent_check
lag.plot(last_mod$residuals)
durbinWatsonTest(last_mod)

#stability_check
library(lmtest)
bptest(last_mod)

#normal_check
shapiro.test(last_mod$residuals)
#jarque.bera.test(last_mod$residuals)
