library(corrplot)
library(leaps)
library(lmtest)
library(factoextra)
library(devtools)
library(ggbiplot)
library(VIM)

#read data
setwd("C:/Users/tuanvl/Desktop/ThS/Ex6")
dat=read.table("ozone.txt",header=TRUE)
str(dat)

#dataprocessing_dummies
dat$eath=0
dat$eath[dat$vent=="Est"]=1
dat$west=0
dat$west[dat$vent=="Ouest"]=1
dat$south=0
dat$south[dat$vent=="Sud"]=1
dat$rain=0
dat$rain[dat$pluie=="Pluie"]=1
dat$vent=NULL
dat$pluie=NULL

#missing value
aggr(dat[1:15],sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)

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
dat=dat[-c(22,30,54,55,56,57,79,80,81),]

#visualize
par(mfrow=c(1,1))
corrplot(cor(dat), method="number")
nrow(dat)

#PCA
dat2=dat[,-c(1)]
pca=prcomp(dat2, scale = TRUE, center = TRUE)
summary(pca)
fviz_eig(pca)
y=subset(dat,select=c(maxO3))
pca_set=data.frame(y,pca$x[,1:14])
last_mod=lm(dat$maxO3~PC1+PC2+PC5,data=pca_set)
summary(last_mod)


par(mfrow=c(2,2))
plot(last_mod)

#independent_check
lag.plot(last_mod$residuals)
car::durbinWatsonTest(last_mod)

#stability_check
bptest(last_mod)

#normal_check
shapiro.test(last_mod$residuals)

pca
fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE   
)
#ggbiplot(pca,choice=c(3,4))
