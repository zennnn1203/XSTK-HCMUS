library(corrplot)
setwd("C:/Users/tuanvl/Desktop/ThS/Ex6")

#read_data
ozone=read.table("ozone.txt",header=TRUE)
str(ozone)
n=nrow(ozone)
#data_processing
ozone$eath=0
ozone$eath[ozone$vent=="Est"]=1
ozone$west=0
ozone$west[ozone$vent=="Ouest"]=1
ozone$south=0
ozone$south[ozone$vent=="Sud"]=1
ozone$rain=0
ozone$rain[ozone$pluie=="Pluie"]=1
ozone$vent=NULL
ozone$pluie=NULL

#multico
cor(ozone)
corrplot(cor(ozone),method = "number")

#Base_model
mod0=lm(maxO3~1,data = ozone)
mod1=lm(maxO3~.,data = ozone)
mod_start=lm(maxO3~Ne9+maxO3v,data = ozone)

#stepwise
bestmod=step(mod_start,scope = list(lower = mod_start,upper = mod1),direction = "forward",k=log(n))
summary(bestmod)
