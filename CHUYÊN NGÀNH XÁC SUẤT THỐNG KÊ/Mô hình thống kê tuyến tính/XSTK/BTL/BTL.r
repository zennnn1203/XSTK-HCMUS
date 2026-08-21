#=================Import library================#
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(GGally)
library(ggpubr)
library(agricolae)

#=================Load the data=================#
setwd("D:\\secondyear\\HK212\\XSTK\\BTL\\Data cho BTL")
load("flights.rda")
head(flights)
dataset = select(flights, c("year", "month", "day", "carrier", "origin", "dest", 
                            "dep_time", "arr_time","dep_delay", "arr_delay", "distance"))

#=================Data cleaning=================#
#Check NAs data
colSums(is.na(dataset))
#Replace NAs with mean of the column's mean
#dataset$dep_time[is.na(dataset$dep_time)]=mean(dataset$dep_time, na.rm = TRUE) 
#dataset$arr_time[is.na(dataset$arr_time)]=mean(dataset$arr_time, na.rm = TRUE) 
#dataset$arr_delay[is.na(dataset$arr_delay)]=mean(dataset$arr_delay, na.rm = TRUE) 
#dataset$dep_delay[is.na(dataset$dep_delay)]=mean(dataset$dep_delay, na.rm = TRUE) 

#Another strategy: Delete the NAs
dataset = dataset[complete.cases(dataset), ]


#==============Data visualization===============#
#============= I. Chuyen doi bien ==============#

# Xu ly truong hop arr_time < dep_time
dataset$dep_time = dataset$dep_time %/% 100 * 60 + dataset$dep_time %% 100
dataset$arr_time = dataset$arr_time %/% 100 * 60 + dataset$arr_time %% 100
dataset$arr_time[dataset$arr_time < dataset$dep_time] = dataset$arr_time[dataset$arr_time < dataset$dep_time] + 24 * 60

# Chuyen cac bien phan loai thanh cac bien so hoc: carrier, origin, dest
#dataset$carrier = as.factor(dataset$carrier)
#dataset$origin = as.factor(dataset$origin)
#dataset$dest = as.factor(dataset$dest)
#dataset$carrier = as.double(unclass(dataset$carrier))
#dataset$origin =  as.double(unclass(dataset$origin))
#dataset$dest =  as.double(unclass((dataset$dest)))

#============= II. Thong ke mo ta ==============#

# 2.1. Thong ke mau 
# Bien so hoc
stats = c("mean", "median", "sd", "min", "max")
dep_time = c(mean(dataset$dep_time), median(dataset$dep_time), sd(dataset$dep_time), min(dataset$dep_time), max(dataset$dep_time))
arr_time = c(mean(dataset$arr_time), median(dataset$arr_time), sd(dataset$arr_time), min(dataset$arr_time), max(dataset$arr_time))
arr_delay = c(mean(dataset$arr_delay), median(dataset$arr_delay), sd(dataset$arr_delay), min(dataset$arr_delay), max(dataset$arr_delay))
dep_delay = c(mean(dataset$dep_delay), median(dataset$dep_delay), sd(dataset$dep_delay), min(dataset$dep_delay), max(dataset$dep_delay))
statistics = data.frame(stats, dep_time, arr_time, dep_delay, arr_delay)

# Bien phan loai
table(dataset$month)
table(dataset$carrier)
table(dataset$origin)
table(dataset$dest)

#+++++++++++++++++++Plotting++++++++++++++++++++#

#Distribution of numeric variables:
hist(dataset$dep_time)
hist(dataset$arr_time)
hist(dataset$dep_delay, breaks = 200, xlim = c(-100, 300))
hist(dataset$arr_delay, breaks = 200, xlim = c(-100, 300))
hist(dataset$distance)

# Tuong quan du lieu giua bien so hoc va bien phan loan
# dep_time ~ carrier, origin, month
# dep_time ~ carrier
plot01 = ggplot(data = dataset, aes(x = carrier, y = dep_time, fill = carrier)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_time ~ origin
plot02 = ggplot(data = dataset, aes(x = origin, y = dep_time, fill = origin)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_time ~ month
plot03 = ggplot(data = dataset, aes(x = month, y = dep_time, fill = month)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_time ~ dest
plot04 = ggplot(data = dataset, aes(x = dest, y = dep_time, fill = dest)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2)
dep_time_plots = list(plot01, plot02, plot03, plot04)
layout1 = rbind(c(1, 2), c(3, 4))
grid.arrange(grobs = dep_time_plots, layout_matrix = layout1)

#arr_time ~ carrier, origin, month, dest
# arr_time ~ carrier
plot05 = ggplot(data = dataset, aes(x = carrier, y = arr_time, fill = carrier)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_time ~ origin
plot06 = ggplot(data = dataset, aes(x = origin, y = arr_time, fill = origin)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_time ~ month
plot07 = ggplot(data = dataset, aes(x = month, y = arr_time, fill = month)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_time ~ dest
plot08 = ggplot(data = dataset, aes(x = dest, y = arr_time, fill = dest)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2)
arr_time_plots = list(plot05, plot06, plot07, plot08)
layout2 = rbind(c(1, 2), c(3, 4))
grid.arrange(grobs = arr_time_plots, layout_matrix = layout2)

#dep_delay ~ carrier, origin, month, dest
# dep_delay ~ carrier
plot09 = ggplot(data = dataset, aes(x = carrier, y = dep_delay, fill = carrier)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_delay ~ origin
plot10 = ggplot(data = dataset, aes(x = origin, y = dep_delay, fill = origin)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_delay ~ month
plot11 = ggplot(data = dataset, aes(x = month, y = dep_delay, fill = month)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_delay ~ dest
plot12 = ggplot(data = dataset, aes(x = dest, y = dep_delay, fill = dest)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2)
dep_delay_plots = list(plot09, plot10, plot11, plot12)
layout3 = rbind(c(1, 2), c(3, 4))
grid.arrange(grobs = dep_delay_plots, layout_matrix = layout3)

#arr_delay ~ carrier, origin, month, dest
# arr_delay ~ carrier
plot13 = ggplot(data = dataset, aes(x = carrier, y = arr_delay, fill = carrier)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_delay ~ origin
plot14 = ggplot(data = dataset, aes(x = origin, y = arr_delay, fill = origin)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_delay ~ month
plot15 = ggplot(data = dataset, aes(x = month, y = arr_delay, fill = month)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_delay ~ dest
plot16 = ggplot(data = dataset, aes(x = dest, y = arr_delay, fill = dest)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2)
arr_delay_plots = list(plot13, plot14, plot15, plot16)
layout4 = rbind(c(1, 2), c(3, 4))
grid.arrange(grobs = arr_delay_plots, layout_matrix = layout4)

#distance ~ carrier, origin, month, dest
# distance ~ carrier
plot17 = ggplot(data = dataset, aes(x = carrier, y = distance, fill = carrier)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# distance ~ origin
plot18 = ggplot(data = dataset, aes(x = origin, y = distance, fill = origin)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# distance ~ month
plot19 = ggplot(data = dataset, aes(x = month, y = distance, fill = month)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2)
# distance ~ dest
plot20 = ggplot(data = dataset, aes(x = dest, y = distance, fill = dest)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2)
distance_plots = list(plot17, plot18, plot19, plot20)
layout5 = rbind(c(1, 2), c(3, 4))
grid.arrange(grobs = distance_plots, layout_matrix = layout5)

# Tuong quan giua ca du lieu kieu so hoc
# Tat ca cac bien so hoc 
ggpairs(dataset[, 7:11])

# arr_time ~ dep_time
ggpairs(select(dataset, c("dep_time", "arr_time")))

# arr_delay ~ dep_delay
ggpairs(select(dataset, c("dep_time", "arr_time")))

# arr_delay ~ dep_delay ~ distance
ggpairs(select(dataset, c("dep_time", "arr_time", "distance")))

#========================= ANOVA =====================+#
anova_data = select(dataset, c("carrier", "dep_delay"))

# Check anova data
sample_n(anova_data, 10)
anova_data$carrier = as.factor(anova_data$carrier)
levels(anova_data$carrier)

# Order the level of the data
anova_data$carrier = ordered(anova_data$carrier, levels = c("AA", "AS", "B6", "DL", "F9", "HA", "OO", "UA", "US", "VX", "WN"))

# Tinh cac gia tri thong ke - count, mean, sd
summarise(
  group_by(anova_data, carrier),
  count = n(),
  mean = mean(dep_delay),
  sd = sd(dep_delay)
)

# Mean plot

df = data.frame(Mean = c(10.6,2.75,8.39,4.78,10.1,2.59,4.39,9.80,2.74,7.86,13.3),
                sd = c(52.1,20.3,31.4,29.2,41.0,47.2,28.7,33.8,26.0,32.9,30.3),
                carrier = as.factor(c("AA","AS","B6","DL","F9","HA","OO","UA","US","VX","WN")))
ggplot(df, aes(x=carrier, y=Mean, fill = carrier)) + 
  geom_point()+
  geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.2,
                position=position_dodge(0.05))

# Tinh toan phan tich phuong sai
anova_analysis = aov(dep_delay ~ carrier, data = anova_data)

# Tom tat phan tich
summary(anova_analysis)

# Su dung LSD test
print(LSD.test(anova_analysis, "carrier"))

# Hoi quy tuyen tinh
sample_dataset <- dataset
dataset1 = select(sample_dataset, c("month", "dep_time", "arr_time","dep_delay", "arr_delay", "distance"))
m1 <- lm(arr_delay ~ ., data = dataset1)
summary(m1)
dataset2 = select(sample_dataset, c("month","dep_delay", "arr_delay", "distance"))
m2 <- lm(arr_delay ~., data = dataset2)
summary(m2)
anova(m1,m2)
par(mfrow = c(2, 2))
plot(m1)
# Predict arr_delay
dataset$pred_arrdelay <- predict(m1, newdata = dataset)
dataset$residual <- dataset$arr_delay - dataset$pred_arrdelay
residualsquare_sum  = sum((dataset$pred_arrdelay - dataset$arr_delay)^2)
mse = residualsquare_sum/160748
