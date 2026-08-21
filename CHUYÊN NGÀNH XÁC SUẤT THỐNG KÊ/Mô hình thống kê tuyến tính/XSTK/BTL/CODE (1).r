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

#================= Load the data =================#
setwd("E:/University/Second year/XSTK/BTL")
load("flights.rda")
head(flights, 3)

dataset = select(flights, c("year", "month", "day", "carrier", "origin", "dest", 
                            "dep_time", "arr_time","dep_delay", "arr_delay", "distance"))
head(dataset, 3)
#================= Data cleaning =================#
# Check NAs data
colSums(is.na(dataset))
# Percentage of NAs(%)
100 - sum(complete.cases(dataset)) / length(dataset$year) * 100

# Clean NAs data
dataset = dataset[complete.cases(dataset), ]
# Check NAs data again
colSums(is.na(dataset))

#============= Data visualization ===============#
#============= I. Modify variables ==============#

# Convert dep_time and arr_time variables from hhmm to minutes
dataset$dep_time = dataset$dep_time %/% 100 * 60 + dataset$dep_time %% 100
dataset$arr_time = dataset$arr_time %/% 100 * 60 + dataset$arr_time %% 100
dataset$arr_time[dataset$arr_time < dataset$dep_time] = dataset$arr_time[dataset$arr_time < dataset$dep_time] + 24 * 60

#============= II. Statistics ==============#
# Numeric data
stats = c("mean", "median", "sd", "min", "max")
dep_time = c(mean(dataset$dep_time), median(dataset$dep_time), sd(dataset$dep_time), min(dataset$dep_time), max(dataset$dep_time))
arr_time = c(mean(dataset$arr_time), median(dataset$arr_time), sd(dataset$arr_time), min(dataset$arr_time), max(dataset$arr_time))
arr_delay = c(mean(dataset$arr_delay), median(dataset$arr_delay), sd(dataset$arr_delay), min(dataset$arr_delay), max(dataset$arr_delay))
dep_delay = c(mean(dataset$dep_delay), median(dataset$dep_delay), sd(dataset$dep_delay), min(dataset$dep_delay), max(dataset$dep_delay))
distance = c(mean(dataset$distance), median(dataset$distance), sd(dataset$distance), min(dataset$distance), max(dataset$distance))
statistics = data.frame(stats, dep_time, arr_time, dep_delay, arr_delay, distance)

# Categorical data
table(dataset$month)
table(dataset$carrier)
table(dataset$origin)
table(dataset$dest)

# Histogram plot
hist(dataset$dep_time, xlab="dep_time", main="Histogram of dep_time", col="lightgreen", border="seagreen")
hist(dataset$arr_time, xlab="arr_time", main="Histogram of arr_time", col="lightgreen", border="seagreen")
hist(dataset$dep_delay, xlab="dep_delay", main="Histogram of dep_delay", col="lightgreen", border="seagreen", breaks = 150, xlim = c(-150, 150))
hist(dataset$arr_delay, xlab="arr_delay", main="Histogram of arr_delay", col="lightgreen", border="seagreen", breaks = 150, xlim = c(-150, 150))
hist(dataset$distance, xlab="distance", main="Histogram of distance", col="lightgreen", border="seagreen")

# Boxplot
# dep_time ~ carrier
ggplot(data = dataset, aes(x = carrier, y = dep_time, fill = carrier)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_time ~ origin
ggplot(data = dataset, aes(x = origin, y = dep_time, fill = origin)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_time ~ month
ggplot(data = dataset, aes(x = month, y = dep_time, fill = month)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)
# dep_time ~ dest

# arr_time ~ carrier
ggplot(data = dataset, aes(x = carrier, y = arr_time, fill = carrier)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_time ~ origin
ggplot(data = dataset, aes(x = origin, y = arr_time, fill = origin)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)
# arr_time ~ month
ggplot(data = dataset, aes(x = month, y = arr_time, fill = month)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)

# dep_delay ~ month
par(mfrow=c(1, 2)) # Arrange 2 plots in 1 row
boxplot(dataset$dep_delay~dataset$month, xlab="month", ylab="dep_delay", main="Boxplot dep_delay ~ month", col="lightskyblue", )
boxplot(dataset$dep_delay~dataset$month, xlab="month", ylab="dep_delay", main="Boxplot dep_delay ~ month - outliers", col="lightskyblue", outline=FALSE)
# dep_delay ~ carrier
par(mfrow=c(1, 2)) # Arrange 2 plots in 1 row
boxplot(dataset$dep_delay~dataset$carrier, xlab="carrier", ylab="dep_delay", main="Boxplot dep_delay ~ carrier", col="lightskyblue", )
boxplot(dataset$dep_delay~dataset$carrier, xlab="carrier", ylab="dep_delay", main="Boxplot dep_delay ~ carrier - outliers", col="lightskyblue", outline=FALSE)

# dep_delay ~ origin
par(mfrow=c(1, 2)) # Arrange 2 plots in 1 row
boxplot(dataset$dep_delay~dataset$origin, xlab="origin", ylab="dep_delay", main="Boxplot dep_delay ~ origin", col="lightskyblue", )
boxplot(dataset$dep_delay~dataset$origin, xlab="origin", ylab="dep_delay", main="Boxplot dep_delay ~ origin - outliers", col="lightskyblue", outline=FALSE)


# arr_delay ~ month
par(mfrow=c(1, 2)) # Arrange 2 plots in 1 row
boxplot(dataset$arr_delay~dataset$month, xlab="month", ylab="arr_delay", main="Boxplot arr_delay ~ month", col="lightsalmon", )
boxplot(dataset$arr_delay~dataset$month, xlab="month", ylab="arr_delay", main="Boxplot arr_delay ~ month - outliers", col="lightsalmon", outline=FALSE)
# arr_delay ~ carrier
par(mfrow=c(1, 2)) # Arrange 2 plots in 1 row
boxplot(dataset$arr_delay~dataset$carrier, xlab="carrier", ylab="arr_delay", main="Boxplot arr_delay ~ carrier", col="lightsalmon", )
boxplot(dataset$arr_delay~dataset$carrier, xlab="carrier", ylab="arr_delay", main="Boxplot arr_delay ~ carrier - outliers", col="lightsalmon", outline=FALSE)
# arr_delay ~ origin
par(mfrow=c(1, 2)) # Arrange 2 plots in 1 row
boxplot(dataset$arr_delay~dataset$origin, xlab="origin", ylab="arr_delay", main="Boxplot arr_delay ~ origin", col="lightsalmon", )
boxplot(dataset$arr_delay~dataset$origin, xlab="origin", ylab="arr_delay", main="Boxplot arr_delay ~ origin - outliers", col="lightsalmon", outline=FALSE)

# distance ~ carrier
ggplot(data = dataset, aes(x = carrier, y = distance, fill = carrier)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)
# distance ~ origin
ggplot(data = dataset, aes(x = origin, y = distance, fill = origin)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)
# distance ~ month
ggplot(data = dataset, aes(x = month, y = distance, fill = month)) +
geom_boxplot(outlier.color = "red", outlier.size = 2)

# Correlation between numeric data
# All numeric data
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
summary(anova_data)

# Calculate count, mean, sd of anova data
summarise(
  group_by(anova_data, carrier),
  count = n(),
  mean = mean(dep_delay),
  sd = sd(dep_delay)
)

# Mean & Variance plot
df = data.frame(Mean = c(10.6,2.75,8.39,4.78,10.1,2.59,4.39,9.80,2.74,7.86,13.3),
                sd = c(52.1,20.3,31.4,29.2,41.0,47.2,28.7,33.8,26.0,32.9,30.3),
                carrier = as.factor(c("AA","AS","B6","DL","F9","HA","OO","UA","US","VX","WN")))
ggplot(df, aes(x=carrier, y=Mean, fill = carrier)) + geom_point()+geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd), width=.2,position=position_dodge(0.05))

# ANOVA analysis
anova_analysis = aov(dep_delay ~ carrier, data = anova_data)

# Summary the analysis
summary(anova_analysis)

# Apply LSD test
print(LSD.test(anova_analysis, "carrier"))

# Check ANOVA assumptions
# Normality test
par(mfrow=c(1, 1))
carrier_AA = subset(anova_data, carrier=="AA")
qqnorm(carrier_AA$dep_delay)
qqline(carrier_AA$dep_delay)
lillie.test(carrier_AA$dep_delay)
# Check other carriers
carrier_AS = subset(anova_data, carrier=="AS")
carrier_B6 = subset(anova_data, carrier=="B6")
carrier_DL = subset(anova_data, carrier=="DL")
carrier_F9 = subset(anova_data, carrier=="F9")
carrier_HA = subset(anova_data, carrier=="HA")
carrier_OO = subset(anova_data, carrier=="OO")
carrier_UA = subset(anova_data, carrier=="UA")
carrier_US = subset(anova_data, carrier=="US")
carrier_VX = subset(anova_data, carrier=="VX")
carrier_WN = subset(anova_data, carrier=="WN")

lillie.test(carrier_AS$dep_delay)
lillie.test(carrier_B6$dep_delay)
lillie.test(carrier_DL$dep_delay)
lillie.test(carrier_F9$dep_delay)
lillie.test(carrier_HA$dep_delay)
lillie.test(carrier_OO$dep_delay)
lillie.test(carrier_UA$dep_delay)
lillie.test(carrier_US$dep_delay)
lillie.test(carrier_VX$dep_delay)
lillie.test(carrier_WN$dep_delay)

# Equal Variance test
boxplot(anova_data$dep_delay ~ anova_data$carrier, outline=FALSE)
# Apply levene test
leveneTest(dep_delay ~ carrier, data = anova_data)

# ANOVA test with no assumption of equal variances
oneway.test(dep_delay ~ carrier, data = anova_data)

# Pairwise t-test with no assumption of equal variances
pairwise.t.test(anova_data$dep_delay, anova_data$carrier,
                p.adjust.method = "none", pool.sd = FALSE)

#=================== Linear Regression model ===============#
dataset1 <- select(dataset, c("month", "dep_time", "arr_time", "dep_delay", "arr_delay", "distance"))
dataset2 = select(dataset, c("month", "dep_delay", "arr_delay", "distance"))

m1 <- lm(arr_delay ~ ., data = dataset1)
summary(m1)
m2 <- lm(arr_delay ~ ., data = dataset2)
summary(m2)
anova(m1, m2)

# Prediect arr_delay
dataset$pred_arrdelay <- predict(m1, newdata = dataset)
dataset$residual <- dataset$arr_delay - dataset$pred_arrdelay
residualsquare_sum = sum((dataset$pred_arrdelay -
                            dataset$arr_delay)^2)
mse = residualsquare_sum/160748
