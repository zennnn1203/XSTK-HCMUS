#-----------------------------PREPROCESSING-------------------------------------
# packages
library("stats")
library("ggplot2")

# prepare workspace & fetching data
path.expand(path = "C:/Users/khanh/P&S assignment")
Data <- read.csv("Computers.csv") # by default, header = TRUE
Data <- Data[, 2:9]
print(any(is.na(Data)))
View(Data)

#-----------------------------FEATURES------------------------------------------
# descriptive statistics
Mean <- lapply(Data[, c("price", "speed", "hd", "ram", "screen")], mean)
Median <- lapply(Data[, c("price", "speed", "hd", "ram", "screen")], median)
Sd <- lapply(Data[, c("price", "speed", "hd", "ram", "screen")], sd)
Max <- lapply(Data[, c("price", "speed", "hd", "ram", "screen")], max)
Min <- lapply(Data[, c("price", "speed", "hd", "ram", "screen")], min)

Mean <- lapply(Mean, round, 2)
Sd <- lapply(Sd, round, 2)

output <- rbind(Mean, Median, Sd, Max, Min)
print(output)

# -------------------------------FREQUENCY--------------------------------------

# frequency distribution
total <- nrow(Data)
cd_f <- round(c(sum(Data$cd == "yes"), sum(Data$cd == "no")) / total, 4)
multi_f <- round(c(sum(Data$multi == "yes"), sum(Data$multi == "no")) / total, 4)
premium_f <- round(c(sum(Data$premium == "yes"), sum(Data$premium == "no")) / total, 4)

name <- c("yes", "no")
names(cd_f) <- name
names(multi_f) <- name
names(premium_f) <- name
print(rbind(cd_f, multi_f, premium_f))

# pie chart
par(mfrow = c(1, 3))
color <- c("red", "yellow")
pie(cd_f, labels = cd_f, main = "cd frenquency", radius = 1.0, col = color)
legend("topright", c("yes", "no"), fill = color)
pie(multi_f, labels = multi_f, main = "multi-core frenquency", radius = 1.0, col = color)
legend("topright", c("yes", "no"), fill = color)
pie(premium_f, labels = premium_f, main = "premium frenquency", radius = 1.0, col = color)
legend("topright", c("yes", "no"), fill = color)

#--------------------------------MODE-------------------------------------------

# mode
# function to get mode
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
Mode <- c(getmode(Data$ram), getmode(Data$screen))
names(Mode) <- c("ram", "screen")
print(Mode)

#-----------------------------PLOT----------------------------------------------

# visualize data
# box plot
par(mfrow = c(1, 3))
boxplot(Data$price, main = "price", col = "blue")
boxplot(Data$speed, main = "speed", col = "blue")
boxplot(Data$hd, main = "hd", col = "blue")

# histogram
par(mfrow = c(2, 2))
hist(Data$price, xlab = "price", col = "orange")
hist(Data$speed, xlab = "speed", breaks = 10, col = "orange")
hist(Data$hd, xlab = "hd", col = "orange")
hist(Data$ram, xlab = "ram", breaks = 5, col = "orange")

# first, create covariance matrix
cov_matrix <- cov(Data[, c("price", "speed", "hd", "ram", "screen")])
View(cov_matrix)
# heat map
par(mfrow = c(1, 1))
heatmap(cov_matrix)

#---------------------------Linear model----------------------------------------
# get a sample of 1000 recent datapoints
Sample <- Data[5260:6259,]

# Model1
m1 <- lm(Sample$price ~ ., Sample)
summary(m1)
# Model2
m2 <- lm(Sample$price ~ . - multi, Sample)
summary(m2)

# use analysis of variance to determine better model
anova(m1, m2)
plot(m2)

# predictions
Sample$pred_price <- predict(m2, newdata = Sample)
Sample$residual <- Sample$price - Sample$pred_price
observed <- c(sum(Sample$price < 2000), 
              1000 - (sum(Sample$price < 2000) + sum(Sample$price > 3000)), 
              sum(Sample$price > 3000))
predicted <- c(sum(Sample$pred_price < 2000),
               1000 - (sum(Sample$pred_price < 2000) + sum(Sample$pred_price > 3000)), 
               sum(Sample$pred_price > 3000))
label <- c("< 2000", "2000 - 3000", "> 3000")
names(observed) <- label
names(predicted) <- label
print(rbind(observed, predicted))
