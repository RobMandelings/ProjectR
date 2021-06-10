# Title     : Project Elementaire Statistiek
# Objective : Analyseren data van de drugs dataset
# Created by: RobMa
# Created on: 26/05/2021

library(ggplot)
library(zoo)
library(e1071)
library(dplyr)

dataset <- read.csv('drugs_2021.csv', sep = ';')

i <- 8
j <- 8
k <- 4

dataset[-c(k + 1, j + 1, i + 1, j*k + 1, i*j + 1, i*k + 1, i*j*k + 1, i + j + k + 1),]

# Vraag 1: verdeling van de variabele age

age <- (dataset$age)
age <- na.omit(age)
los <- dataset$los
time <- dataset$time

qqnorm(age, main = title('Normal Q-Q Plot variabele age'))
qqline(age)

age_hist <- hist(age, breaks = 30)
xfit <- seq(min(age), max(age), length = 40) 
yfit <- dnorm(xfit, mean = mean(age), sd = sd(age)) 
yfit <- yfit * diff(age_hist$mids[1:2]) * length(age)

lines(xfit, yfit, col = "red", lwd = 2)


table(age)

shapiro.test(age)

# Vraag 1: normalizering

log_age <- log(age)

qqnorm(log_age, main = title('Normal Q-Q Plot variabele age'))
qqline(log_age)

log_age_hist <- hist(log_age, breaks = 30)
xfit <- seq(min(log_age), max(log_age), length = 40) 
yfit <- dnorm(xfit, mean = mean(log_age), sd = sd(log_age)) 
yfit <- yfit * diff(log_age_hist$mids[1:2]) * length(log_age)

lines(xfit, yfit, col = "red", lwd = 2)
shapiro.test(log_age)

# Vraag 2: Hangt de efficientie af van het type programma

treat <- dataset$treat
time <- dataset$time
time_treat_0 <- subset(dataset$time, treat == 0)
time_treat_1 <- subset(dataset$time, treat == 1)

time_treat_0_hist <- hist(time_treat_0, xlim = c(0,max(time_treat_0, time_treat_1)), ylim = c(0, 130), main = "Histogram of time with treat = 0")
xfit <- seq(min(time_treat_0), max(time_treat_0), length = 40) 
yfit <- dnorm(xfit, mean = mean(time_treat_0), sd = sd(time_treat_0)) 
yfit <- yfit * diff(time_treat_0_hist$mids[1:2]) * length(time_treat_0)
lines(xfit, yfit, col = "red", lwd = 2)

time_treat_1_hist <- hist(time_treat_1, xlim = c(0,max(time_treat_0, time_treat_1)), ylim = c(0, 130), main="Histogram of time with treat = 1")
xfit <- seq(min(time_treat_1), max(time_treat_1), length = 40) 
yfit <- dnorm(xfit, mean = mean(time_treat_1), sd = sd(time_treat_1)) 
yfit <- yfit * diff(time_treat_1_hist$mids[1:2]) * length(time_treat_1)
lines(xfit, yfit, col = "red", lwd = 2)


mean(time_treat_0)
median(time_treat_0)

mean(time_treat_1)
median(time_treat_1)

shapiro.test(time_treat_0)
shapiro.test(time_treat_1)

wilcox.test(time_treat_0, time_treat_1, alternative = "two.sided")
wilcox.test(time_treat_1, time_treat_0, alternative = "greater")


# Vraag 3: Verband drugsgebruik voor de inschrijving en type behandeling

table(dataset$treat, dataset$ivhx)
chisq.test(dataset$treat, dataset$ivhx)

# Vraag 4: Duur van de behandeling invloed op tijd tot herval?

los <- dataset$los
time <- dataset$time

plot(dataset$los, dataset$time, main = "Scatterplot", xlab = "los (duur behandeling)", ylab = 'time (tijd tot herval)')
abline(lm(dataset$los ~ dataset$time, data = mtcars), col = "red")

cor.test(dataset$los, dataset$time, method = "spearman")
losAndTime <- dataset[c("los", "time")]
lmLosTime <- lm(los~time, data=losAndTime)
summary(lmLosTime)

los_100 <- subset(dataset$time, los == 100)
max(los_100)
min(los_100)
#
# quantile(age, na.rm = TRUE)
# hist(age)
# max(age, na.rm = TRUE)