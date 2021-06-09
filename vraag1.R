# Title     : Project Elementaire Statistiek
# Objective : Analyseren data van de drugs dataset
# Created by: RobMa
# Created on: 26/05/2021

library(ggplot)
library(zoo)

dataset <- read.csv('drugs_2021.csv', sep = ';')

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

lines(xfit, yfit, col = "black", lwd = 2)



table(age)

shapiro.test(age)

# Vraag 1: normalizering

hist(log(age))
qqnorm(log(age))

# Vraag 2: Hangt de efficientie af van het type programma

treat <- dataset$treat
time <- dataset$time

time_treat_0 <- subset(dataset$time, treat == 0)
median(time_treat_0)

time_treat_1 <- subset(dataset$time, treat == 1)
median(time_treat_1)

mean(treat)
median(treat)

# Vraag 3: Verband drugsgebruik voor de inschrijving en type behandeling

table(dataset$treat, dataset$ivhx)
chisq.test(dataset$treat, dataset$ivhx)

# Vraag 4: Duur van de behandeling invloed op tijd tot herval?

plot(dataset$los, dataset$time, main = "Scatterplot", xlab = "los (duur behandeling)", ylab = 'time (tijd tot herval)')
abline(lm(dataset$los ~ dataset$time, data = mtcars), col = "red")

cor.test(dataset$los, dataset$time, method = "spearman")
#
# quantile(age, na.rm = TRUE)
# hist(age)
# max(age, na.rm = TRUE)