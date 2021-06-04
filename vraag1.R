# Title     : Project Elementaire Statistiek
# Objective : Analyseren data van de drugs dataset
# Created by: RobMa
# Created on: 26/05/2021

# Vraag 1: verdeling van de variabele age

age <- dataset$age

qqnorm(age, main = title('Normal Q-Q Plot variabele age'))
qqline(age)

shapiro.test(age)

# Vraag 2: Hangt de efficientie af van het type programma

treat <- dataset$treat
time <- dataset$time

time_treat_0 <- subset(dataset$time, treat == 0)
median(time_treat_0)

time_treat_1 <- subset(dataset$time, treat == 1)
median(time_treat_1)

mean(treat)
median(treat)


#
# quantile(age, na.rm = TRUE)
# hist(age)
# max(age, na.rm = TRUE)