# Title     : Project Elementaire Statistiek
# Objective : Analyseren data van de drugs dataset
# Created by: RobMa
# Created on: 26/05/2021

# Verdeling van de variabele 'age'

test <- read.csv('drugs_2021.csv', sep = ';')
head(test)

age <- test$age

# hist(age, main = '', xlim = c(20, 50))
qqnorm(age)
qqline(age)