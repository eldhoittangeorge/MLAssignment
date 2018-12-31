
library(ggplot2)
library(stats)
library(lattice)
library(cluster)
library(fpc)


data = read.csv("snsdata.csv")
str(data)

summary(data$age)
summary(data$gender)
summary(data$gradyear)

#print(data$age[1:100])
data$age <- ifelse(data$age > 13 & data$age < 20,data$age,NA)
summary(data$age)

data$female <- ifelse(data$gender == 'F' & !is.na(data$gender),1,0)
data$unknownGender <- ifelse(is.na(data$gender),1,0)

summary(data$female)











