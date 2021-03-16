#set working directory
rm(list=ls())

#install.packages("readr")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("stargazer")
#install.packages("outliers")


library(readr)
library(tidyverse)
library(dplyr)
library(stargazer)
library(outliers)

#read data

fruitflies=read.table("data/fruitflies.txt", header = TRUE)
titanic=read.table("data/titanic.txt", header = TRUE)
africa=read.table("data/africa.txt")

#exercise 1 - Fruit flies


#a)

fruitflies$loglongevity = log(fruitflies$longevity)
attach(fruitflies)
plot(loglongevity ~ thorax, pch = 20, col=ifelse(activity=="isolated", "red", ifelse(activity=="low", "blue", "green")))
legend("topleft", legend=c("isolated", "low", "high"), col=c("red", "blue","green"), pch=20, box.lty=2)
fruitflies$activity = as.factor(fruitflies$activity)
fruitflieslm1 = lm(loglongevity~activity, data=fruitflies)
anova(fruitflieslm1)[5]
summary(fruitflieslm1)[4]





#b)




#c)




#d)





#e)





#f)



# exercise 2 - Titanic



#a)
summary(titanic)
hist(titanic,main="Age")
hist(titanic,main="Survived")
barplot(xtabs(Survived~Age,data=titanic))
barplot(xtabs(Survived~PClass,data=titanic))
barplot(xtabs(Survived~Sex,data=titanic))

#b)




#c)




#d)





#e)





#f)



# exercise 3 - Military coups jn Africa


#a)
africa
test = glm(miltcoup ~ oligarchy + pollib+ parties+pctvote+popn+size+numelec+numregim, data = africa, family = poisson(link = "log"))
summary(test)




#b)




#c)




