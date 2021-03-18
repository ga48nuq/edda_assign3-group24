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
# hist(titanic,main="Age")
# hist(titanic,main="Survived")
barplot(xtabs(Survived~Age,data=titanic))
barplot(xtabs(Survived~PClass,data=titanic))
barplot(xtabs(Survived~Sex,data=titanic))

#b)

titanicglm=glm(Survived~Age+PClass+Sex,data=titanic,family=binomial)
summary(titanicglm)


#c)

titanic$Age=as.numeric(titanic$Age) 
glm3=glm(Survived~Age+PClass+Sex,data=titanic,family=binomial)
glm3

glm4=glm(Survived~Age*Sex,data=titanic,family=binomial)
anova(glm4,test="Chisq")

glm5=glm(Survived~Age*PClass,data=titanic,family=binomial)
anova(glm5,test="Chisq")


#d)

# aggregated data format
# "This aggregated format in the form of pair(success,failure), the counts ofsuccesses and failures for each combination of levels of the factors (or values ofnumeric variables), is one of 3 possible ways to specify the responses inRfor thelogistic model. This format is not useful if there is a continuous predictor in the modelthat is different for different individuals (e.g., different ages for different individuals)."




#e)
# not finished: issue = missing values for Age

titanic$Age=as.numeric(titanic$Age)
titanic$PClass=as.numeric(titanic$PClass)
titanic$Sex=as.numeric(titanic$Sex)
titanic$Survived=as.numeric(titanic$Survived)

z=chisq.test(titanic); z
chisq.test(titanic,simulate.p.value=TRUE)
residuals(z)

z=chisq.test(titanic$Survived); z
chisq.test(titanic$Survived,simulate.p.value=TRUE)
residuals(z)

z1=chisq.test(titanic$Age); z1
chisq.test(titanic$Age,simulate.p.value=TRUE)
residuals(z1)

z2=chisq.test(titanic$PClass); z2
chisq.test(titanic$PClass,simulate.p.value=TRUE)
residuals(z2)

z3=chisq.test(titanic$Sex); z3
chisq.test(titanic$Sex,simulate.p.value=TRUE)
residuals(z3)


titanicdf <- select(titanic, -Age) %>% head()

x=chisq.test(titanicdf); x
chisq.test(titanicdf,simulate.p.value=TRUE)
residuals(x)  

#f)



# exercise 3 - Military coups jn Africa


#a)
africa
test = glm(miltcoup ~ oligarchy + pollib+ parties+pctvote+popn+size+numelec+numregim, data = africa, family = poisson(link = "log"))
summary(test)




#b)




#c)




