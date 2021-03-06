#set working directory
rm(list=ls())

#install.packages("readr")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("stargazer")
#install.packages("outliers")
#install.packages("tidyr")


library(readr)
library(tidyverse)
library(dplyr)
library(stargazer)
library(outliers)
library(tidyr)

#read data

fruitflies=read.table("data/fruitflies.txt", header = TRUE)
titanic=read.table("data/titanic.txt", header = TRUE)
africa=read.table("data/africa.txt", header = TRUE)

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
# above option 1, below option 2
fruitflies["loglongevity"] <- log(fruitflies["longevity"])
head(fruitflies, 5)
  

plot(longevity ~ thorax, pch = toupper(as.character(activity)), data = fruitflies)


contrasts(fruitflies$activity) <- contr.sum
fruitfliesaov <- lm(loglongevity ~ activity, data = fruitflies)
anova(fruitfliesaov)

coefficients <- summary(fruitfliesaov)$coefficients
coeff_intercept <- coefficients[[1]]
coeff_estimate_high <- coefficients[[2]]
coeff_estimate_isolated <- coefficients[[3]]
coeff_estimate_low <- -coeff_estimate_high - coeff_estimate_isolated
mean_thorax <- mean(fruitflies$thorax)
(loglongevity_high <- coeff_intercept + coeff_estimate_high)
(loglongevity_low <- coeff_intercept + coeff_estimate_low)
(loglongevity_isolated <- coeff_intercept + coeff_estimate_isolated)
exp(loglongevity_high)
exp(loglongevity_low)
exp(loglongevity_isolated)



#b)
contrasts(fruitflies$activity) <- contr.sum
fruitfliesaov <- lm(loglongevity ~ thorax + activity, data = fruitflies)
anova(fruitfliesaov)

coefficients <- summary(fruitfliesaov)$coefficients
coeff_intercept <- coefficients[[1]]
coeff_thorax <- coefficients[[2]]
coeff_estimate_high <- coefficients[[3]]
coeff_estimate_isolated <- coefficients[[4]]
coeff_estimate_low <- -coeff_estimate_high - coeff_estimate_isolated
mean_thorax <- mean(fruitflies$thorax)
(loglongevity_high <- coeff_intercept + coeff_thorax * mean_thorax + coeff_estimate_high)
(loglongevity_isolated <- coeff_intercept + coeff_thorax * mean_thorax + coeff_estimate_isolated)
(loglongevity_low <- coeff_intercept + coeff_thorax * mean_thorax + coeff_estimate_low)
exp(loglongevity_high)
exp(loglongevity_isolated)
exp(loglongevity_low)


#c)
high = fruitflies[fruitflies$activity == "high", ]
isolated = fruitflies[fruitflies$activity == "isolated", ]
low = fruitflies[fruitflies$activity == "low", ]

par(mfrow = c(1, 3))
plot(isolated$thorax, isolated$loglongevity, main = "Isolated sexual activity")
plot(low$thorax, low$loglongevity, main = "Low sexual activity")
plot(high$thorax, high$loglongevity, main = "High sexual activity")



#d)





#e)
par(mfrow = c(1, 2))
fruitfliesaov <- lm(loglongevity ~ thorax + activity, data = fruitflies)
plot(fruitfliesaov, which = c(1, 2))
shapiro.test(residuals(fruitfliesaov))




#f)
par(mfrow = c(1, 2))
fruitfliesaov <- lm(longevity ~ thorax + activity, data = fruitflies)
plot(fruitfliesaov, which = c(1, 2))
shapiro.test(residuals(fruitfliesaov))

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

titanic %>% drop_na()
titanic$Age=as.numeric(titanic$Age)
#titanic$Age=na.omit(titanic$Age)
titanic$PClass=as.numeric(titanic$PClass)
titanic$Sex=as.numeric(titanic$Sex)
titanic$Survived=as.numeric(titanic$Survived)
titanicdf <- select(titanic, -Name) %>% head()
z=chisq.test(titanicdf); z
chisq.test(titanicdf,simulate.p.value=TRUE)
residuals(z)
# 
# z=chisq.test(titanic$Survived); z
# chisq.test(titanic$Survived,simulate.p.value=TRUE)
# residuals(z)
# 
# z1=chisq.test(titanic$Age); z1
# chisq.test(titanic$Age,simulate.p.value=TRUE)
# residuals(z1)
# 
# z2=chisq.test(titanic$PClass); z2
# chisq.test(titanic$PClass,simulate.p.value=TRUE)
# residuals(z2)
# 
# z3=chisq.test(titanic$Sex); z3
# chisq.test(titanic$Sex,simulate.p.value=TRUE)
# residuals(z3)


# titanicdf <- select(titanic, -Age, -Name) %>% head()
# 
# x=chisq.test(titanicdf); x
# chisq.test(titanicdf,simulate.p.value=TRUE)
# residuals(x)  

#f)



# exercise 3 - Military coups in Africa


#a)
africa
test = glm(miltcoup ~ oligarchy + pollib+ parties+pctvote+popn+size+numelec+numregim, data = africa, family = poisson(link = "log"))
summary(test)




#b)

summary(lm(miltcoup~oligarchy + pollib + parties + pctvote + 
             popn + size + numelec + numregim,data=africa))


#c)

africa$pollib = factor(africa$pollib)

a = mean(africa$oligarchy)
b = mean(africa$parties)
mean(africa$oligarchy)
mean(africa$parties)

test1 = glm(miltcoup ~ oligarchy + pollib+ parties, data = africa, family = poisson(link = "log"))

pollib_0 = data.frame(oligarchy = a, parties = b, pollib ="0" )
pollib_1 = data.frame(oligarchy = a, parties = b, pollib ="1")
pollib_2 = data.frame(oligarchy = a, parties = b, pollib ="2")

ans_pollib0 = predict(test1 , pollib_0 , type ="response") 
ans_pollib1 = predict(test1 , pollib_1 , type ="response")
ans_pollib2 = predict(test1 , pollib_2 , type ="response")

print(ans_pollib0)
print(ans_pollib1)
print(ans_pollib2)




