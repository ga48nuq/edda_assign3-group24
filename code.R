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

fruitflies=read.table("data/fruitflies.txt")
titanic=read.table("data/titanic.txt")
africa=read.table("data/africa.txt")

#exercise 1 - Fruit flies


#a)






#b)




#c)




#d)





#e)





#f)



# excercise 2 - Titanic



#a)






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




