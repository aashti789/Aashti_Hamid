library(readxl)
walmart <- read.csv('C:/R dataset/walmart.csv')
View(walmart)
library(dplyr)
v1 <- walmart %>% group_by(Store) %>% summarise(sum(Weekly_Sales))
print(v1)
v2 <- arrange(v1, desc(v1$`sum(Weekly_Sales)`))
v2
#Answer is 20th store has the maximum sales.
n1 <- nrow(walmart)
n2 <- v2$`sum(Weekly_Sales)`
n3 <- n2/n1
n3
v3 <- walmart %>%  group_by(Store) %>% summarise(sd(Weekly_Sales))
print(v3)
v4 <- arrange(v3, desc(v3$`sd(Weekly_Sales)`))
v4
#Answer is 14th store has the maximum standard deviation i.e maximum variation in sales.
install.packages('lubridate')
library(lubridate)
#Considering US third quater, JULY, AUGUST, SEPTEMBER
        
a <- '1/07/2012'
dmy(a)
b <- '30/09/2012'
dmy(b)
g <- walmart[c("Store","Date", "Weekly_Sales")]
g
h <- g[270:282,]
h
max1 <- max(h$Weekly_Sales)
max1
filter(h,h$Weekly_Sales==max1)
#Good quaterly sales is of Store :2, on the date 06-07-2012 and it is 2041507
j <- walmart[c("Date","Holiday_Flag", "Weekly_Sales")]
j
k <- j %>% filter(Holiday_Flag==0) %>% summarise(mean(Weekly_Sales))
k
# l <- j %>% filter(Holiday_Flag==1) %>% summarise(j$Weekly_Sales>1041256)
# l
m <- filter(j, Holiday_Flag=="1")
m
n <- filter(m,Weekly_Sales>1041256)
n
#n will give all the stores which have values greater than the mean.
#for linear regression
library(caTools)
s <- sample.split(walmart$Store, SplitRatio = 0.7)
s
library(dplyr)
walmart_Train <- filter(walmart, s== TRUE)
walmart_Train
walmart_Test <- filter(walmart, s== FALSE) 
walmart_Test
walmart_model <- lm(formula= walmart_Train$Weekly_Sales~walmart_Train$CPI+walmart_Train$Fuel_Price+ walmart_Train$Unemployment)
walmart_model
summary(walmart_model)
#So analyzing the model we get that for weekly sales CPI and Unemployment are highly significant variables but not Fuel Price.
library(lubridate)
a <- '05/02/2010'
b <- dmy(a)









