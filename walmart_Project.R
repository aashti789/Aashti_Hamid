#import the data
#we should import the character variable as it is
walmart <- read.csv('C:/R dataset/walmart.csv', stringsAsFactors = FALSE)
str(walmart) #to know the structure of the data, structure of Date is character.
View(walmart)
#call the lubridate package.
library('lubridate')
walmart$Date <- dmy(walmart$Date)
str(walmart) #Type of variable is changed, it is now date.

#Which store has the maximum sales.
v1 <- walmart %>% group_by(Store) %>% summarise(sum(Weekly_Sales))
print(v1)
v2 <- arrange(v1, desc(v1$`sum(Weekly_Sales)`))
v2
#Answer is 20th store has the maximum sales.

#Which store has the maximum standard deviation.
n1 <- nrow(walmart)
n2 <- v2$`sum(Weekly_Sales)`
n3 <- n2/n1
n3
v3 <- walmart %>%  group_by(Store) %>% summarise(sd(Weekly_Sales))
print(v3)
v4 <- arrange(v3, desc(v3$`sd(Weekly_Sales)`))
v4
#Answer is 14th store has the maximum standard deviation i.e maximum variation in sales.

max_std <- walmart %>% group_by(Store) %>% summarise(std=sd(Weekly_Sales),avg= mean(Weekly_Sales))
max_std
View(walmart)
max_std$CV <- (max_std$std/max_std$avg)*100
max_std$CV #Coefficient of mean to standard deviation.

#Quaterly Sales.
library(lubridate)
walmart$qtr <- quarter(walmart$Date, with_year = TRUE) #to get the year as well as the date.
View(walmart)
qtr_sales <- walmart %>% group_by(Store, qtr) %>% summarise(qtrly_sales= sum(Weekly_Sales))
qtr_sales$lag4 <-  lag(qtr_sales$qtrly_sales,4)
q3_2012 <- qtr_sales %>% filter(qtr==2012.3)
View(q3_2012)
q3_2012$gr <- ((q3_2012$qtrly_sales-q3_2012$lag4)*100)/q3_2012$lag4
arrange(q3_2012, desc(q3_2012$gr))
#44, 38,18th these stores have a good quaterly growth rate.


#mean sales in non-holiday season for all stores together.
walmart %>% filter(Holiday_Flag==0) %>% summarise(mean= mean(Weekly_Sales))
j <- walmart[c("Date","Holiday_Flag", "Weekly_Sales")]
j
k <- j %>% filter(Holiday_Flag==0) %>% summarise(mean(Weekly_Sales))
k
# l <- j %>% filter(Holiday_Flag==1) %>% summarise(j$Weekly_Sales>1041256)
# l
m <- filter(j, Holiday_Flag=="1")
m
n <- filter(m,Weekly_Sales>1041256)
View(n)
#n will give all the stores which have values greater than the mean.


walmart$mnth <- month(walmart$Date)
walmart$yr <- year(walmart$Date)
View(walmart)
#Concatenate month and year
walmart <- walmart %>% mutate(mon_yr=paste(yr, 'M', mnth))

walmart$sem <- semester(walmart$Date, with_year = TRUE)
View(walmart)
#provide a monthly view
monyr <- walmart %>% group_by(mon_yr) %>% summarise(total_sales= sum(Weekly_Sales))
#bar plot to describe the data
library('ggplot2')
ggplot(data=monyr, mapping= aes(x= mon_yr,y=total_sales))+ geom_bar(stat = 'identity')
#Semester aggregate.
semyr <- walmart %>% group_by(sem) %>% summarise(total_sales= sum(Weekly_Sales))
View(semyr)
#Create a bar plot with the semesters
ggplot(data= semyr, mapping= aes(x= sem, y= total_sales))+ geom_bar(stat= 'identity')
#IInd semester have the highest Sales.

##LInear regression Model
store_1 <- walmart %>% filter(Store==1)
#Sort the weekly sales by Date.
store_1 <- store_1 %>% arrange(Date)
#Restructure the dates: 1 for 1st date
store_1$new_dt <- factor(row_number(store_1$Store))
View(store_1)
#Hypothesize if CPI, Unemployment etc have any impact on sales.
library(caTools)
set.seed(100)
#Dividing the data into training and test datasets
s <- sample.split(store_1$Weekly_Sales, SplitRatio = 0.7)
#Training dataset
store_1_train <- filter(store_1, s== TRUE)
dim(store_1_train)
#Test dataset
store_1_test <- filter(store_1, s== FALSE)
dim(store_1_test)
#Building the model
sales_model <- lm(Weekly_Sales~ Holiday_Flag+ Temperature+ Fuel_Price+ CPI+ Unemployment, data= store_1_train)
summary(sales_model)
#According to the model built, CPI and Unemployment seems significant.
sales_model <- lm(Weekly_Sales ~ CPI+ Unemployment, data= store_1_train)
summary(sales_model)
#Since p<0.05 therefore, overall model is significant.



##Change dates into days.
store_1_train$day <- day(store_1_train$Date)
View(store_1_train)
store_1_test$day <- day(store_1_test$Date)
sales_model <- lm(Weekly_Sales~ CPI+ Unemployment+day, data= store_1_train)
summary(sales_model)
#  So, According to the LInear regression model, CPI, Unemployment and day are significant.
#Weekly_Sales= -3914558 +20350*CPI+153712*Unemployment-6390*day

#Checking the accuracy of the model
predict_store_1 <-  predict(sales_model, store_1_test)
predict_store_1
error <-  store_1_test- predict_store_1
qqnorm(error)
summary(error)
