install.packages('readxl')
library(readxl)
banking <-  read.csv('C:/R dataset/Demo 1_ Identifying Data Structures.csv')
View(banking)
str(banking)
View(mtcars)
library(dplyr)
select(mtcars, mpg, wt)
v1 <- c(mtcars$mpg, mtcars$wt),
v1
mtcars[,c(1,6)]
mtcars[, c(1:6)]
select(mtcars, 1:6)
filter(mtcars, cyl==4)
mtcars %>% group_by(hp>100) %>% filter(cyl==6)
arrange(mtcars, disp, cyl==6)
help("summarise")
summarise(mtcars, cyl==6)
mtcars %>% group_by(cyl) %>% summarise(mean(wt))
mtcars %>% group_by(gear) %>% summarise(sd(wt))
str(banking)
v <- rename(banking,c("ï..age"= "age"))
v
help(rename)
banking <- rename(banking, c("Age"= "ï..age"))
View(banking)
v1 <- c(banking$Age)
v1
generations <- ifelse(v1<22, 'Z',ifelse(v1<41, 'Y', ifelse(v1<53, 'X', 'Babyboomers')))
banking <- cbind(banking, generations)
banking
View(banking)
table(banking$poutcome, banking$generations)
qt(c(0.025, 0.975), df=5)
sqrt(5)
qnorm(0.025, mean = 0, sd= 1/2, lower.tail = TRUE)
help(qt)
pf(0.05, 2, 60)
