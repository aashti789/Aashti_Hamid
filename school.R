pnorm(8.4, mean = 9.5, sd= 1.1, lower.tail = TRUE)
pnorm(12.8, mean = 9.5, sd= 1.1, lower.tail = FALSE)
v1 <- pnorm(7.3, mean = 9.5, sd=1.1, lower.tail = TRUE)
v2 <- pnorm(11.7, mean = 9.5, sd=1.1, lower.tail = FALSE)
1-(v1+v2)
pnorm(2.2, mean = 2, sd= 0.7/sqrt(50), lower.tail =FALSE)
qnorm(0.05, mean = 0, sd=1)
library(readxl)
school <- read.csv('C:/R dataset/scores_class.csv')
View(school)
m1 <- mean(school$math)
m1
nrow(school)
sd(school$math)
sqrt(200)
pnorm(52.645, mean = 50, sd= 9.368448/14.14214, lower.tail = FALSE)
#Reject the hypothesis as p<alpha
#2) Null hyp: mean_boy-mean_girl=0
school1 <- school[c("female","math")]
school1
library(dplyr)
math_b <- school1 %>% subset(female==0)
math_b
math_g <- school1 %>% filter(female==1)
math_g
t.test(math_b, math_g, conf.level = 0.95)
#p>alpha, fail to reject that means there is very small difference in the math scores=> governor is not speaking truth.
#3) null hyp is there is no difference btw the reading and writing scores of the students.
dif <- school[c("read", "write")]
dif
t.test(school$read, school$write, conf.level = 0.95, paired=TRUE)
#p>alpha => we fail to reject the null, no significant difference.
