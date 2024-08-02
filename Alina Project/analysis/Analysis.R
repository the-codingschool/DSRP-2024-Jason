#load all required libraries 
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(tidyverse)

###TESTS
#ANOVA test
#one way between zipcode and score
#bronx
one.way <- aov(zipcode ~ score, data = data_bronx)
summary(one.way)

#brooklyn
one.way1 <- aov(zipcode ~ score, data = data_brooklyn)
summary(one.way1)

#manhattan
one.way4 <- aov(zipcode ~ score, data = data_manhattan)
summary(one.way4)

#staten island
one.way2 <- aov(zipcode ~ score, data = data_staten)
summary(one.way2)

#queens
one.way3 <- aov(zipcode ~ score, data = data_queens)
summary(one.way3)

#one way between boro and score
one.wayten <- aov(score ~ boro, data = data_2024)
summary(one.wayten)

#one way between cuisine and score
# bronx
one.way11 <- aov(score ~ cuisine_description, data = data_bronx)
summary(one.way11)

#brooklyn
one.way12 <- aov(score ~ cuisine_description, data = data_brooklyn)
summary(one.way12)

#manhattan
one.way13 <- aov(score ~ cuisine_description, data = data_manhattan)
summary(one.way13)

# queens
one.way14 <- aov(score ~ cuisine_description, data = data_queens)
summary(one.way14)

#staten
one.way15 <- aov(score ~ cuisine_description, data = data_staten)
summary(one.way15)

#two way between boro and cuisine and score 
two.way <- aov(score ~ boro + cuisine_description, data = data_2024)
summary(two.way)

#two way between cuisine and zipcode and score 
#bronx
two.way <- aov(score ~ cuisine_description + zipcode, data = data_bronx)
summary(two.way)

#brooklyn
two.way <- aov(score ~ cuisine_description + zipcode, data = data_brooklyn)
summary(two.way)
#manhattan
two.way <- aov(score ~ cuisine_description + zipcode, data = data_manhattan)
summary(two.way)
#staten island
two.way <- aov(score ~ cuisine_description + zipcode, data = data_queens)
summary(two.way) 
#queens
two.way <- aov(score ~ cuisine_description + zipcode, data = data_staten)
summary(two.way)

#chi-squared test
#between boro and grade 
con_table <- table(data_2024$boro, data_2024$grade)
result <- chisq.test(con_table)
print(result)

result$residuals

#fishertest
fisher.test(con_table,simulate.p.value=TRUE)

#between cuisine and grade
con_table1 <- table(data_2024$cuisine_description, data_2024$grade)
result2 <- chisq.test(con_table1)
print(result2)

result2$residuals

#between cuisine and boro
con_table2 <- table(data_2024$cuisine_description, data_2024$boro)
result3 <- chisq.test(con_table2)
print(result3)

result3$residuals

