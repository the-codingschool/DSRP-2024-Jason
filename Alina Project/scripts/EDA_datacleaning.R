
getwd()

#read in dataset
data <- read.csv("/Users/alimi/Downloads/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv (2)/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv")

head(data)
summary(data)
str(data)
colnames(data)

data
library(janitor)

 #clean names of dataframe

data_clean <- clean_names(data,case = 'snake')

#different inspections/scores/grades/boros
unique(data_clean$score)
unique(data_clean$grade)
unique(data_clean$inspection_type)
unique(data_clean$boro)

#total restarunts with each grade
table(data_clean$grade)

#subsets of grades received 
nrow(subset(data_clean, grade == "A"))
nrow(subset(data_clean, grade == "B"))
nrow(subset(data_clean, grade == "C"))
nrow(subset(data_clean, grade == "N"))
nrow(subset(data_clean, grade == "P"))
nrow(subset(data_clean, grade == "Z"))

#stats on the data
mean(data_clean$score, na.rm = TRUE)
median(data_clean$score, na.rm = TRUE)
range(data_clean$score, na.rm = TRUE)

#subsets on scores recieved
nrow(subset(data_clean, score <20))
nrow(subset(data_clean, score <60)) - nrow(subset(data_clean, score <20))
nrow(subset(data_clean, score <120)) - nrow(subset(data_clean, score <60))
nrow(subset(data_clean, score <168)) - nrow(subset(data_clean, score <120))

#ggplot/histograms/box plots/ bar graphs/ pie charts/ scatterplots/ line graphs from data
  # distributions of scores
ggplot(data = data_clean, aes(score)) + geom_histogram(bins = 70)+
  labs(title = "distribution of scores of restaurants overall", x = "score",y = "count")

  # restaurants per borough per year
library(tidyr)

    # 2024
data_sep <-separate(data_clean, col=inspection_date, into=c("day", "month", "year"), sep = "/")
data_sep%>%filter(year == 2024)
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2024", x = "boroughs",y = "count")

    # 2023
data_sep%>%filter(year == 2023)
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2023", x = "boroughs",y = "count")

    # 2022
data_sep%>%filter(year == 2022)
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2022", x = "boroughs",y = "count")

    # 2021
data_sep%>%filter(year == 2021)
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2021", x = "boroughs",y = "count")

    # 2020
data_sep%>%filter(year == 2020)
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2020", x = "boroughs",y = "count")

    # 2019
data_sep%>%filter(year == 2019)
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2019", x = "boroughs",y = "count")

    # 2018
data_sep%>%filter(year == 2018)
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2018", x = "boroughs",y = "count")


