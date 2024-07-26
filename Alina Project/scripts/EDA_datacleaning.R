
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
  # restarunts per borough per year  
    # 2024

    # 2023

    # 2022

    # 2021

    # 2020

    # 2019

    # 2018


