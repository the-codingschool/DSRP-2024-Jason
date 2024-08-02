#load all required libraries 
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(tidyverse)


getwd()

#read in dataset
  data <- read.csv("/Users/alimi/Downloads/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv (2)/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv")

#some info on the data
  head(data)
  summary(data)
  str(data)
  colnames(data)

#clean names of dataframe
  data_clean <- clean_names(data,case = 'snake')
  data_clean <- remove_empty(data_clean)
  data_clean <- na.omit(data_clean)


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

#separate data
  data_sep <- separate(data = data_clean, col = inspection_date, into = c("day", "month", "year"),   sep = "/")
  arrange(data_sep, year)

#ggplot/histograms/box plots/ bar graphs/ pie charts/ scatterplots/ line graphs from data
  # distributions of scores overall 

  ggplot(data = data_clean, aes(score)) + geom_histogram(bins = 70)+
  labs(title = "distribution of scores of restaurants overall", x = "score",y = "count")

  # restaurants per borough thorughout the years
    # in 2024
  data_2024 <- data_sep[data_sep$year %in% "2024", ]
  ggplot(data_2024, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2024", x = "boroughs",y = "count")
 
   #in 2023
  data_2023 <- data_sep[data_sep$year %in% "2023", ]
  ggplot(data_2023, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2023", x = "boroughs",y = "count")

  #in 2022
  data_2022 <- data_sep[data_sep$year %in% "2022", ]
  ggplot(data_2022, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2022", x = "boroughs",y = "count")

  #in 2021
  data_2021 <- data_sep[data_sep$year %in% "2021", ]
  ggplot(data_2021, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2021", x = "boroughs",y = "count")

  #in 2020
  data_2020 <- data_sep[data_sep$year %in% "2020", ]
  ggplot(data_2020, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough in 2020", x = "boroughs",y = "count")

  
# Distribution of the grades per borough compared in 2020 to 2024
    #overall in 2024
  data_2024 <- data_sep[data_sep$year %in% "2024", ]
  ggplot(data = data_2024, aes(x = boro, fill = grade)) +geom_bar()

      #bronx
  data_bronx <- data_2024[data_2024$boro %in% "Bronx", ]
  pie(table(data_bronx$grade), main = "Distribution of grades in bronx in 2024", labels = paste(names(table(data_bronx$grade)), "\n", table(data_bronx$grade), sep = ""))

      #brooklyn
  data_brooklyn <- data_2024[data_2024$boro %in% "Brooklyn", ]
  pie(table(data_brooklyn$grade), main = "Distribution of grades in brooklyn in 2024", labels = paste(names(table(data_brooklyn$grade)), "\n", table(data_brooklyn$grade), sep = ""))

      #manhattan
  data_manhattan <- data_2024[data_2024$boro %in% "Manhattan", ]
  pie(table(data_manhattan$grade),main = "Distribution of grades in manhattan in 2024", labels = paste(names(table(data_manhattan$grade)), "\n", table(data_manhattan$grade), sep = ""))

      #queens
  data_queens <- data_2024[data_2024$boro %in% "Queens", ]
  pie(table(data_queens$grade), main = "Distribution of grades in Queens in 2024", labels = paste(names(table(data_queens$grade)), "\n", table(data_queens$grade), sep = ""))

      #staten island
  data_staten <- data_2024[data_2024$boro %in% "Staten Island", ]
  pie(table(data_staten$grade),main = "Distribution of grades in staten Island in 2020", labels = paste(names(table(data_staten$grade)), "\n", table(data_staten$grade), sep = ""))


#grades by cuisine in each borough in 2024
  #in 2024
  data_2024 <- data_sep[data_sep$year %in% "2024", ]

    #bronx 
  data_bronx <- data_2024[data_2024$boro %in% "Bronx", ]
  ggplot(data = data_bronx, aes(x = cuisine_description, fill = grade)) + geom_bar() + labs(title = "distribution of scores per cuisine in Bronx in 2024",   x = "Cuisines", y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))
  
  #brooklyn
  data_brooklyn <- data_2024[data_2024$boro %in% "Brooklyn", ]
  ggplot(data = data_brooklyn, aes(x = cuisine_description, fill = grade)) + geom_bar() + labs(title = "distribution of scores per cuisine in Brooklyn in 2024",  x = "Cuisines",  y = "Count", fill = "Grades")

  #manhattan
  data_manhattan <- data_2024[data_2024$boro %in% "Manhattan", ]
  ggplot(data = data_manhattan, aes(x = cuisine_description, fill = grade)) + geom_bar() + labs(title = "distribution of scores per cuisine in Manhattan in 2024", x = "Cuisines",  y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))

    #queens
    data_queens <- data_2024[data_2024$boro %in% "Queens", ]
    ggplot(data = data_queens, aes(x = cuisine_description, fill = grade)) + geom_bar() + labs(title = "distribution of scores per cuisine in Queens in 2024", x = "Cuisines", y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))

    #staten island
    data_staten <- data_2024[data_2024$boro %in% "Staten Island", ]
    ggplot(data = data_staten, aes(x = cuisine_description, fill = grade)) + geom_bar() + labs(title = "distribution of scores per cuisine in Staten Island in 2024",x = "Cuisines", y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))

  
#grades variance between zip codes in each borough in 2024

  #in 2024
  data_2024 <- data_sep[data_sep$year %in% "2024", ]

    #bronx 
    data_bronx <- data_2024[data_2024$boro %in% "Bronx", ]
    ggplot(data = data_brooklyn, aes(x = zipcode, fill = grade)) + geom_bar() + labs(title = "distribution of scores per zipcode in Brooklyn in 2024", x = "zipcodes", y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))

    #brooklyn
    data_brooklyn <- data_2024[data_2024$boro %in% "Brooklyn", ]
    ggplot(data = data_brooklyn, aes(x = zipcode, fill = grade)) + geom_bar() + labs(title = "distribution of scores per zipcode in Brooklyn in 2024", x = "Cuisines", y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))

    #manhattan
    data_manhattan <- data_2024[data_2024$boro %in% "Manhattan", ]
    ggplot(data = data_manhattan, aes(x = zipcode, fill = grade)) + geom_bar() + labs(title = "distribution of scores per zipcode in Manhattan in 2024", x = "Cuisines", y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))

    #queens
    data_queens <- data_2024[data_2024$boro %in% "Queens", ]
    ggplot(data = data_queens, aes(x = zipcode, fill = grade)) + geom_bar() +labs(title = "distribution of scores per zipcode in Queens in 2024", x = "Cuisines", y = "Count", fill = "Grades")+ guides(x = guide_axis(angle = 90))

    #staten island
    data_staten <- data_2024[data_2024$boro %in% "Staten Island", ]
    ggplot(data = data_staten, aes(x = zipcode, fill = grade)) + geom_bar() +  labs(title = "distribution of scores per zipcode in Staten Island in 2024",  x = "Cuisines", y = "Count", fill = "Grades") + guides(x = guide_axis(angle = 90))

  
    #distribution of scores by Zipcode in each borough in 2024
    #in 2024
    data_2024 <- data_sep[data_sep$year %in% "2024", ]
    
    #bronx 
    data_bronx <- data_2024[data_2024$boro %in% "Bronx", ]
    ggplot(data = data_bronx, aes(x = score, fill = zipcode)) + geom_histogram(bins = 100) + labs(title = "distribution of scores per Zipcode in the Bronx in 2024", x = "Scores", y = "Count", fill = "Zipcodes")
    
    #brooklyn
    data_brooklyn <- data_2024[data_2024$boro %in% "Brooklyn", ]
    ggplot(data = data_brooklyn, aes(x = score, fill = zipcode)) + geom_histogram(bins = 100) + labs(title = "distribution of scores per Zipcode in Brooklyn in 2024", x = "Scores", y = "Count", fill = "Zipcodes")
    
    #manhattan
    data_manhattan <- data_2024[data_2024$boro %in% "Manhattan", ]
    ggplot(data = data_manhattan, aes(x = score, fill = zipcode)) + geom_histogram(bins = 100) + labs(title = "distribution of scores per Zipcode in Manhattan in 2024", x = "Scores", y = "Count", fill = "Zipcodes")
    
    #queens
    data_queens <- data_2024[data_2024$boro %in% "Queens", ]
    ggplot(data = data_queens, aes(x = score, fill = zipcode)) + geom_histogram(bins = 100) + labs(title = "distribution of scores per Zipcode in Queens in 2024", x = "Scores", y = "Count", fill = "Zipcodes")
    
    #staten island
    data_staten <- data_2024[data_2024$boro %in% "Staten Island", ]
    ggplot(data = data_staten, aes(x = score, fill = zipcode)) + geom_histogram(bins = 100) + labs(title = "distribution of scores per Zipcode in Statan Island in 2024", x = "Scores", y = "Count", fill = "Zipcodes")
    
    
    #distribution of the grades per year
  ggplot(data = data_sep, aes(x = year, fill = grade)) + geom_bar()

  #distribution of inspection scores 
    #per year
  ggplot(data = data_sep, aes(x = score, fill = year)) + geom_histogram(bins = 100) + labs(title = "distribution of scores per year", x = "Scores", y = "Count", fill = "Years")


