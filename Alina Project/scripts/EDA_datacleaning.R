library(titanic)
library(dplyr)
library(ggplot2)
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
remove_empty(data_clean)
na.omit(data_clean)


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
library(tidyr)
data_sep <- separate(data_clean, col=inspection_date, into=c("day", "month", "year"), sep = "/")
arrange(data_sep, year)

#ggplot/histograms/box plots/ bar graphs/ pie charts/ scatterplots/ line graphs from data
  # distributions of scores overall 

ggplot(data = data_clean, aes(score)) + geom_histogram(bins = 70)+
  labs(title = "distribution of scores of restaurants overall", x = "score",y = "count")

  # restaurants per borough overall
  
ggplot(data_sep, aes(x = boro)) + geom_bar() + labs(title = "number or restaurants per borough", x = "boroughs",y = "count")

   
  # Distribution of the grades
pie(table(data_sep$grade),
    main = "Distribution of grades",
    labels = paste(names(table(data_sep$grade)), "\n", table(data_sep$grade), sep = ""))

  #distribution of the grades per year
ggplot(data = data_sep, aes(x = year, fill = grade)) +
  geom_bar()

  # distribution of the grades per borough 
ggplot(data = data_sep, aes(x = boro, fill = grade)) +
  geom_bar()

  #distribution of inspection scores 
    #per year
ggplot(data = data_sep, aes(x = score, fill = year)) +
  geom_histogram(bins = 100) +
  labs(title = "distribution of scores per year",
       x = "Scores",
       y = "Count",
       fill = "Years")
    # per borough
ggplot(data = data_sep, aes(x = score, fill = boro)) +
  geom_histogram(bins = 100) +
  labs(title = "distribution of scores per boro",
       x = "Scores",
       y = "Count",
       fill = "Boroughs")
