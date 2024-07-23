library(janitor)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
getwd()

# read in dataset####
df <- read_csv("/Users/rohinv/Downloads/DSRP-2024-Jason/data/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv.zip")

#clean column names of the dataset####
names(df)

#change column case to snakecase####
df_cleanCol <- df |>
 clean_names(case = "snake")


#filter data for only 2023####
df_2023 <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection == 2023)
df_2023

#visualizing restaurant abundance per borough in 2023####
df_2023|>
  ggplot(aes(boro, fill = grade)) + geom_bar() + 
  labs(x = "Borough", y = "Number of Restaurants", title = "Number of restaurants in each New York Borough in 2023")
#Lots of NA values

#plot without the NA values
df_2023|>
  filter(!is.na(grade))|>
  ggplot(aes(boro, fill = grade)) + geom_bar() + 
  labs(x = "Borough", y = "Number of Restaurants", title = "Number of restaurants in each New York Borough in 2023")
  

#Pie Chart for proportion of valid grades in New York in 2023####
df_ny <- df_2023 |>
  filter(!is.na(grade))

grade_counts_ny <- df_ny |>
  group_by(grade) |>
  summarise(count = n())
counts <- grade_counts_ny$count
grade <- grade_counts_ny$grade

pie(counts, main = "Grade of Restaurants in New York City", labels = grade)
#Manhattan####
df_manhattan <- df_2023 |>
  filter(boro == "Manhattan", !is.na(grade))

grade_counts_manhattan <- df_manhattan |>
  group_by(grade) |>
  summarise(count = n())
counts <- grade_counts_manhattan$count
grade <- grade_counts_manhattan$grade

pie(counts, main = "Grade of Restaurants in Manhattan", labels = grade)

#Brooklyn ####
df_brooklyn <- df_2023 |>
  filter(boro == "Brooklyn", !is.na(grade))

grade_counts_brooklyn <- df_brooklyn |>
  group_by(grade) |>
  summarise(count = n())
counts <- grade_counts_brooklyn$count
grade <- grade_counts_brooklyn$grade

pie(counts, main = "Grade of Restaurants in Brooklyn", labels = grade)

#Bronx####
df_bronx <- df_cleanCol |>
  filter(boro == "Bronx", !is.na(grade))

grade_counts_bronx <- df_bronx |>
  group_by(grade) |>
  summarise(count = n())
counts <- grade_counts_bronx$count
grade <- grade_counts_bronx$grade

pie(counts, main = "Grade of Restaurants in The Bronx", labels = grade)
#Queens####
df_Q <- df_cleanCol |>
  filter(boro == "Queens", !is.na(grade))

grade_counts_Q <- df_Q |>
  group_by(grade) |>
  summarise(count = n())
counts <- grade_counts_Q$count
grade <- grade_counts_Q$grade

pie(counts, main = "Grade of Restaurants in Queens", labels = grade)

#Staten Island####
df_S <- df_cleanCol |>
  filter(boro == "Staten Island", !is.na(grade))

grade_counts_S <- df_S |>
  group_by(grade) |>
  summarise(count = n())
counts <- grade_counts_S$count
grade <- grade_counts_S$grade

pie(counts, main = "Grade of Restaurants in Staten Island", labels = grade)


#Checking whether a restaurant is graded and has a value that is N/A and vice versa####
df_graded_irregularly <- df_cleanCol|>
  filter(!is.na(grade), inspection_date == "01/01/1900")

df_graded_irregularly2 <- df_cleanCol |>
  filter(is.na(grade), inspection_date != "01/01/1900")

df_graded_irregularly2
df_graded_irregularly


#total number of violation counts per in New York vs. year####

df_byYear <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection != 1900)|>
  arrange(year_of_inspection)

df_byYear|>
  summarise(sum(score))
  