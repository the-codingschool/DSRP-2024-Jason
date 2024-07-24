library(janitor)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

year = 2016

#Read in dataframe)
df <- read_csv("/Users/rohinv/Downloads/DSRP-2024-Jason/data/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv.zip")

#fix case discrepencies in column name
df_cleanCol <- df |>
  clean_names(case = "snake")

#filter rows based on year
df_2023 <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection == year)

#Create pie chart based on
df_ny <- df_2023 |>
  filter(!is.na(grade))

grade_counts_ny <- df_ny |>
  group_by(grade) |>
  summarise(count = n())
counts <- grade_counts_ny$count
grade <- grade_counts_ny$grade

title <- paste("Grades of Restaurants in New York City in", year)
pie(counts, main = title , labels = grade)


#Scatter plot showing average number of violations per restaurant over the last few years####
df_byYear <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection != 1900, !is.na(score), score != "NA")|>
  arrange(year_of_inspection)

#create a table with columns year_of_inspection and mean number of violations per restaurant
df_year_vs_violations <- summarise(group_by(df_byYear, year_of_inspection), mean(score))

#create scatter plot
df_year_vs_violations|>
  ggplot(aes(year_of_inspection, `mean(score)`)) + geom_point()



#Bar Chart showing the amount of inspections logged over the years####
plot_byYear <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  select(!c("day","month"))|>
  ggplot(aes(year_of_inspection, fill = factor(boro))) + geom_bar() + labs(title = "Year of Inspection vs. Count", x = "Year of Inspection", y = "Count")

plot_byYear



