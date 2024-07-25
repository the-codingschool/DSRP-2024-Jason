library(janitor)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

#CLEAN COLUMNS
  #read in dataset####
df <- read_csv("/Users/rohinv/Downloads/DSRP-2024-Jason/data/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv.zip")

  #clean column names of the dataset####
names(df)

#change column case to snakecase
df_cleanCol <- df |>
 clean_names(case = "snake")



#PREPARE DATA FOR VISUALIZATION
  #filter data for only 2023####
      #seperate the inspection_date column into day, month, and year to filter based on date
df_2023 <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection == 2023)

#VISUALIZE DATA
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
  


#HOW MUCH DOES THE GRADING IN DIFFERENT BOROUGHS DIFFER BY?

#PIE CHARTS COMPARING GRADE PROPORTIONS IN NYC BOROUGHS

#PIE CHARTS
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



#CHECKING FOR ANOMALOUS ENTRIES

#Checking whether a restaurant is graded AND has a grade value that is N/A, and vice versa####
df_graded_irregularly <- df_cleanCol|>
  filter(!is.na(grade), inspection_date == "01/01/1900")

df_graded_irregularly2 <- df_cleanCol |>
  filter(is.na(grade), inspection_date != "01/01/1900")

df_graded_irregularly2
df_graded_irregularly



#HOW HAVE THE NUMBER OF VIOLATIONS PER RESTAURANT CHANGED OVER THE LAST FEW YEARS?

#DISCOVERING IRREGULARITIES IN DATA AMOUNT VS. TIME DISTRIBUTION
#average number of violation counts per in New York vs. year####
  #filter NA placeholders from both variables and arrange dataframe by year of inspection
df_byYear <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection != 1900, !is.na(score), score != "NA")|>
  arrange(year_of_inspection)

  #create a table with columns year_of_inspection and mean number of violations per restaurant
df_year_vs_violations <- summarise(group_by(df_byYear, year_of_inspection), mean(score))
  
  #create scatter plot
df_year_vs_violations|>
  ggplot(aes(year_of_inspection, `mean(score)`)) + geom_line() + geom_point()+
  labs(title = "Mean Number of Violations per Restaurant", x = "Year of Inspection", y = "Mean Number of violations per restaurant")



#Bar Chart showing the amount of inspections logged over the years####
plot_byYear <- df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  select(!c("day","month"))|>
  filter(year_of_inspection != "1900")|>
  ggplot(aes(year_of_inspection)) + geom_bar() + labs(title = "Year of Inspection vs. Count", x = "Year of Inspection", y = "Count")

plot_byYear






#FINDING CORRELATION BETWEEN LOATION AND TYPE OF VIOLATION
#map of new york restaurants####
#plot data for a specific year and violation code type
df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(latitude != 0, longitude != 0, year_of_inspection == 2023, violation_code == "04L", boro == "Staten Island")|>
  ggplot(aes(x = longitude, y = latitude, color = boro)) + geom_point()

#How many restaurants fit that criteria
df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(latitude != 0, longitude != 0, year_of_inspection == 2023, violation_code == "04K", boro == "Manhattan")|>
  nrow()

#Average number of violations per restaurant for each zipcode in a borough####
df_cleanCol |>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(latitude != 0, longitude != 0, year_of_inspection == 2023, violation_code == "04L")|>
  group_by(zipcode) |>
  summarise(mean_score = mean(score, na.rm = TRUE))|>
  ggplot(aes(x = factor(zipcode), y = mean_score)) + geom_histogram(stat = "identity")

  # On the map: restaurants in 2023 whose inspection scores are above 60
df_cleanCol|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(latitude != 0, longitude != 0, year_of_inspection == 2023, boro == "Manhattan", score >= 60)|>
  ggplot(aes(x = longitude, y = latitude, color = boro)) + geom_point()






#Replace NA Placeholders with NA itself####
df_clean <- df_cleanCol|>
  mutate(inspection_date = ifelse(inspection_date == "01/01/1900", NA, inspection_date))|>
  filter(!is.na(inspection_date))|>
  separate(inspection_date, into=c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection >= 2022)|>
  select(!c("community_board", "council_district", "census_tract", "bin", "bbl", "nta", "location_point1", "phone"))|>
  mutate(longitude = ifelse(longitude == 0, NA, longitude))|>
  mutate(latitude = ifelse(latitude == 0, NA, latitude))|>
  mutate(action = ifelse(action == "No violations were recorded at the time of this inspection.", NA, action))

df_clean
