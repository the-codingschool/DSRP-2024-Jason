library(janitor)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

df <- read_csv("/Users/rohinv/Downloads/DSRP-2024-Jason/data/DOHMH_New_York_City_Restaurant_Inspection_Results_20240721.csv.zip")

df_clean <- df|>
  clean_names(case = "snake")|>
  mutate(inspection_date = ifelse(inspection_date == "01/01/1900", NA, inspection_date))|>
  filter(!is.na(inspection_date))|>
  filter(!is.na(grade), grade != "N")|>
  separate(inspection_date, into = c("day", "month", "year_of_inspection"), sep = "/")|>
  filter(year_of_inspection == 2023)

#remove the unneccesary columns
df_clean <- df_clean |>
  select(!c("community_board", "council_district", "census_tract", "bin", "bbl", "nta", "location_point1", "phone", "grade_date"))

#Convert NA placeholder values to NA
df_clean <- df_clean |>
  mutate(longitude = ifelse(longitude == 0, NA, longitude))|>
  mutate(latitude = ifelse(latitude == 0, NA, latitude))|>
  mutate(action = ifelse(action == "No violations were recorded at the time of this inspection.", NA, action))|>
  filter(boro != 0)

#Change the case of text columns
df_clean<- df_clean |>
  mutate(dba = toupper(dba))|>
  mutate(boro = tolower(boro))|>
  mutate(cuisine_description = tolower(cuisine_description))

df_clean