#Mean Score per Inspection in every Manhattan Zipcode in 2023####

df_zipcodeScore <- df_clean |> 
  filter(boro == "manhattan") |> 
  group_by(zipcode) |> 
  summarise(n = mean(score))

df_zipcodeScoreVisualization <- left_join(df_clean, df_zipcodeScore, by = "zipcode")

df_zipcodeScoreVisualization |>
  filter(boro == "manhattan")|>
  ggplot(aes(x = longitude, y = latitude, color = n)) + geom_point() + labs(title = "Mean Score per Inspection in every Manhattan Zipcode in 2023", x = "Longitude", y = "Latitude")





#Perceent of restaurants in different cuisines with a vermin violation in 2023####
  #04K: rats
  #04L: mice
  #04M: roaches
  #04N: Flies

Code1 <- "04L"

#Compute the count by cuisine
df_count_by_cuisine <- df_clean |>
  group_by(cuisine_description) |>
  summarise(count_by_cuisine = n())

#Filter the data for Manhattan and the specific violation code
df_filtered <- df_clean |> 
  filter(boro == "manhattan", cuisine_description != "not listed/not applicable", violation_code == Code1)

#Join the filtered data with the count_by_cuisine
df_joined <- left_join(df_filtered, df_count_by_cuisine, by = "cuisine_description")

#Summarise to calculate the ratio
df_VerminByCuisine <- df_joined |> 
  group_by(cuisine_description) |> 
  summarise(n = n() / first(count_by_cuisine))

df_VerminByCuisine <- df_VerminByCuisine |> 
  mutate(cuisine_description = fct_reorder(cuisine_description, n))

#Filter and plot the data
df_VerminByCuisine |>
  filter()|>
  ggplot(aes(x = cuisine_description, y = n, fill = cuisine_description)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Cuisine", y = "Instances of Rat Violations in 2023", title = "Percent of Restaurants with Roach Violations in 2023 by Cuisine", color = "Cuisine") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Visualization with cuisine description in it####

df_merged2 <- left_join(df_merged, df_zipcode_cuisine, by = "zipcode")

df_merged2
