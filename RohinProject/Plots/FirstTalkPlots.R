#Mean Score per Inspection in every Manhattan Zipcode in 2023####

df_zipcodeScore <- df_clean |> 
  filter(boro == "manhattan") |> 
  group_by(zipcode) |> 
  summarise(n = mean(score))

df_zipcodeScoreVisualization <- left_join(df_clean, df_zipcodeScore, by = "zipcode")

df_zipcodeScoreVisualization |>
  filter(boro == "manhattan")|>
  ggplot(aes(x = longitude, y = latitude, color = n)) + geom_point() + labs(title = "Mean Score per Inspection in every Manhattan Zipcode in 2023", x = "Longitude", y = "Latitude")





#Mean number of vermin violations in every Manhattan zipcode in 2023####

#04K: rats
#04L: mice
#04M: roaches
#04N: Flies

Code = "04K"

#Summarize the data based on zip code and number of 04_ violations
df_vermin <- df_clean|>
  filter(violation_code == Code)|>
  group_by(zipcode)|>
  summarise(n = n())

#Join the Number of 04_ violations to the original data
df_VerminVisualization <- left_join(df_clean, df_vermin, by = "zipcode")

#Plot the data
df_VerminVisualization|>
  filter(boro == "manhattan")|>
  ggplot(aes(x = longitude, y = latitude, color = n)) + geom_point() + labs(title = "Number of Restaurants in Manhattan with a Rat Violation by Zipcode", x = "Longitude", y = "Latitude")


#Perceent of restaurants in different cuisines with a vermin violation in 2023####
  #04K: rats
  #04L: mice
  #04M: roaches
  #04N: Flies

Code1 <- "04K"

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
  labs(x = "Cuisine", y = "Instances of Rat Violations in 2023", title = "Percent of Restaurants with Rat Violations in 2023 by Cuisine") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
