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

Code <- "04N"

#create two dataframes with number of restaurants and number of infested restaurants by zipcode
df_total_restaurants <- df_clean |>
  group_by(zipcode) |>
  summarise(total_restaurants = n())

df_vermin <- df_clean |>
  filter(violation_code == Code) |>
  group_by(zipcode) |>
  summarise(vermin_restaurants = n())

#use the total number of restaurants in the zipcode and the vermin infested restarurants and find the percent of restaurants in the zipcode infested
df_merged <- left_join(df_vermin, df_total_restaurants, by = "zipcode")
df_merged <- df_merged |>
  mutate(percent_vermin = (vermin_restaurants / total_restaurants) * 100)
df_merged <- left_join(df_clean, df_merged, by = "zipcode")

#Plot the data
df_merged |>
  filter(boro == "manhattan", vermin_restaurants != 0) |>
  ggplot(aes(x = longitude, y = latitude, color = percent_vermin)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "orange") +
  labs(title = "  Percent of Restaurants in Manhattan with Rat Infestation by Zipcode", x = "Longitude", y = "Latitude", color = "Percentage of Restaurants") +
  theme_classic() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

#Perceent of restaurants in different cuisines with a vermin violation in 2023####
  #04K: rats
  #04L: mice
  #04M: roaches
  #04N: Flies

Code1 <- "05A"

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
