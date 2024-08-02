#Mean number of vermin violations in every Manhattan zipcode in 2023####

#04K: rats
#04L: mice
#04M: roaches
#04N: Flies

Code <- "04M"

#Create a function to calculate the mode of a categorical column
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

#Summarise the data by most common cuisine in each zipcode
df_zipcode_cuisine <- df_merged|>
  filter(boro == "manhattan",  violation_code == Code)|>
  group_by(zipcode)|>
  summarise(zipcode_cuisine = Mode(cuisine_description))

df_merged <- left_join(df_merged, df_zipcode_cuisine, by = "zipcode")

#Plot the data; most common restaurant with violation by zipcode
b <- df_merged |>
  filter(boro == "manhattan", vermin_restaurants != 0, percent_vermin < 20) |>
  ggplot(aes(x = longitude, y = latitude, color = zipcode_cuisine)) +
  geom_point() +
  labs(title = "  Percent of Restaurants in Manhattan with Rat Infestation by Zipcode", x = "Longitude", y = "Latitude", color = "% Restaurants with Rat Infestation") +
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

#Plot the data; percent restaurants that have pest violations in the zipcode
a <- df_merged |>
  filter(boro == "manhattan", vermin_restaurants != 0, percent_vermin < 20) |>
  ggplot(aes(x = longitude, y = latitude, color = percent_vermin)) +
  geom_point() +
  #gghighlight(zipcode_cuisine == "coffee/tea") +
  scale_color_gradient(low = "lightblue", high = "black") +
  labs(title = "  Percent of Restaurants in Manhattan with Rat Infestation by Zipcode", x = "Longitude", y = "Latitude", color = "% Restaurants with Rat Infestation") +
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

b
a
